(ns synth.sequencer
  (:require [cljs.core.async :refer [put!]]
            [synth.instrument :as i]
            [reagent.core :as reagent :refer [atom]]
            [synth.scales :as s]))

(defprotocol Controlable
  (start [this])
  (stop [this])
  (main-loop [this])
  (set-bpm [this bpm]))

(defprotocol Stepable
  (step [this step-num])
  (toggle-step [this step])
  (set-step-pitch [this step pitch])
  (randomize-pitch [this])
  (step-transformer [this transformer]))


(defn randomize-step [step]
  (assoc step :pitch (s/random-pitch-val)))

(defn swap-step
  [step]
  (assoc step :note-on (not (:note-on step))))

(defn clear-current [steps]
  (swap! steps (fn [old-steps]
                 (vec
                  (map #(assoc % :current false) old-steps)))))

(defn set-in-step [steps num what to]
  (swap! steps assoc-in [num what] to))

(defn bpm->ms [bpm]
  (/ (* 60 1000) bpm))

(defrecord Sequencer [steps synth-chan transformer]
  Stepable
  (step [this timer]
    (let [step-num (mod timer (count @steps))
          event (-> @steps (nth step-num) (@transformer))]
      (clear-current steps)
      (set-in-step steps step-num :current true)
      (if (:note-on event)
        (i/play synth-chan (:pitch event))
        (i/stop synth-chan (:pitch event)))))

  (toggle-step [this step]
    (swap! steps assoc-in [(:num step) :note-on]
           (-> step :note-on not)))

  (set-step-pitch [this step pitch]
    (swap! steps assoc-in [(:num step) :pitch] pitch))

  (randomize-pitch [this]
    (swap! steps #(for [step %] (assoc step :pitch (s/random-pitch-val)))))
  (step-transformer [this new-transformer] (reset! transformer new-transformer)))

(defrecord Clock [sequencers interval running count]
  Controlable
  (start [this]
    (reset! running true)
    (main-loop this))

  (main-loop [this]
    (js/setTimeout (fn []
                     (if @running (do
                                    (doseq [sequencer sequencers]
                                      (step sequencer @count))
                                    (swap! count inc)
                                    (main-loop this)))) @interval))

  (stop [this]
    (reset! running false)
    (reset! count 0)
    (js/setTimeout (fn [] (doseq [sequencer sequencers]
                           (i/stop (:synth-chan sequencer) 69)))
                   (* 2 interval)))

  (set-bpm [this bpm]
    (reset! interval (bpm->ms bpm))))

(defn sequencer
  ([synth-chan]
   (->Sequencer (atom [{:note-on true :pitch 69}])
                synth-chan (atom identity)))
  ([synth-chan steps]
   (->Sequencer steps synth-chan (atom identity))))

(defn clock [sequencers bpm]
  (->Clock sequencers (atom (bpm->ms bpm)) (atom false) (atom 0)))

(defn split-tail [s]
  (let [l (last s)]
    (split-with #(not= l %) s)))

(defn euclidean [a b]
  (let [zeros (- b a)
        f-s (concat (repeat a [1]) (repeat zeros [0]))]
    (loop [[h-els t-els] (split-tail f-s)]
      (let [n-hs (map concat h-els t-els)
            n-ts (drop (count n-hs) t-els)
            o-hs (drop (count n-hs) h-els)
            n-s (concat n-hs o-hs n-ts)]
        (if (empty? n-hs)
          (flatten n-s)
          (recur (split-tail n-s)))))))

(defn euclidean-steps [steps]
  (let [num (count steps)
        turned-on (->> steps (filter :note-on) count)
        e (euclidean turned-on num)]
    (->> (interleave e steps) (partition 2) (map (fn [[t s]] (if (= 1 t) (assoc s :note-on true)
                                                      (assoc s :note-on false)))) (into []))))


(defn shorten-seq [steps]
  (into [] (take (-> steps count dec (max 1)) steps)))

(defn expand-seq [steps scale-key]
  (let [seq-len (count steps)
        copy-step (nth steps (dec seq-len))
        new-step (assoc copy-step :num (-> copy-step :num inc))]
    (into [] (conj steps new-step))))
