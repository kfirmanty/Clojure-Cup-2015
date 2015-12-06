(ns synth.sequencer
  (:require [cljs.core.async :refer [put!]]
            [synth.instrument :as i]
            [reagent.core :as reagent :refer [atom]]))

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

(defn random-pitch-val []
  (int (+ 60 (* 20 (Math/random)))))

(defn pentatonic-pitch-val []
  (let [steps [0 3 5 7 10 12]
        base 69
        step-num (int (* (Math/random) (count steps)))]
    (+ base (nth steps step-num))))

(defn randomize-step [step]
  (assoc step :pitch (pentatonic-pitch-val)))

(defn swap-step
  [step]
  (assoc step :note-on (not (:note-on step))))

(defn clear-current [steps]
  (swap! steps (fn [old-steps]
                 (vec
                  (map #(assoc % :current false) old-steps)))))

(defn set-in-step [steps num what to]
  (swap! steps assoc-in [num what] to))

(defn bpm-to-ms [bpm]
  (/ (* 60 1000) bpm))

(defrecord Sequencer [steps synth-chan transformer]
  Stepable
  (step [this step-num]
    (let [event (-> @steps (nth step-num) (@transformer))]
      (clear-current steps)
      (set-in-step steps step-num :current true)
      (if (:note-on event)
        (i/play synth-chan (:pitch event))
        (i/stop synth-chan (:pitch event)))
                                        ;(put! synth-chan event)
      ))

  (toggle-step [this step]
    (swap! steps assoc-in [(:num step) :note-on]
           (-> step :note-on not)))

  (set-step-pitch [this step pitch]
    (println pitch)
    (swap! steps assoc-in [(:num step) :pitch] pitch))

  (randomize-pitch [this]
    (swap! steps #(for [step %] (assoc step :pitch (random-pitch-val)))))
  (step-transformer [this new-transformer] (reset! transformer new-transformer)))

(defrecord Clock [sequencer interval running count]
  Controlable
  (start [this]
    (reset! running true)
    (main-loop this))

  (main-loop [this]
    (js/setTimeout (fn []
                     (if @running (do
                                    (step sequencer @count)
                                    (swap! count #(-> % inc (mod 16)))
                                    (main-loop this)))) @interval))

  (stop [this]
    (reset! running false)
    (reset! count 0))

  (set-bpm [this bpm]
    (println bpm)
    (reset! interval (bpm-to-ms bpm))))

(defn sequencer
  ([synth-chan]
   (->Sequencer (atom [{:note-on true :pitch 69}])
                synth-chan (atom identity)))
  ([synth-chan steps]
   (->Sequencer steps synth-chan (atom identity))))

(defn clock [sequencer bpm]
  (->Clock sequencer (atom (bpm-to-ms bpm)) (atom true) (atom 0)))
