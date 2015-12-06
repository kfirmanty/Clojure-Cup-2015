(ns synth.sequencer
  (:require [cljs.core.async :refer [put!]]
            [synth.instrument :as i]
            [reagent.core :as reagent :refer [atom]]))

(defprotocol Controlable
  (start [this])
  (stop [this])
  (main-loop [this]))

(defprotocol Stepable
  (step [this])
  (set-step [this step]))

(defn swap-step
  [steps num]
  (let [step (nth steps num)
        step-rev (assoc step :note-on (not (:note-on step)))]
    (assoc steps num step-rev)))

(defrecord Sequencer [steps events synth-chan]
  Stepable
  (step [this]
    (let [event (first (take 1 @events))]
                 (if (:note-on event)
                   (i/play synth-chan (:pitch event))
                   (i/stop synth-chan (:pitch event)))
             ;(put! synth-chan event)
                 (swap! events #(rest %))))
  (set-step [this step]
    (swap! steps #(swap-step % step))
    (swap! events #(-> @steps cycle))))

(defrecord Clock [sequencer interval running]
  Controlable
  (start [this]
    (swap! running (fn [] true))
    (main-loop this))

  (main-loop [this]
    (js/setTimeout (fn []
                      (if @running (do (step sequencer)
                                           (main-loop this)))) interval))

  (stop [this]
    (swap! running (fn [] false))))

(defn sequencer
  ([synth-chan]
   (->Sequencer (atom [{:note-on true :pitch 69}])
                (atom (cycle [{:note-on true :pitch 69} {:note-on false :pitch 69}]))
                synth-chan))
  ([synth-chan steps]
   (->Sequencer steps (atom (cycle @steps)) synth-chan)))

(defn clock [sequencer bpm]
  (->Clock sequencer (/ (* 60 1000) bpm) (atom true)))
