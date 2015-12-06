(ns synth.sequencer
  (:require [cljs.core.async :refer [put!]]
            [synth.instrument :as i]
            [reagent.core :as reagent :refer [atom]]))

(defprotocol Controlable
  (start [this])
  (stop [this])
  (main-loop [this]))

(defprotocol Stepable
  (step [this step-num])
  (set-step [this step]))

(defn swap-step
  [step]
  (assoc step :note-on (not (:note-on step))))

(defrecord Sequencer [steps synth-chan]
  Stepable
  (step [this step-num]
    (let [event (nth @steps step-num)]
      (if (:note-on @event)
        (i/play synth-chan (:pitch @event))
        (i/stop synth-chan (:pitch @event)))
                                        ;(put! synth-chan event)
      ))
  (set-step [this step]
    (swap! step #(swap-step %))))

(defrecord Clock [sequencer interval running count]
  Controlable
  (start [this]
    (swap! running (fn [] true))
    (main-loop this))

  (main-loop [this]
    (js/setTimeout (fn []
                     (if @running (do
                                    (step sequencer @count)
                                    (swap! count #(-> % inc (mod 16)))
                                    (main-loop this)))) interval))

  (stop [this]
    (swap! running (fn [] false))))

(defn sequencer
  ([synth-chan]
   (->Sequencer (atom [{:note-on true :pitch 69}])
                synth-chan))
  ([synth-chan steps]
   (->Sequencer steps synth-chan)))

(defn clock [sequencer bpm]
  (->Clock sequencer (/ (* 60 1000) bpm) (atom true) (atom 0)))
