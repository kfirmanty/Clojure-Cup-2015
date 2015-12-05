(ns synth.sequencer
  (:require [cljs.core.async :refer [put!]]
             [synth.instrument :as i]))

(defprotocol Controlable
  (start [this])
  (stop [this])
  (main-loop [this]))

(defprotocol Stepable
  (step [this]))

(defrecord Sequencer [events synth-chan]
  Stepable
  (step [this] (let [event (first (take 1 @events))]
                 (if (:note-on event)
                   (i/play synth-chan 69)
                   (i/stop synth-chan 69))
             ;(put! synth-chan event)
             (swap! events #(rest %)))))

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
   (->Sequencer (atom (cycle [{:note-on true} {:note-on false}])) synth-chan))
  ([synth-chan events]
   (->Sequencer (atom (cycle events)) synth-chan)))

(defn clock [sequencer bpm]
  (->Clock sequencer (/ 1000 bpm) (atom true)))
