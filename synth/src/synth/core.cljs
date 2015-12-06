(ns synth.core
  (:require [reagent.core :as reagent :refer [atom]]
            [synth.audio :as audio]
            [synth.instrument :as i]
            [synth.mg20 :as syn]
            [synth.sequencer :as s]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defonce ctx (audio/audio-context))
(defonce s (audio/connect  (syn/mg20 ctx) (.-destination ctx)))

(defonce steps (atom (into [] (for [i (range 16)]
                                {:note-on true
                                 :pitch (+ 60 (* 20 (Math/random)))
                                 :num i}))))

(defonce sequencer (s/sequencer s steps))
(defonce clock (s/clock sequencer (* 4 120)))

(defn step-button [step]
  ^{:key (:num step)} [:button {:on-click #(s/toggle-step sequencer step)
                                :class (cond
                                           (:current step) "current-step"
                                           (:note-on step) "note-on"
                                           :else "note-off")}])

(defn hello-world []
  [:div [:h1 (:text @app-state)]
   [:button {:on-click #(i/play s 69)} "on"]
   [:button {:on-click #(i/stop s 69)} "off"]
   [:div
    [:button {:on-click #(s/start clock)} "start seq"]
    [:button {:on-click #(s/stop clock)} "stop seq"]
    [:button {:on-click #(s/randomize-pitch sequencer)} "randomize"]]
      [:div
    (doall (for [step @steps]
             (step-button step)))]])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
