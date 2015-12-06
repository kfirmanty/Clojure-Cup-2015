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

(defonce steps (into [] (for [i (range 16)]
                            {:note-on true :pitch (+ 50 (* 20 (Math/random)))})))
(defonce sequencer (s/sequencer s steps))
(defonce clock (s/clock sequencer (* 4 120)))

(defn hello-world []
  [:div [:h1 (:text @app-state)]
   [:button {:on-click #(i/play s 69)} "on"]
   [:button {:on-click #(i/stop s 69)} "off"]
   [:div
    [:button {:on-click #(s/start clock)} "start seq"]
    [:button {:on-click #(s/stop clock)} "stop seq"]]
   [:div
    (for [x (range 16)]
      [:button {:on-click (fn []
                            (s/set-step sequencer x))} "."])]])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
