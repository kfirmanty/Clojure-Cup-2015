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
                                (atom {:note-on true
                                       :pitch (+ 50 (* 20 (Math/random)))
                                       :num i})))))

(defonce sequencer (s/sequencer s steps))
(defonce clock (s/clock sequencer (* 4 120)))

(defonce step-on "#A1DAFF")
(defonce step-off "#EEF8FF")

(defn change-color [step bckg-col]
  (reset! bckg-col (if (:note-on @step) step-on step-off)))

(defn step-button [step]
  (let [bckg-col (atom step-on)]
      ^{:key (:num @step)} [:button {:on-click (fn [click]
                                                (s/set-step sequencer step)
                                                (change-color step bckg-col))
                                    :style {:background-color @bckg-col}}]))

(defn hello-world []
  [:div [:h1 (:text @app-state)]
   [:button {:on-click #(i/play s 69)} "on"]
   [:button {:on-click #(i/stop s 69)} "off"]
   [:div
    [:button {:on-click #(s/start clock)} "start seq"]
    [:button {:on-click #(s/stop clock)} "stop seq"]]
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
