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
                                 :pitch (s/random-pitch-val)
                                 :num i}))))

(defonce sequencer (s/sequencer s steps))
(defonce clock (s/clock sequencer (* 4 120)))

(defn step-button [step]
  ^{:key (:num step)} [:button {:on-click #(s/toggle-step sequencer step)
                                :class (cond
                                           (:current step) "current-step"
                                           (:note-on step) "note-on"
                                           :else "note-off")}])
(defn knob [nam k]
   [:div.knob
       [:span nam]
       [:input {:type :range
                :min (:min k)
                :max (:max k)
                :step (/ (- (:max k) (:min k)) 100)
                :defaultValue (audio/current k)
                :on-change #(audio/setv k (js/Number.parseFloat (-> % .-target .-value)))}]])

(defn module [nam & rest]
  [:div.module
   [:span.title nam]
   (list rest)
   [:div.clearfix]])

(defn synthesizer [sy]
  [:div.box
   [:h2 "Lambda-1 Synthesizer"]

   [module "Oscillators"
    [knob "detune" (-> sy :osc :osc2-detune)]]

   [module "LP Filter"
    [knob "cutoff" (-> sy :filt :cutoff)]
    [knob "resonance" (-> sy :filt :resonance)]]

   [module "Envelope"
    [knob "A" (-> sy :envs :a)]
    [knob "D" (-> sy :envs :d)]
    [knob "S" (-> sy :envs :s)]
    [knob "R" (-> sy :envs :r)]]

   [:div.clearfix]
   ])

(defonce mouse-listeners (atom {:pos #{}}))

(defn mouse-broadcast [x y]
  (doseq [f @mouse-listeners]
    (f x y)))

(defn deg->unit [d]
  (/ (+ 130 d)
     (* 2 130)))

(defn unit->deg [u]
  (- (* 2 130 u) 130))

(defn range->unit [v min max]
  (/ (- v min)
     (- max min)))

(defn knob-rotate [katom x y then]
  (fn [mx my]
    (let [dx (- x mx)
          dy (- y my)
          r
          (/ (* 180 (- (js/Math.atan2 dx dy)))
             js/Math.PI)]
      (when (<= (js/Math.abs r) 130)
        (swap! katom assoc :deg r :val (deg->unit r))
        (when then
          (then @katom))))
    ))



(defn svg-knob [title x y kb notches]
  (let [val (audio/current kb)
        min (:min kb)
        max (:max kb)
        s (atom {:val (range->unit val min max)
                 :deg (-> val (range->unit min max) unit->deg)
                 :mul (- max min)})]
    (fn []
      [:g.sknob {:transform (str "translate(" x "," y ")")}
       [:g {:transform (str "rotate(" (:deg @s) ")")
            :onclick (fn [e] (println "kek" e))
            :on-mouse-down (fn [e] (println "kek" e)
                             (.preventDefault e)
                             (.stopPropagation e)
                             (swap! mouse-listeners
                                    conj
                                    (knob-rotate s x y
                                                 (fn [z]
                                                   (audio/setv kb (+ min
                                                                     (* (:val z)
                                                                        (:mul z))))))
                                    )
                             )}
        [:circle {:r 15 :cx 0 :cy 0 }]
        [:rect {:x -1 :y -12 :width 3 :height 5 :style {:fill "#fff"}}]]
       [:text {:x 0 :y 30 :text-anchor :middle} title]
        ])))

(defn svg-box []
  [:svg {:width 500 :height 300
         :on-mouse-up (fn [e]
                        (.preventDefault e)
                        (reset! mouse-listeners #{}))
         :on-mouse-move (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (let [br (-> e .-target .getBoundingClientRect)
                                x
                                (- (.-clientX e)
                                   (.-left br))
                                y
                                (- (.-clientY e)
                                   (.-top br))]
                            (mouse-broadcast x y)


                            ))}
   [:rect.bg {:x 0 :y 0 :rx 5 :ry 5 :width 500 :height 300}]

   [:rect.group {:x 10 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 15 :y 25 } "MASTER"]
   [svg-knob "TUNE" 40 50 (-> s :osc :main-tune)]

   [:rect.group {:x 80 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 85 :y 25 } "OSC 1"]
   [svg-knob "OCTAVE" 110 170 (-> s :osc :osc1-oct)]

   [:rect.group {:x 150 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 155 :y 25 } "OSC 2"]
   [svg-knob "DETUNE" 180 110 (-> s :osc :osc2-detune)]
   [svg-knob "OCTAVE" 180 170 (-> s :osc :osc2-oct)]

   [:rect.group {:x 220 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 225 :y 25 } "LP FILTER"]
   [svg-knob "CUTOFF" 250 50 (-> s :filt :cutoff) nil]
   [svg-knob "RESO" 250 110 (-> s :filt :resonance) nil]

   [:rect.group {:x 290 :y 10 :rx 5 :ry 5 :width 130 :height 140}]
   [:text.gtitle {:x 295 :y 25 } "ENVELOPE"]
   [svg-knob "ATTACK" 320 50 (-> s :envs :a)]
   [svg-knob "DECAY" 390 50 (-> s :envs :d)]
   [svg-knob "SUSTAIN" 320 110 (-> s :envs :s)]
   [svg-knob "RELEASE" 390 110 (-> s :envs :r)]



   ])

(defn hello-world []
  [:div [:h1 (:text @app-state)]
   [:button {:on-click #(i/play s 69)} "on"]
   [:button {:on-click #(i/stop s 69)} "off"]
   [synthesizer s]
   [svg-box]
   [:div
    [:button {:on-click #(s/start clock)} "start seq"]
    [:button {:on-click #(s/stop clock)} "stop seq"]
    [:button {:on-click #(s/step-transformer sequencer s/randomize-step)} "randomize"]]
   [:div
    (for [step @steps]
      (step-button step))]])


(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
