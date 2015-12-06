(ns synth.core
  (:require [reagent.core :as reagent :refer [atom]]
            [synth.audio :as audio]
            [synth.instrument :as i]
            [synth.mg20 :as syn]
            [synth.sequencer :as s]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

(defonce ctx (audio/audio-context))
(defonce s (audio/connect  (syn/mg20 ctx) (.-destination ctx)))

(defonce steps (atom (into [] (for [i (range 16)]
                                {:note-on true
                                 :pitch (s/pentatonic-pitch-val)
                                 :num i}))))

(defonce sequencer (s/sequencer s steps))
(defonce clock (s/clock sequencer (* 4 100)))

(defn step-button [step]
  [:button {:on-click #(s/toggle-step sequencer step)
                                :class (cond
                                           (:current step) "current-step"
                                           (:note-on step) "note-on"
                                           :else "note-off")}])


(defonce mouse-listeners (atom #{}))
(defonce up-listeners (atom #{}))

(defn mouse-broadcast [x y]
  (doseq [f @mouse-listeners]
    (f x y)))

(defn up-broadcast []
  (doseq [f @up-listeners]
    (f)))

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

(defn knob-park [katom then]
  (fn []
    (let [steps (:steps @katom)
          cur   (:deg @katom)
          dif   (vec (map (juxt identity (fn [s]
                                           (js/Math.abs (- s cur)))) steps))
          min (-> (sort-by second dif) first first)]
      (swap! katom assoc :deg min :val (deg->unit min))
      (when then
        (then @katom))
      )))

(defn calc-knob-steps [notches min max]
  (when notches
    (vec (map (fn [n]
                (-> n (range->unit min max) unit->deg)) notches))))


(defn svg-knob [title x y kb notches opts]
  (let [val (audio/current kb)
        min (:min kb)
        max (:max kb)
        steps (calc-knob-steps notches min max)
        s (atom {:val (range->unit val min max)
                 :deg (-> val (range->unit min max) unit->deg)
                 :mul (- max min)
                 :steps steps})]
    (fn []
      [:g.sknob {:transform (str "translate(" x "," y ")")}
       (for [st steps]
          [:line {:x1 0 :y1 -17
                  :x2 0 :y2 -20
                  :stroke-width 1

                  :transform (str "rotate(" st ")")}])
       [:g {:transform (str "rotate(" (:deg @s) ")")
            :on-mouse-down (fn [e]
                             (.preventDefault e)
                             (.stopPropagation e)
                             (when (:hard opts)
                               (swap! up-listeners
                                      conj
                                      (knob-park s (fn [z]
                                                     (audio/setv kb (+ min
                                                                       (* (:val z)
                                                                          (:mul z))))))
                                      ))
                             (swap! mouse-listeners
                                    conj
                                    (knob-rotate s x y
                                                 (when-not (:hard opts)
                                                   (fn [z]
                                                     (audio/setv kb (+ min
                                                                       (* (:val z)
                                                                          (:mul z)))))))
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
                        (up-broadcast)
                        (reset! mouse-listeners #{})
                        (reset! up-listeners #{}))

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
   [svg-knob "TUNE" 40 50 (-> s :osc :main-tune) [-100 0 100]]

   [:rect.group {:x 80 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 85 :y 25 } "OSC 1"]
   [svg-knob "OCTAVE" 110 170 (-> s :osc :osc1-oct) [-3 -2 -1 0 1 2 3] {:hard true}]

   [:rect.group {:x 150 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 155 :y 25 } "OSC 2"]
   [svg-knob "DETUNE" 180 110 (-> s :osc :osc2-detune) [-10 0 10]]
   [svg-knob "OCTAVE" 180 170 (-> s :osc :osc2-oct) [-3 -2 -1 0 1 2 3] {:hard true}]

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

(defn randomize-button [s]
  [:button {:on-click (fn [e]
                        (let [fun (if (= identity @(:transformer s)) s/randomize-step identity)]
                          (s/step-transformer s fun)))
            :class (if (not (= identity @(:transformer s))) "pressed" "depressed")}])

(defn sequencer-block [s clk]
  [:div
   [:div
    [:button {:on-click #(s/start clk)} "start seq"]
    [:button {:on-click #(s/stop clk)} "stop seq"]
    [randomize-button s]
     [:div.sknob
       [:span "BPM"]
       [:input {:type :range
                :min 1
                :max 200
                :step 2
                :defaultValue 100
                :on-change #(s/set-bpm clk (* 4 (js/Number.parseFloat (-> % .-target .-value))))}]]]
   [:div {:style {:float "left"}}
    (for [step @(:steps s)]
      ^{:key (:num step)} [:div {:style {:float "left"}} (step-button step)
       [:input.pknob {:type :range
                :min 57
                :max 81
                :step 1
                :defaultValue (:pitch step)
                      :on-change #(s/set-step-pitch s step (js/Number.parseFloat (-> % .-target .-value)))}]])]
     ])


(defn hello-world []
  [:div
   [:button {:on-click #(i/play s 69)} "on"]
   [:button {:on-click #(i/stop s 69)} "off"]

   [svg-box]
   [sequencer-block sequencer clock]])



(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
