(ns synth.core
  (:require [reagent.core :as reagent :refer [atom]]
            [synth.audio :as audio]
            [synth.instrument :as i]
            [synth.mg20 :as syn]
            [synth.sequencer :as s]
            [synth.scales :as scales]
            [ajax.core :as ajax]))

(enable-console-print!)



(defonce ctx (audio/audio-context))
(defonce s (audio/connect  (syn/mg20 ctx) (.-destination ctx)))
(defonce s2 (audio/connect  (syn/mg20 ctx) (.-destination ctx)))
(defonce mon (let [m (audio/monitor ctx)]
               (audio/connect s (:input m))
               m))

(defonce sequencers [(s/sequencer s (atom [])) (s/sequencer s2 (atom []))])

(defn update-steps [sequencers raw-msg]
  (let [msg (cljs.reader/read-string raw-msg)]
    (loop [s sequencers steps msg]
      (when (and (some? (first s)) (some? (first steps)))
        (reset! (:steps (first s)) (first steps))
        (recur (rest s) (rest steps))))))

(defn gen-steps [len]
  (into [] (for [i (range len)]
               {:note-on true
                :pitch (scales/random-weighted :phrygian-dominant)
                :num i})))

(defn init-sequencers [sequencers]
  (let [hash (clojure.string/replace js/window.location.hash #"#" "")
        db-res (when hash (ajax/GET (str "/db/" hash)
                                    {:handler #(update-steps sequencers %)
                                     :error-handler (fn [msg]
                                                      (doseq [s sequencers]
                                                        (reset! (:steps s) (gen-steps 16))))}))]))

(defonce clock (s/clock sequencers (* 4 100)))

(defn step-button [step sequencer]
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

(defn taper-log [x] (* x x))

(defn svg-knob [title x y kb notches opts]
  (let [val (audio/current kb)
        min (:min kb)
        max (:max kb)
        steps (calc-knob-steps notches min max)
        taper (or (:taper opts) identity)
        s (atom {:val (range->unit val min max)
                 :deg (-> val (range->unit min max) unit->deg)
                 :mul (- max min)
                 :steps steps})]
    (fn []
      [:g.sknob {:transform (str "translate(" x "," y ")")}
       (for [st steps]
          ^{:key st} [:line {:x1 0 :y1 -17
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
                                                     (println title (:val z) min max)
                                                     (audio/setv kb (+ min
                                                                       (* (taper (:val z))
                                                                          (:mul z))))))
                                      ))
                             (swap! mouse-listeners
                                    conj
                                    (knob-rotate s x y
                                                 (when-not (:hard opts)
                                                   (fn [z]

                                                     (audio/setv kb (+ min
                                                                       (* (taper (:val z))
                                                                          (:mul z)))))))
                                    )
                             )}

        [:circle {:r 15 :cx 0 :cy 0 }]
        [:rect {:x -1 :y -12 :width 3 :height 5 :style {:fill "#fff"}}]]
        [:text {:x 0 :y 30 :text-anchor :middle} title]
        ])))

(defn svg-synth-box [s name]
  [:svg {:width 440 :height 230

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
   [svg-knob "VOLUME" 40 110 (-> s :master-vol) nil {:taper taper-log}]


   [:rect.group {:x 80 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 85 :y 25 } "OSC 1"]
   [svg-knob "WAVE" 110 50 (-> s :osc :osc1-vf) [0 1 2 3] {:hard true}]
   [svg-knob "OCTAVE" 110 170 (-> s :osc :osc1-oct) [-3 -2 -1 0 1 2 3] {:hard true}]

   [:rect.group {:x 150 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 155 :y 25 } "OSC 2"]
   [svg-knob "WAVE" 180 50 (-> s :osc :osc2-vf) [0 1 2 3] {:hard true}]
   [svg-knob "DETUNE" 180 110 (-> s :osc :osc2-detune) [-10 0 10]]
   [svg-knob "OCTAVE" 180 170 (-> s :osc :osc2-oct) [-3 -2 -1 0 1 2 3] {:hard true}]

   [:rect.group {:x 220 :y 10 :rx 5 :ry 5 :width 60 :height 210}]
   [:text.gtitle {:x 225 :y 25 } "LP FILTER"]
   [svg-knob "CUTOFF" 250 50 (-> s :filt :cutoff) nil {:taper taper-log}]
   [svg-knob "RESO" 250 110 (-> s :filt :resonance) nil {:taper taper-log}]
   [svg-knob "ENV AMT" 250 170 (-> s :filt :env-amt) nil]

   [:rect.group {:x 290 :y 10 :rx 5 :ry 5 :width 130 :height 140}]
   [:text.gtitle {:x 295 :y 25 } "ENVELOPE"]
   [svg-knob "ATTACK" 320 50 (-> s :envs :a)]
   [svg-knob "DECAY" 390 50 (-> s :envs :d)]
   [svg-knob "SUSTAIN" 320 110 (-> s :envs :s)]
   [svg-knob "RELEASE" 390 110 (-> s :envs :r)]
   [:text.name {:x 300 :y 195} (str "SYNTH " name)]
   ])


(defn randomize-button [s]
  [:button {:on-click (fn [e]
                        (let [fun (if (= identity @(:transformer s)) s/randomize-step identity)]
                          (s/step-transformer s fun)))
            :class (if (not (= identity @(:transformer s))) "pressed" "depressed")}])

(defn sequencer-block [clk]
  [:div
   [:div.sequencer
       [:div.sknob
       [:span "BPM"]
       [:input {:type :range
                :min 1
                :max 200
                :step 2
                :defaultValue 100
                :on-change #(s/set-bpm clk (* 4 (js/Number.parseFloat (-> % .-target .-value))))}]]
    [:button {:on-click #(s/start clk)} "start seq"]
    [:button {:on-click #(s/stop clk)} "stop seq"]]

   (for [s sequencers]
     [:div.sequencer [randomize-button s]
      [:div.sequencer
       (for [step @(:steps s)]
         ^{:key (:num step)} [:div.sequencer (step-button step s)
                              [:input.pknob {:type :range
                                             :min 57
                                             :max 81
                                             :step 1
                                             :defaultValue (:pitch step)
                                             :on-change #(s/set-step-pitch s step (js/Number.parseFloat (-> % .-target .-value)))}]])]])
     ])


(defn seq-knob-rotate [katom x y then]
  (fn [mx my]
    (let [dx (- x mx)
          dy (- y my)
          r
          (/ (* 180 (- (js/Math.atan2 dx dy)))
             js/Math.PI)]
      (when (<= (js/Math.abs r) 130)
        (swap! katom assoc :deg r)
        (when then
          (then))))
    ))

(defn seq-knob-park [katom stepss then]
  (fn []
    (swap! katom assoc :rotating false)
    (let [
          cur   (:deg @katom)
          dif   (vec (map (juxt identity (fn [s]
                                           (js/Math.abs (- s cur)))) stepss))
          min (-> (sort-by second dif) first first)]
      ;(swap! katom assoc :deg min :val (deg->unit min))
      (when then
        (then (deg->unit min)))
      )))

(defn unit->range [u min max]
  (+ min (* (- max min) u)))

(defn seq-knob [x y se d]
  (let [val (:pitch (get @se (:num d)))
        min 60
        max (+ 23 60)
        stepss (calc-knob-steps (range min (inc max)) min max)
        rotor (atom {:deg 0 :rotating false})]

    (fn []
      [:g.sknob.white {:transform (str "translate(" x "," y ")")}
       (for [st stepss]
          ^{:key st} [:line {:x1 0 :y1 -17
                      :x2 0 :y2 -20
                      :stroke-width 1

                             :transform (str "rotate(" st ")")}])

       [:g {:transform (str "rotate(" (if (get @rotor :rotating)
                                        (get @rotor :deg)
                                        (-> @se (get (:num d)) :pitch (range->unit min max) unit->deg)) ")")
            :on-mouse-down (fn [e]
                           ;  (println "kupa")
                             (.preventDefault e)
                             (.stopPropagation e)
                             (swap! rotor assoc :rotating true)
                             (swap! up-listeners
                                    conj
                                    (seq-knob-park rotor  stepss (fn [z]
                                                                   (swap! se assoc-in [(:num d) :pitch] (unit->range z min max) )
                                                  ))
                                    )
                             (swap! mouse-listeners
                                    conj
                                    (seq-knob-rotate rotor x y
                                                nil)
                                    )
                             )}

        [:circle {:r 15 :cx 0 :cy 0 }]
        [:rect.mark {:x -1 :y -12 :width 3 :height 5}]]

       ])))

(defn seq-button [x y sq s]
  [:rect.seq-but {:class (when (:note-on s) :note) :x x :y y :width 30 :height 30 :rx 5 :ry 5 :on-click #(s/toggle-step sq s)}])

(defn seq-step [sq se s]
  (let [i (:num s)
        cur (:current s)
        note (:note-on s)]
    [:g
     [:rect.group.step-g {:x (+ 10 (* 61 i)) :y 40 :rx 5 :ry 5 :width 55 :height 100
                          :class (str (when note " note")
                                      (when (= (rem i 4) 0) " bar")
                                      (when cur " on"))}]
     [seq-button (+ 23 (* 61 i)) 53 sq s]
     [seq-knob (+ 38 (* 61 i)) 110 se s]

     ])
  )

(defn svg-seq-box [sqs sts nam]
  [:svg.seq-box {:width 1000 :height 150
                 :on-mouse-up (fn [e]
                              ;  (println "eee")
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
   [:rect.group.seq-header {:x 10 :y 10 :width 970 :height 30 :rx 5 :ry 5}]
   [:text.seq-name {:x 16 :y 32} (str "SEQ " nam)]
   (for [s @sts]
     ^{:key (str (:num s) "-step")} [seq-step sqs sts s])]
  )

(defn control-btn [title x y w h f]
  (let [active (atom false)]
    (fn []
      [:g.ctl-btn {:class (when @active :push)
                   :transform (str "translate(" x "," y ")")
                   :on-mouse-down #(reset! active true)
                   :on-mouse-up #(reset! active false)
                   :on-click f}

       [:rect {:x 0 :y 0 :width w :height h :rx 5 :ry 5}]
       [:text {:x (/ w 2) :y (* h 0.6) :text-anchor :middle} title]
       ]
      ))
  )

(defn randomize-pitch-in-seq-steps [sequencer scale-key]
  (let [steps (:steps sequencer)
        steps-pitch (for [i (range (count @steps))]
                      (scales/random-weighted scale-key))
        steps-pitch-even (scales/even-out steps-pitch scale-key)]
     (doseq [i (range (count @steps))]
       (swap! steps assoc-in [i :pitch] (nth steps-pitch-even i)))))

(defn shorten-seq [sequencer]
  (swap! (:steps sequencer) #(take (-> % count dec (max 1)) %)))

(defn expand-seq [sequencer scale-key]
  (let [steps (:steps sequencer)
        seq-len (count @steps)
        base-step (nth @steps (dec seq-len))
        new-step (assoc base-step :num (-> base-step :num inc))]
    (swap! steps #(conj % new-step))))

(defn save-db-success [msg]
  (println msg))

(defn svg-control-box []
  [:svg {:width 120 :height 230}
   [:rect.group.red {:x 10 :y 10 :rx 5 :ry 5 :width 100 :height 210}]
   [:text.gtitle.red {:x 15 :y 25 } "CONTROL"]
   ;;[control-btn "-" 22 40 30 30 #(shorten-seq (first sequencers))]
   ;;[control-btn "+" 67 40 30 30 #(expand-seq (first sequencers) :pentatonic-minor)]
   [control-btn "SAVE" 15 75 90 30
    (fn []
      (let [steps (map #(-> % :steps deref) sequencers)]
        (ajax/POST "/db"
                   {:params  {:synth steps}
                    :format :json
                    :handler save-db-success})))]
   [control-btn "START SEQ" 15 145 90 30 #(s/start clock)]
   [control-btn "STOP SEQ" 15 180 90 30 #(s/stop clock)]
   [control-btn "RANDOMIZE" 15 40 90 30 (fn [] (doseq [s sequencers]
                                                 (randomize-pitch-in-seq-steps s :pentatonic-minor))
                                           true)]

   ])

(defn update-monitor []
  (.requestAnimationFrame js/window update-monitor)
  (when-let [cv (.getElementById js/document "mont")]
    (audio/monitor-refresh mon)
    (let [tc (.getContext cv "2d")]
      (.clearRect tc 0 0 (:tsize mon) 256)
      (.beginPath tc)
      (.moveTo tc 0 128)
      (.lineTo tc 512 128)
      (.moveTo tc 0 0)
      (.lineTo tc 512 0)
      (.moveTo tc 0 256)
      (.lineTo tc 512 256)
      (.stroke tc)
       (.beginPath tc)
      (.moveTo tc 0 (aget (:tdata mon) 0))
      (dotimes [x (:tsize mon)]

        ;(.moveTo tc x 128)
        (.lineTo tc x (aget (:tdata mon) x))
        (.stroke tc)
        )))
  )

(defn monitor-ui []
  (update-monitor)
  (fn []
    [:div
     [:canvas#mont {:width (:tsize mon) :height 256}]
     [:canvas#monf {:width (:fsize mon) :height 100}]]))

(defn hello-world []
  (init-sequencers sequencers)
  [:div#wrap
   [svg-control-box]
   [svg-synth-box s "A"]
   [svg-synth-box s2 "B"]
   ;;[monitor-ui]
   (for [[sequencer nam] (map vector sequencers ["A" "B"])]
     ^{:key (str "se-view-" (rand))} [svg-seq-box sequencer (:steps sequencer) nam])
   ;[sequencer-block sequencer clock]
   ])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
