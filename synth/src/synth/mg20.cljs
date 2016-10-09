(ns synth.mg20
  (:require [synth.audio :as a]
            [synth.instrument :as i]
            [reagent.core :as reagent :refer [atom]]
            [clojure.walk :as walk]))

(defn knob? [k]
  (or
   (instance? a/Knob k)
   (instance? a/Switch k)))

(defn serialize-knob [k]
  (a/current k))

(defn serialize-group [g]
  (into {} (filter some?
                                    (map (fn [[k v]]
                                           (if (knob? v)
                                             [k (serialize-knob v)])
                                           ) g)))
  )

(defn deserialize-group! [g])

(defrecord MG20 [ctx master-vol osc filt envs out playing]
  a/Node
  (connect [self somewhere] (.connect out somewhere) self)

  i/Instrument
  (play [_ note]
   ; (println "play" note)
                                        ;(aset osc "frequency" "value" (a/midi->hz note))
    (a/setv (:osc1-pitch osc) (a/midi->hz note (a/current (:osc1-oct osc))))
    (a/setv (:osc2-pitch osc) (a/midi->hz note (a/current (:osc2-oct osc))))
                                        ;(aset out "gain" "value" 0.5)
    ;; (println "start" note)
    (reset! playing true)
    (a/trigger (:out envs))
    )

  (stop [_ note]
   ;; (println "stop" note)
                                        ;(aset out "gain" "value" 0)
    (reset! playing false)

    (a/detrigger (:out envs))
    )

  (serialize [self]
    {:osc (serialize-group osc)
     :filt (serialize-group filt)
     :envs (serialize-group envs)
     :master-vol (serialize-knob master-vol)}
    )

  (deserialize [self data]
    (doseq [[k v] (:osc data)]
      (a/setv (get osc k) v)
      )
    (doseq [[k v] (:filt data)]
      (a/setv (get filt k) v)
      )
    (doseq [[k v] (:envs data)]
      (a/setv (get envs k) v)
      )
    (a/setv master-vol (:master-vol data))
    )

  (set-prop [_ prop val]
    (println "boop" prop val)))

(defn vf-switcher [t]
  (fn [n]
    (let [vf (get [:sawtooth :square :triangle :sine] n)]
      (set! (.-type t) (name vf))
      )))

(defn oscillators [ctx unit]
  (let [osc1-pitch (a/knob ctx unit 0 20000)
        osc2-pitch (a/knob ctx unit 0 20000)
        osc1-oct   (a/knob ctx unit -3 3)
        osc2-oct   (a/knob ctx unit -3 3)
        main-tune  (a/knob ctx unit -100 100)
        pitch1      (a/+- ctx (:out osc1-pitch) (:out main-tune))
        pitch2      (a/+- ctx (:out osc2-pitch) (:out main-tune))
        osc1       (a/osc ctx :sawtooth 0)
        osc2       (a/osc ctx :sawtooth 0)
        osc1-vf    (a/switch (vf-switcher osc1) 0 3)    ; :sawtooth :triangle :square
        osc2-vf    (a/switch (vf-switcher osc2) 0 3)
        osc2-detune (a/knob ctx unit -10 10)
        osc1-gain  (a/tknob ctx unit 0 1)
        osc2-gain  (a/tknob ctx unit 0 1)
        out        (a/mix- ctx (:out osc1-gain) (:out osc2-gain))
        ]

    (a/setv main-tune 0)
    (a/setv osc2-detune 0.1)
    (a/setv osc1-gain 1)
    (a/setv osc2-gain 1)

    (a/setv osc1-oct 0)
    (a/setv osc2-oct -1)

    (a/wire pitch1 (.-frequency osc1))
    (a/wire pitch2 (.-frequency osc2))
    (a/wire (:out osc2-detune) (.-frequency osc2))
    (a/wire osc1 (:out osc1-gain))
    (a/wire osc2 (:out osc2-gain))

    {:osc1-pitch  osc1-pitch
     :osc2-pitch  osc2-pitch
     :osc1-oct    osc1-oct
     :osc2-oct    osc2-oct
     :osc1-vf     osc1-vf
     :osc2-vf     osc2-vf
     :main-tune   main-tune
     :osc2-detune osc2-detune
     :osc1-gain   osc1-gain
     :osc2-gain   osc2-gain
     :out         out}))

(defn filters [ctx unit]
  (let [lowpass (a/afilter ctx :lowpass 0)
        lp-cutoff (a/knob ctx unit 0 16000)
        env-amt   (a/tknob ctx unit 0 10000)
        lp-reso   (a/knob ctx unit 0 40)
        out       (a/gain ctx 1)]

    (a/setv lp-cutoff 1000)
    (a/setv lp-reso   1)
    (a/wire (:out env-amt)   (.-frequency lowpass))
    (a/wire (:out lp-cutoff) (.-frequency lowpass))
    (a/wire (:out lp-reso)   (.-Q lowpass))
    (a/wire lowpass out)

    {:lowpass lowpass
     :cutoff lp-cutoff
     :env-amt env-amt
     :resonance lp-reso
     :out     out}))

(defn envelope [ctx unit]
  (let [ak   (a/knob ctx unit 0 1)
        dk   (a/knob ctx unit 0 1)
        sk   (a/knob ctx unit 0 1)
        rk   (a/knob ctx unit 0 1)
        env1 (a/adsr ctx unit ak dk sk rk)]

    (a/setv sk 0.5)
    (a/setv ak 0.1)
    (a/setv dk 0.1)
    (a/setv rk 0.1)
    {:a ak :d dk :s sk :r rk :out env1}
    ))

(defn mg20 [ctx]
  (let [unit (a/constant ctx 1)
        oscs (oscillators ctx unit)
        filts (filters ctx unit)
        env1  (envelope ctx unit)
        master-vol (a/tknob ctx unit 0 0.5)

        out (a/gain ctx 0)]
    (a/setv master-vol 0.1)
    (a/wire (:out oscs) (:lowpass filts))
    (a/wire (:out filts) out)
    (a/connect (:out env1) (.-gain out))
    (a/connect (:out env1) (-> filts :env-amt :out))
    (a/wire out (:out master-vol))
    (MG20. ctx master-vol oscs filts env1 (:out master-vol) (atom false))))
