(ns synth.mg20
  (:require [synth.audio :as a]
            [synth.instrument :as i]))

(defrecord MG20 [ctx osc envs out]
  a/Node
  (connect [self somewhere] (.connect out somewhere) self)

  i/Instrument
  (play [_ note]
   ; (println "play" note)
                                        ;(aset osc "frequency" "value" (a/midi->hz note))
    (a/setv (:osc1-pitch osc) (a/midi->hz note (a/current (:osc1-oct osc))))
    (a/setv (:osc2-pitch osc) (a/midi->hz note (a/current (:osc2-oct osc))))
                                        ;(aset out "gain" "value" 0.5)
    (a/trigger (:out envs))
    )

  (stop [_ note]
   ; (println "stop" note)
    ;(aset out "gain" "value" 0)
    (a/detrigger (:out envs))
    )

  (set-prop [_ prop val]
    (println "boop" prop val)))

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
        osc2-detune (a/knob ctx unit -10 10)
        osc1-gain  (a/knob ctx unit 0 1)
        osc2-gain  (a/knob ctx unit 0 1)
        out        (a/mix- ctx (:out osc1-gain) (:out osc2-gain))]

    (a/setv main-tune 0)
    (a/setv osc2-detune 0.1)
    (a/setv osc1-gain 1)
    (a/setv osc2-gain 1)

    (a/setv osc2-oct 0)
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
     :main-tune   main-tune
     :osc2-detune osc2-detune
     :osc1-gain   osc1-gain
     :osc2-gain   osc2-gain
     :out         out}))

(defn filters [ctx unit]
  (let [lowpass (a/afilter ctx :lowpass 0)
        lp-cutoff (a/knob ctx unit 0 20000)
        lp-reso   (a/knob ctx unit 0 100)
        out       (a/gain ctx 1)]

    (a/setv lp-cutoff 1000)
    (a/setv lp-reso   1)

    (a/wire (:out lp-cutoff) (.-frequency lowpass))
    (a/wire (:out lp-reso)   (.-Q lowpass))
    (a/wire lowpass out)

    {:lowpass lowpass
     :out     out}))

(defn envelope [ctx unit]
  (let [ak   (a/knob ctx unit 0 10)
        dk   (a/knob ctx unit 0 10)
        sk   (a/knob ctx unit 0 1)
        rk   (a/knob ctx unit 0 10)
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
        out (a/gain ctx 0)]
    (a/wire (:out oscs) (:lowpass filts))
    (a/wire (:out filts) out)
    (a/connect (:out env1) (.-gain out))
    (MG20. ctx oscs env1 out)))
