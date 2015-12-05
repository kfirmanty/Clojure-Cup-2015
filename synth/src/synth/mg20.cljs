(ns synth.mg20
  (:require [synth.audio :as a]
            [synth.instrument :as i]))

(defrecord MG20 [ctx osc out]
  a/Node
  (connect [self somewhere] (.connect out somewhere) self)

  i/Instrument
  (play [_ note]
    (println "play" note)
                                        ;(aset osc "frequency" "value" (a/midi->hz note))
    (a/setv (:main-pitch osc) (a/midi->hz note))
    (aset out "gain" "value" 0.5)
    )

  (stop [_ note]
    (println "stop" note)
    (aset out "gain" "value" 0))

  (set-prop [_ prop val]
    (println "boop" prop val)))

(defn oscillators [ctx unit]
  (let [main-pitch (a/knob ctx unit 0 20000)
        main-tune  (a/knob ctx unit -100 100)
        pitch      (a/+- ctx (:out main-pitch) (:out main-tune))
        osc1       (a/osc ctx :sawtooth 0)
        osc2       (a/osc ctx :sawtooth 0)
        osc2-detune (a/knob ctx unit -10 10)
        osc1-gain  (a/knob ctx unit 0 1)
        osc2-gain  (a/knob ctx unit 0 1)
        out        (a/mix- ctx (:out osc1-gain) (:out osc2-gain))]

    (a/setv main-tune 0)
    (a/setv osc2-detune 0.2)
    (a/wire pitch (.-frequency osc1))
    (a/wire pitch (.-frequency osc2))
    (a/wire (:out osc2-detune) (.-frequency osc2))
    (a/wire osc1 (:out osc1-gain))
    (a/wire osc2 (:out osc2-gain))

    {:main-pitch  main-pitch
     :main-tune   main-tune
     :osc2-detune osc2-detune
     :osc1-gain   osc1-gain
     :osc2-gain   osc2-gain
     :out         out}))


(defn mg20 [ctx]
  (let [unit (a/constant ctx 1)
        oscs (oscillators ctx unit)
        out (a/gain ctx 0)]
    (a/setv (:osc1-gain oscs) 1)
    (a/setv (:osc2-gain oscs) 1)
    (a/wire (:out oscs) out)
    (MG20. ctx oscs out)))
