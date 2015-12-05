(ns synth.instrument)

(defprotocol Instrument
  (play [self note])
  (stop [self note])
  (set-prop [self prop val]))
