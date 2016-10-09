(ns synth.instrument)

(defprotocol Instrument
  (play [self note])
  (stop [self note])
  (serialize [self])
  (deserialize [self data])
  (set-prop [self prop val]))
