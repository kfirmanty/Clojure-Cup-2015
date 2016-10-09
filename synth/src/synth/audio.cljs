(ns synth.audio)

(defn audio-context []
  (let [c (or js/window.AudioContext js/window.webkitAudioContext)]
    (c.)))

(defn wire [& ms]
  (reduce (fn [x y] (.connect x y) y) ms))

(defn buffer [ctx vals]
  (let [b (.createBuffer ctx 1 (count vals) (.-sampleRate ctx))
        cd (.getChannelData b 0)]
    (dotimes [i (count vals)]
      (aset cd i (get vals i)))))

(defn bufsrc [ctx buf]
  (let [bs (.createBufferSource ctx)]
    (set! (.-buffer bs) buf)
    (set! (.-loop   bs) true)
    (.start bs 0)
    bs))

(defn noise [ctx len]
  (let [buf (buffer ctx (take (* len (.-sampleRate ctx))
                              (repeatedly #(- (* 2 (js/Math.random)) 1))))
        src (bufsrc ctx buf)]
    src))

(defn osc [ctx type freq]
  (if-not (= type :noise)
    (let [o (.createOscillator ctx)]
      (set! (.-type o) (name type))
      (aset o "frequency" "value" freq)
      (.start o)
      o)
    (noise ctx 2)))

(defn afilter [ctx type freq]
  (let [o (.createBiquadFilter ctx)]
    (set! (.-type o) (name type))
    (aset o "frequency" "value" freq)
    o))

(defn gain
  ([ctx] (gain ctx 1))
  ([ctx val]
   (let [g (.createGain ctx)]
     (aset g "gain" "value" val)
     g)))

(defn shaper [ctx vals]
  (let [vf (js/Float32Array. (clj->js vals))
        s  (.createWaveShaper ctx)]
    (set! (.-curve s) vf)
    s))

(defn *- [ctx a b]
  (let [g (gain ctx 0)]
    (wire a g)
    (wire b (.-gain g))
    g))

(defn +- [ctx & rs]
  (let [g (gain ctx 1)]
    (doseq [r rs] (wire r g))
    g))

(defn mix- [ctx & rs]
  (let [g (gain ctx (/ 1 (count rs)))]
    (doseq [r rs] (wire r g))
    g))

(defn constant [ctx n]
  (let [dummy (osc ctx :sine 1)
        sh    (shaper ctx [1 1])
        v     (gain ctx n)]
    (wire dummy sh v)))

(defprotocol Node
  (connect [self somewhere]))

(defprotocol Triggered
  (trigger [self])
  (detrigger [self]))

(defprotocol Setting
  (setv [self val])
  (slide-to [self val t])
  (current [self]))

(defrecord ADSR [ctx unit a d s r out]
  Node
  (connect [self somewhere] (.connect out somewhere) self)

  Triggered
  (trigger [_]
    ;(println "trigger" a (current d) s (current r))
    (let [t (.-currentTime ctx)]
      (doto (.-gain out)
        (.cancelScheduledValues t)
        ;(.setValueAtTime 0 t)
        (.setValueAtTime (.-value (.-gain out)) t)
        (.linearRampToValueAtTime 1 (+ t 0.005 (current a)))
        ;(.setValueAtTime (.-value (.-gain out)) (+ t (current a)))
        (.linearRampToValueAtTime (current s) (+ t 0.005 (current a) (current d)))))
    )

  (detrigger [_]
    (let [rr (current r)]
     ; (println "detrigger" (.-currentTime ctx) rr)
      (let [t (.-currentTime ctx)]
        (.cancelScheduledValues (.-gain out) t)
        (.setValueAtTime (.-gain out) (.-value (.-gain out)) t)
     ;   (println (+ t rr))
        (.linearRampToValueAtTime (.-gain out) 0 (+ 0.005 t rr))

        ))))

(defn adsr [ctx unit a d s r]
  (ADSR. ctx unit a d s r (wire unit
                                (gain ctx 0))
         ))

(defn clip [val min max]
  (cond (< val min) min
        (> val max) max
        :else       val))

(defrecord Knob [ctx unit min max out dummy]
  Node
  (connect [self somewhere] (.connect out somewhere))

  Setting
  (current [_] @dummy)
  (setv [_ val]
    (reset! dummy (clip val min max))
    (.setValueAtTime (.-gain out)
                                    (clip val min max)
                                    (.-currentTime ctx)))
  (slide-to [_ val t]
    (reset! dummy (clip val min max))
    (.linearRampToValueAtTime (.-gain out)
                                                (clip val min max)
                                                (+ (.-currentTime ctx) t)))
  )

(defn knob [ctx unit min max]
  (Knob. ctx unit min max (wire unit
                                (gain ctx min)) (atom min)))

(defn tknob [ctx unit min max]
  (Knob. ctx unit min max (gain ctx min) (atom min)))

(defrecord Switch [min max setting upfn]
  Setting
  (current [_] @setting)

  (setv [_ val]
    (reset! setting val)
    (when upfn
      (upfn val)))

  (slide-to [_ val t]
    (println "can't slide switch"))
  )

(defn switch [f min max]
  (Switch. min max (atom min) f))

(defn midi->hz
  ([midi] (midi->hz midi 0))
  ([midi oct]
   (* 440 (js/Math.pow 2.0 (/ (- (+ (* 12 oct) midi) 69) 12)))))

(defrecord Monitor [input tsize fsize fdata tdata])

(defn monitor [ctx]
  (let [sz 512
        m (.createAnalyser ctx)
        fs  (.-frequencyBinCount m)
        f (js/Uint8Array. fs)
        t (js/Uint8Array. sz)]
    (set! (.-fftSize m) sz)
    (Monitor. m sz fs f t)))

(defn monitor-refresh [m]
 ; (println (:input m))
  (.getByteTimeDomainData (:input m) (:tdata m))
  ;(.getByteFrequencyData (:input m) (:fdata m))
  )
