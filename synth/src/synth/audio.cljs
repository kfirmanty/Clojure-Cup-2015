(ns synth.audio)

(defn audio-context
  (let [AudioContext (or js/window.AudioContext
                         js/window.webkitAudioContext)]
    (AudioContext.)))

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

(defn filter [ctx type freq]
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
  (let [vf (js/Float32array. (clj->js vals))
        s  (.createWaveShaper ctx)]
    (set! (.-curve s) vf)
    s))

(defn *~ [ctx a b]
  (let [g (gain ctx 0)]
    (wire a g)
    (wire b (.-gain g))
    g))

(defn +~ [ctx & rs]
  (let [g (gain ctx 1)]
    (doseq [r rs] (wire r g))
    g))

(defn mix~ [ctx & rs]
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
  (set-now [self val])
  (slide-to [self val t])
  (current [self]))

(defrecord ADSR [ctx unit a d s r out]
  Node
  (connect [self somewhere] (.connect out somewhere))

  Triggered
  (trigger [_]
    (let [t (.-currentTime ctx)]
      (doto (.-gain out)
        (.cancelScheduledValues t)
        (.setValueAtTime 0 t)
        (.linearRampToValueAtTime 1 (+ t (current a)))
        (.linearRampToValueAtTime (current s) (+ t (current a) (current d)))))
    )

  (detrigger [_]
    (let [t (.-currentTime ctx)]
      (.cancelScheduledValues (.-gain out) t)
      (.linearRampToValueAtTime (.-gain out) 0 (+ t (current r))))))

(defn adsr [ctx unit a d s r]
  (ADSR. ctx unit a d s r (wire unit
                                (gain ctx 0))))

(defn clip [val min max]
  (cond (< val min) min
        (> val max) max
        :else       val))

(defrecord Knob [ctx unit min max out]
  Node
  (connect [self somewhere] (.connect out somewhere))

  Setting
  (current [_] (-> out .-gain .-value))
  (set-now [_ val] (.setValueAtTime (.-gain out)
                                    (clip val min max)
                                    (.-currentTime ctx)))
  (slide-to [_ val t] (.linearRampToValueAtTime (.-gain out)
                                                (clip val min max)
                                                (+ (.-currentTime ctx) t)))
  )

(defn knob [ctx unit min max out]
  (Knob. ctx unit min max (wire unit
                                (gain ctx min))))

(defn midi->hz [midi]
  (* 440 (js/Math.pow 2.0 (/ (- midi 69) 12))))
