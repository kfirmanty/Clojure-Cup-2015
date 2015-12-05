(ns synth.audio)

(defn audio-context
  (let [AudioContext (or js/window.AudioContext
                         js/window.webkitAudioContext)]
    (AudioContext.)))

(defn wire [& ns]
  (reduce (fn [x y] (.connect x y) y) ns))

(defn buffer [ctx vals]
  (let [b (.createBuffer ctx 1 (count vals) (.-sampleRate ctx))
        cd (.getChannelData b 0)]
    (dotimes [i (count vals)]
      (aset cd i (get vals i)))))

(defn bufsrc [ctx buf]
  (let [bs (.createBufferSource ctx)]
    (set! (.-buffer bs buf))
    (set! (.-loop   bs true))
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

(defn constant [ctx n]
  (let [dummy (osc ctx :sine 1)
        sh    (shaper ctx [1 1])
        v     (gain ctx n)]
    (wire dummy sh v)))
