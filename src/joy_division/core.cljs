(ns joy-division.core
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]))

(def resolution 64)
(def step 10)

(def audioContext (js/AudioContext.))
(def analyser (let [a (.createAnalyser audioContext)]
                (set! (.-fftSize a) resolution)
                a))
(def frequency-ints (js/Uint8Array. (.-frequencyBinCount analyser)))

(def canvas
  (let [element (js/document.querySelector "canvas")
        size (* (.-frequencyBinCount analyser) step)
        dpr js/devicePixelRatio]
    (set! (.-width element) (* size dpr))
    (set! (.-height element) (* size dpr))
    element))

(def canvas-context
  (let [ctx (.getContext canvas "2d")
        dpr js/devicePixelRatio]
    (.scale ctx dpr dpr)
    (set! (.-lineWidth ctx) 2)
    (set! (.-strokeStyle ctx) "white")
    ctx))

(defn update-frequencies! []
  (.getByteTimeDomainData analyser frequency-ints)
  (js/Array.prototype.slice.call frequency-ints))

(defn normalize [ints]
  (map (fn [i] (/ i 256)) ints))

(defn to-points [frequencies]
  (map-indexed (fn [i ratio] {:x (* i step), :y (* ratio (.-height canvas))}) frequencies))

(defn control-points [points]
  (map (fn [point next-point] {:x (/ (+ (:x point) (:x next-point)) 2), :y (/ (+ (:y point) (:y next-point)) 2)}) points (rest points)))

(defn clear-canvas []
  (.clearRect canvas-context 0 0 (.-width canvas) (.-height canvas)))

(defn draw [points]
  (let [offset (/ step 2)]
    (clear-canvas)
    (.beginPath canvas-context)
    (.moveTo canvas-context (+ offset (:x (first points))) (:y (first points)))

    (doseq [point (butlast points)
            next-point (rest points)]
      (let [x (+ offset (:x point))
            y (:y point)
            cx (/ (+ x (:x next-point)) 2)
            cy (/ (+ y (:y next-point)) 2)]
        (.quadraticCurveTo canvas-context x y cx cy)))

    (.stroke canvas-context)))

(defn update-loop []
  (-> (update-frequencies!)
      normalize
      to-points
      draw)
  ;(js/console.log (clojure.string/join ", " (normalize (update-frequencies!))))

  (js/requestAnimationFrame update-loop))

(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (update-loop)))
