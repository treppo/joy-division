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

(def max-amplitude (* (.-frequencyBinCount analyser) step))

(def canvas
  (let [element (js/document.querySelector "canvas")
        dpr js/devicePixelRatio]
    (set! (.-width element) (* max-amplitude dpr))
    (set! (.-height element) (* max-amplitude dpr))
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
  (map-indexed (fn [i ratio] {:x (* i step), :y (* ratio max-amplitude)}) frequencies))

(defn offset [points] (map (fn [{x :x, y :y}] {:x (+ x (/ step 2)), :y y}) points))

(defn clear-canvas [context]
  (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context))))

(defn midway-points [points]
  (let [midway-point (fn [point next]
                       {:x (/ (+ (:x point) (:x next)) 2)
                        :y (/ (+ (:y point) (:y next)) 2)})]
    (map midway-point points (rest points))))

(defn join-points [xs ys] (map (fn [x y] [x y]) xs ys))

(defn draw [points]
  {:desc
   "Drawing smooth lines between points by calculating midway-points,
    drawing curves between those and using the points as control points"}

  (clear-canvas canvas-context)
  (.beginPath canvas-context)
  (.moveTo canvas-context (:x (first (midway-points points))) (:y (first (midway-points points))))

  (doseq [[point midway-point] (join-points (butlast (rest points)) (rest (midway-points points)))]
    (.quadraticCurveTo canvas-context (:x point) (:y point) (:x midway-point) (:y midway-point)))

  (.stroke canvas-context))

(defn update-loop []
  (-> (update-frequencies!)
       normalize
       to-points
       offset
       draw)
  (js/requestAnimationFrame update-loop)
  )

(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (update-loop)))
