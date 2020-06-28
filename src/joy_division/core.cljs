(ns joy-division.core
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]))

(enable-console-print!)

(def lines-count 32)
(def points-per-line 32)
(def resolution (* lines-count points-per-line 2))
(def y-step 10)
(def step 10)

(def audioContext (js/AudioContext.))
(def analyser (let [a (.createAnalyser audioContext)]
                (set! (.-fftSize a) resolution)
                a))

(defn frequency-bin-count [analyser]
  "half the fftSize of the analyser"
  (.-frequencyBinCount analyser))

(def canvas-dimension (* points-per-line step js/devicePixelRatio))

(def frequency-uint-array (js/Uint8Array. (frequency-bin-count analyser)))

(def canvas
  (let [element (js/document.querySelector "canvas")]
    (set! (.-width element) canvas-dimension)
    (set! (.-height element) canvas-dimension)
    element))

(def canvas-context
  (let [ctx (.getContext canvas "2d")
        dpr js/devicePixelRatio]
    (.scale ctx dpr dpr)
    (set! (.-lineWidth ctx) 2)
    (set! (.-strokeStyle ctx) "white")
    ctx))

(defn update-frequencies! []
  (.getByteTimeDomainData analyser frequency-uint-array)
  (js/Array.prototype.slice.call frequency-uint-array))

(defn normalize [ints]
  (map (fn [i] (/ i 256)) ints))

(defn amplify [frequencies]
  (map (fn [frequency] (* 128 frequency)) frequencies))

(defn to-points [line-index frequencies]
  (map-indexed (fn [i frequency] {:x (* i step), :y (- 320 frequency (* line-index y-step))}) frequencies))

(defn offset [points]
  (map (fn [{x :x, y :y}] {:x (+ x (/ step 2)), :y y}) points))

(defn clear [context]
  (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context))))

(defn midway-points [points]
  (let [midway-point (fn [point next]
                       {:x (/ (+ (:x point) (:x next)) 2)
                        :y (/ (+ (:y point) (:y next)) 2)})]
    (map midway-point points (rest points))))

(defn join-points [xs ys] (map (fn [x y] [x y]) xs ys))

(defn draw [lines]
  "Drawing smooth lines between points by calculating midway-points,
   drawing curves between those and using the points as control points"

  (clear canvas-context)
  (doseq [points lines]
    (.beginPath canvas-context)
    (.moveTo canvas-context (:x (first (midway-points points))) (:y (first (midway-points points))))

    (doseq [[point midway-point] (join-points (butlast (rest points)) (rest (midway-points points)))]
      (.quadraticCurveTo canvas-context (:x point) (:y point) (:x midway-point) (:y midway-point)))
    (.save canvas-context)
    (.fill canvas-context)
    (set! (.-globalCompositeOperation canvas-context) "destination-out")
    (.restore canvas-context)

    (.stroke canvas-context)))

(defn update-loop [previous]
  (let [freqs (take lines-count (conj previous (update-frequencies!)))]
    (->> (conj freqs)
         (map normalize)
         (map amplify)
         (map-indexed to-points)
         (map offset)
         draw)
    (js/requestAnimationFrame (fn [] (update-loop freqs)))))

(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (update-loop ())))
