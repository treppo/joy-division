(ns joy-division.core
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]))

;(enable-console-print!)

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
  "half of the analyser's fftSize"
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

(defn updated-frequencies []
  (.getByteTimeDomainData analyser frequency-uint-array)
  (js/Array.prototype.slice.call frequency-uint-array))

(defn clear [context]
  (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context))))

(defn draw-lines [lines]
  (clear canvas-context)
  (doseq [points lines]
    (.beginPath canvas-context)
    (.moveTo canvas-context (:x (first points)) (:y (first points)))

    (doseq [point (rest points)]
      (.lineTo canvas-context (:x point) (:y point)))

    (.stroke canvas-context)))

(defn normalize [integer] (/ integer 256))

(defn amplify [frequency] (* 128 frequency))

(defn point [i frequency]
  (let [height (* y-step lines-count)]
    {:x (* i step), :y (- height frequency)}))

(defn x-align [{x :x, y :y}] {:x (+ x (/ step 2)), :y y})

(def point-xf (comp (map normalize) (map amplify) (map-indexed point) (map x-align)))


(defn vertical-shift [points] (map (fn [{x :x, y :y}] {:x x, :y (- y y-step)}) points))

(defn shift-lines [lines] (map vertical-shift (take (- lines-count 1) lines)))

(defn update-loop
  ([] (update-loop ()))
  ([previous]
   (let [new-line (into () point-xf (updated-frequencies))
         shifted (shift-lines previous)
         lines (conj shifted new-line)]
     (js/requestAnimationFrame (fn [] (draw-lines lines) (update-loop lines))))))

(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (update-loop)))
