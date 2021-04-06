(ns joy-division.core
  (:require
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]))


;; (enable-console-print!)

(def lines-count 32)
(def points-per-line 32)
(def y-step 10)
(def step 8)
(def resolution (* step points-per-line 2))
(def width (* points-per-line step))
(def height (* lines-count y-step))
(def amplification 100)
(def margin 30)


(def audioContext
  (if (js-in "webkitAudioContext" js/window)
    (js/webkitAudioContext.)
    (js/AudioContext.)))


(def analyser
  (let [a (.createAnalyser audioContext)]
    (set! (.-fftSize a) resolution)
    a))


(defn frequency-bin-count
  [analyser]
  "half of the analyser's fftSize"
  (.-frequencyBinCount analyser))


(def frequency-uint-array (js/Uint8Array. (frequency-bin-count analyser)))


(def canvas
  (let [element (js/document.querySelector "canvas")]
    (set! (.-width element) (* width js/devicePixelRatio))
    (set! (.-height element) (* height js/devicePixelRatio))
    element))


(def canvas-context
  (let [ctx (.getContext canvas "2d")
        dpr js/devicePixelRatio]
    (.scale ctx dpr dpr)
    (set! (.-lineWidth ctx) 2)
    (set! (.-strokeStyle ctx) "white")
    ctx))


(defn updated-frequencies
  []
  (.getByteTimeDomainData analyser frequency-uint-array)
  (js/Array.prototype.slice.call frequency-uint-array))


(defn clear
  [context]
  (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context))))


(defn draw-lines
  [lines]
  (clear canvas-context)
  (doseq [[first-point & rest-points] lines]
    (.beginPath canvas-context)
    (.moveTo canvas-context (.-x first-point) (.-y first-point))

    (doseq [point rest-points]
      (.lineTo canvas-context (.-x point) (.-y point)))

    (.save canvas-context)
    (set! (.-globalCompositeOperation canvas-context) "destination-out")
    (.fill canvas-context)
    (.restore canvas-context)
    (.stroke canvas-context)))


(deftype Point
  [x y])


(defn normalized
  [frequency]
  (/ (Math/abs (- frequency 128)) 256))


(defn variance
  [i frequency]
  (let [x (* step i)
        distance-to-center (Math/abs (- x (/ width 2)))
        variance (/ (max (- (/ width 2) 25 distance-to-center) 0) 8)]
    (* (* amplification frequency) variance)))


(defn point
  [i frequency]
  (Point. (* step i) (- height frequency)))


(defn x-align
  [point]
  (Point. (+ (.-x point) (/ step 2)) (.-y point)))


(defn when-at-step
  [index point]
  (if (zero? (mod index step)) point))


(def point-xf (comp (keep-indexed when-at-step) (map normalized) (map-indexed variance) (map-indexed point) (map x-align)))


(defn shift-line
  [points]
  (loop [remaining points
         acc (transient [])]
    (if (empty? remaining)
      (persistent! acc)
      (recur (rest remaining)
             (conj! acc (Point. (.-x (first remaining)) (- (.-y (first remaining)) y-step)))))))


(defn shift-lines
  [lines]
  (map shift-line (take (- lines-count 1) lines)))


(defn update-loop
  ([] (js/requestAnimationFrame (fn [] (update-loop []))))
  ([previous]
   (let [new-line (into () point-xf (updated-frequencies))
         shifted (shift-lines previous)
         lines (conj shifted new-line)]
     (js/requestAnimationFrame (fn [] (draw-lines (reverse lines)) (update-loop lines))))))


(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (update-loop)))
