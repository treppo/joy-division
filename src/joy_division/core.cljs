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
(def height (+ 20 (* lines-count y-step)))
(def amplification 1)
(def margin 30)
(def line-width 2)


(defn create-audio-context
  []
  (if (js-in "webkitAudioContext" js/window)
    (js/webkitAudioContext.)
    (js/AudioContext.)))


(defn create-analyser
  [audio-context]
  (let [a (.createAnalyser audio-context)]
    (set! (.-fftSize a) resolution)
    a))


(def canvas
  (let [element (js/document.querySelector "canvas")]
    (set! (.-width element) (* width js/devicePixelRatio))
    (set! (.-height element) (* height js/devicePixelRatio))
    element))


(def canvas-context
  (let [ctx (.getContext canvas "2d")
        dpr js/devicePixelRatio]
    (.scale ctx dpr dpr)
    (set! (.-lineWidth ctx) line-width)
    (set! (.-strokeStyle ctx) "white")
    ctx))


(defn updated-frequencies
  [analyser]
  (let [frequency-array (js/Uint8Array. (.-frequencyBinCount analyser))]
    (.getByteTimeDomainData analyser frequency-array)
    (js/Array.prototype.slice.call frequency-array)))


(defn clear
  [context]
  (.clearRect context 0 0 (.-width (.-canvas context)) (.-height (.-canvas context))))


(defn draw-lines
  [lines]
  (clear canvas-context)
  (doseq [[first-point & rest-points] lines]
    (.beginPath canvas-context)
    (.moveTo canvas-context (.-x first-point) (- (.-y first-point) (/ line-width 2)))

    (doseq [point rest-points]
      (.lineTo canvas-context (.-x point) (- (.-y point) (/ line-width 2))))

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


(defn shaped
  [i frequency]
  (let [x (* step i)
        distance-to-center (Math/abs (- x (/ width 2)))
        variance (- (/ width 2) distance-to-center)
        framed (max (- variance margin) 0)]
    (* (* amplification frequency) framed)))


(defn point
  [i frequency]
  (Point. (* step i) (- height frequency)))


(defn x-align
  [point]
  (Point. (+ (.-x point) (/ step 2)) (.-y point)))


(defn when-at-step
  [index point]
  (if (zero? (mod index step)) point))


(def point-xf
  (comp (keep-indexed when-at-step)
        (map normalized)
        (map-indexed shaped)
        (map-indexed point)
        (map x-align)))


(defn shift-line
  [points]
  (loop [remaining points
         acc (transient [])]
    (if (empty? remaining)
      (persistent! acc)
      (recur (rest remaining)
             (conj! acc
                    (Point. (.-x (first remaining))
                            (- (.-y (first remaining)) y-step)))))))


(defn shift-lines
  [lines]
  (map shift-line (take (- lines-count 1) lines)))


(defn update-loop
  ([frequencies-fn] (js/requestAnimationFrame (fn [] (update-loop [] frequencies-fn))))
  ([previous frequencies-fn]
   (let [new-line (into () point-xf (frequencies-fn))
         shifted (shift-lines previous)
         lines (conj shifted new-line)]
     (js/requestAnimationFrame
       (fn []
         (draw-lines (reverse lines))
         (update-loop lines frequencies-fn))))))


(defn start
  []
  (go
    (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
          audio-context (create-audio-context)
          source (.createMediaStreamSource audio-context stream)
          analyser (create-analyser audio-context)]
      (.connect source analyser)
      (update-loop (fn [] (updated-frequencies analyser))))))


(defn main
  []
  (.addEventListener
    (js/document.querySelector "#start")
    "click"
    (fn [event]
      (.remove (.-target event))
      (js/document.documentElement.requestFullscreen)
      (start))))


(main)
