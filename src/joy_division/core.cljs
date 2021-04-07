(ns joy-division.core
  (:require
    [cljs.core.async :refer [go]]
    [cljs.core.async.interop :refer-macros [<p!]]))


(def y-step 10)
(def step 8)
(def top-padding 20)
(def bottom-margin 20)
(def amplification 20)
(def margin 10)
(def line-width 2)
(defn window-width [] (.-innerWidth js/window))
(defn window-height [] (.-innerHeight js/window))


(defn resolution
  []
  "has to be power of two"
  (let [fitting-exponent (Math/floor (/ (Math/log (window-width)) (Math/log 2)))]
    ;; decrement exponent for the graph to be portrait
    (Math/pow 2 (- fitting-exponent 1))))


(defn points-per-line [] (/ (resolution) js/devicePixelRatio step))
(defn width [] (* (points-per-line) step))


(defn lines-count
  []
  (/ (Math/floor (/ (- (window-height) (* js/devicePixelRatio top-padding) bottom-margin) y-step)) js/devicePixelRatio))


(defn height [] (+ top-padding (* (lines-count) y-step)))


(defn init-config
  []
  {:y-step y-step
   :step step
   :width (width)
   :height (height)
   :margin margin
   :amplification amplification
   :lines-count (lines-count)})


(defn create-audio-context
  []
  (if (js-in "webkitAudioContext" js/window)
    (js/webkitAudioContext.)
    (js/AudioContext.)))


(defn create-analyser
  [audio-context]
  (let [a (.createAnalyser audio-context)]
    (set! (.-fftSize a) (resolution))
    a))


(defn canvas
  [{:keys [width height]}]
  (let [element (js/document.querySelector "canvas")]
    (set! (.-width element) (* width js/devicePixelRatio))
    (set! (.-height element) (* height js/devicePixelRatio))
    element))


(defn canvas-context
  [config]
  (let [ctx (.getContext (canvas config) "2d")
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
  [lines canvas-context]
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
  [i frequency {:keys [step width margin amplification]}]
  (let [x (* step i)
        distance-to-center (Math/abs (- x (/ width 2)))
        variance (- (/ width 2) distance-to-center)
        framed (max (- variance margin) 0)]
    (* (* amplification frequency) (/ framed amplification))))


(defn point
  [i frequency {step :step height :height}]
  (Point. (* step i) (- height frequency)))


(defn x-align
  [point step]
  (Point. (+ (.-x point) (/ step 2)) (.-y point)))


(defn when-at-step
  [index frequency step]
  (if (zero? (mod index step)) frequency))


(defn point-xf
  [config]
  (comp (keep-indexed (fn [idx freq] (when-at-step idx freq (:step config))))
        (map normalized)
        (map-indexed (fn [idx freq] (shaped idx freq config)))
        (map-indexed (fn [idx freq] (point idx freq config)))
        (map (fn [point] (x-align point (:step config))))))


(defn shift-line
  [points y-step]
  (loop [remaining points
         acc (transient [])]
    (if (empty? remaining)
      (persistent! acc)
      (recur (rest remaining)
             (conj! acc
                    (Point. (.-x (first remaining))
                            (- (.-y (first remaining)) y-step)))))))


(defn shift-lines
  [lines {:keys [y-step lines-count]}]
  (map #(shift-line % y-step) (take (- lines-count 1) lines)))


(defn update-loop
  ([frequencies-fn] (js/requestAnimationFrame (fn [] (update-loop [] frequencies-fn))))
  ([previous frequencies-fn]
   (let [config (init-config)
         new-line (into () (point-xf config) (frequencies-fn))
         shifted (shift-lines previous config)
         lines (conj shifted new-line)]
     (js/requestAnimationFrame
       (fn []
         (draw-lines (reverse lines) (canvas-context config))
         (update-loop lines frequencies-fn))))))


(defn start
  []
  (go
    (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
          audio-context (create-audio-context)
          source (.createMediaStreamSource audio-context stream)
          analyser (create-analyser audio-context)]
      (.connect source analyser)
      (update-loop #(updated-frequencies analyser)))))


(defn main
  []
  (.addEventListener (js/document.querySelector "#start") "click"
                     (fn [event]
                       (.remove (js/document.querySelector "p"))
                       (start)))
  (.addEventListener js/document "keypress"
                     (fn [event]
                       (when (or (= (.-key event) "Enter") (= (.-key event) " "))
                         (js/document.documentElement.requestFullscreen))))
  (.addEventListener js/document "dblclick"
                     (fn [event]
                       (js/document.documentElement.requestFullscreen))))


(main)
