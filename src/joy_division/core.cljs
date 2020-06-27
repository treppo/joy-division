(ns joy-division.core
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]))

(def resolution 32)
(def step 10)

(def audioContext (js/AudioContext.))
(def analyser (let [a (.createAnalyser audioContext)]
                (set! (.-fftSize a) resolution)
                a))
(def frequency-ints (js/Uint8Array. (.-fftSize analyser)))

(def canvas
  (let [element (js/document.querySelector "canvas")
        size (* resolution step)
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

(defn normalize [ints] (map (fn [i] (/ i 128.0)) ints))
(defn to-points [frequencies] (map-indexed (fn [i ratio] {:x (* i step), :y (* ratio resolution)}) frequencies))
(defn clear-canvas [] (.clearRect canvas-context 0 0 (.-width canvas) (.-height canvas)))

(defn draw []
  (let [frequencies (normalize (update-frequencies!))
        points (to-points frequencies)]
    (clear-canvas)
    (.beginPath canvas-context)
    (.moveTo canvas-context (:x (first points)) (:y (first points)))

    (doseq [point (rest points)]
      (.lineTo canvas-context (:x point) (:y point)))

    (.lineTo canvas-context (.-width canvas) 10)

    (.stroke canvas-context)

    (js/requestAnimationFrame draw)
    ))

(go
  (let [stream (<p! (js/navigator.mediaDevices.getUserMedia #js {:audio true}))
        source (.createMediaStreamSource audioContext stream)]
    (.connect source analyser)
    (draw)))
