(ns sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [sketch.supportfuncs :as s])
  (:gen-class))

(defn rainbowcolors
  [offset
   frame
   period]
  (let [slopeup (/ 255 (* period (/ 1 6)))
        slopedown (- (/ 255 (* period (/ 1 6))))
        oframe (mod (+ frame offset) period)]
    (vec (map int [(if (< oframe (/ period 2))
                     (min 255 (max (* (- oframe (/ period 3)) slopedown) 0))
                     (min 255 (max (* (- oframe (* period (/ 2 3))) slopeup) 0)))
                   (if (> oframe (/ period 6))
                     (min 255 (max (* (- oframe (* period (/ 2 3))) slopedown) 0))
                     (min 255 (max (* oframe slopeup) 0)))
                   (if (> oframe (* period (/ 2 3)))
                     (min 255 (max (* (- oframe period) slopedown) 0))
                     (min 255 (max (* (- oframe (* period (/ 1 3))) slopeup) 0)))]))))

(defn setup []
  (q/color-mode :rgb)
  (let [x (/ (q/width) 2)
        y (/ (q/height) 2)
        fibdepth 36
        origscale (Math/pow (/ 1 1.61803399) (* 4 5)) 
        threshold (* origscale (Math/pow 1.61803399 (* 4 5)))
        scalefacrate 1.03
        framerate 60]
    (q/frame-rate framerate)
    (q/set-state! :x x
                  :y y
                  :rect-data (s/gen-rect-data-memo x y fibdepth)
                  :fibdepth fibdepth
                  :originalscale origscale
                  :scale-factor origscale
                  :scale-threshold threshold 
                  :scale-factor-rate scalefacrate
                  :color-offsets [0 20 40 60 80]
                  :color-period (int (/ (Math/log (/ threshold origscale)) (Math/log scalefacrate)))
                  :color-frame 0)))

(defn update-state [state]
  {:rect-data (s/scale-rect-data (:x state) (:y state)
                                 (s/gen-rect-data-memo (:x state)
                                                       (:y state)
                                                       (:fibdepth state))
                                 (:scale-factor state))
   :originalscale (:originalscale state)
   :scale-factor (if (>= (:scale-factor state) (:scale-threshold state))
                   (:originalscale state)
                   (* (:scale-factor state) (:scale-factor-rate state)))
   :scale-factor-rate (:scale-factor-rate state)
   :scale-threshold (:scale-threshold state)
   :x (:x state)
   :y (:y state)
   :fibdepth (:fibdepth state)
   :color-offsets (:color-offsets state)
   :color-period (:color-period state)
   :color-frame (mod (inc (:color-frame state)) (:color-period state))})

(defn draw-state [state]
  (q/background 240)
  (q/fill 125 230 171)
  (q/stroke 0 0 0)
  (q/stroke-weight 1)
  (let [rectdata (:rect-data state)
        phi 2.1]
    ;this loop draws the squares (possibly make recursive for readability/perf???)
    (loop [cnt 0
           color (rainbowcolors (get (:color-offsets state) 0)
                                (:color-frame state)
                                (:color-period state))]
      (if (= cnt (:fibdepth state))
        '(nil)
        (do
          (q/fill (get color 0) (get color 1) (get color 2))
          (q/rect ((get rectdata cnt) :x)
                  ((get rectdata cnt) :y)
                  ((get rectdata cnt) :width)
                  ((get rectdata cnt) :height))          
          (recur (inc cnt)
                 (rainbowcolors (get (:color-offsets state) (mod (inc cnt) 5))
                                (:color-frame state)
                                (:color-period state))))))
    (q/no-fill)
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    ;loop draws the spiral
    (loop [cnt 0
           curvedir 0
           curx ((get rectdata cnt) :x)
           cury ((get rectdata cnt) :y)
           width ((get rectdata cnt) :width)
           height ((get rectdata cnt) :height)]
      
      (if (= cnt (:fibdepth state))
        '(nil)
        (do
          ;(println rectdata) this shit will absolutely yeet the repl into oblivion
          (cond
            (= curvedir 0)
            (q/bezier curx cury
                      curx (+ cury (/ height phi))
                      (+ curx (/ width phi)) (+ cury height)
                      (+ curx width) (+ cury height))
            (= curvedir 1)
            (q/bezier curx (+ cury height)
                      (+ curx (/ width phi)) (+ cury height)
                      (+ curx width) (+ cury (/ height phi))
                      (+ curx width) cury)
            (= curvedir 2)
            (q/bezier (+ curx width) (+ cury height)
                      (+ curx width) (+ cury (/ height 2))
                      (+ curx (/ width phi)) cury
                      curx cury)
            (= curvedir 3) 
            (q/bezier (+ curx width) cury
                      (+ curx (/ width phi)) cury
                      curx (+ cury (/ height phi))
                      curx (+ cury height)))
          (recur (inc cnt)
                 (mod cnt 4)
                 ((get rectdata cnt) :x)
                 ((get rectdata cnt) :y)
                 ((get rectdata cnt) :width)
                 ((get rectdata cnt) :height)))))))



(defn -main
  [& args]
  (q/sketch
    :title "fib"
    :host "host"
    :size :fullscreen
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top
               :resizeable
               :present]
    :middleware [m/fun-mode]))
