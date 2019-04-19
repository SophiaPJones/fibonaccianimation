(ns sketch.supportfuncs)

(defn fib
  ([] (fib 1 0))
  ([n,p] (cons n (lazy-seq (fib (+ n p) n)))))

(def cursor {:x 0
             :y 0})

(defn move-cursor
  [newx newy]
  (assoc cursor :x newx :y newy))

(defn gen-rect-data
  [startx starty n]
  (let [fibseq (vec (take (+ n 1) (fib)))]
    (loop [cnt      1
           udlr     0
           curpos   (move-cursor (+ startx 1) starty)
           rectdata [{:x startx
                      :y starty
                      :width 1
                      :height 1}]]
      (if (= cnt n)
        rectdata
        (recur (inc cnt)
               (inc (mod udlr 4))
               (cond
                 (= 0 udlr)
                 (assoc curpos
                        :x (- (get curpos :x) (get fibseq cnt))
                        :y (- (get curpos :y) (get fibseq (inc cnt))))
                 (= 1 udlr)
                 (assoc curpos
                        :x (- (get curpos :x) (get fibseq (inc cnt)))
                        :y (get curpos :y))
                 (= 2 udlr)
                 (assoc curpos
                        :x (get curpos :x) 
                        :y (+ (get curpos :y) (get fibseq cnt)))
                 (= 3 udlr)
                 (assoc curpos
                        :x (+ (get curpos :x) (get fibseq cnt))
                        :y (- (get curpos :y) (get fibseq (dec cnt))))
                 (= 4 udlr)
                 (assoc curpos
                        :x (- (get curpos :x) (get fibseq (dec cnt)))
                        :y (- (get curpos :y) (get fibseq (inc cnt)))))
               (conj rectdata {:x (curpos :x)
                               :y (curpos :y)
                               :width (get fibseq cnt)
                               :height (get fibseq cnt)}))))))

;figured this could get expensive, since depth isn't changing may as well memo it.
(def gen-rect-data-memo (memoize gen-rect-data))

(defn scale-rect-data
  [x
   y
   rect-data
   scalefactor]
  (loop [cnt 0
         curmap (get rect-data cnt)
         outputmap []]
    (if (nil? curmap)
      outputmap 
      (recur (inc cnt)
             (get rect-data (inc cnt))
             (conj outputmap {:x (+
                                  (* (:x curmap) scalefactor)
                                  (* x (- 1 scalefactor)))
                              :y (+
                                  (* (:y curmap) scalefactor)
                                  (* y (- 1 scalefactor)))
                              :width (*
                                      (:width curmap)
                                      scalefactor)
                              :height (*
                                       (:height curmap)
                                       scalefactor)})))))
