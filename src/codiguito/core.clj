(ns codiguito.core
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]]))

(defn rect-from-polar [[x y] r a]
  ((juxt #(+ x (* r (sin %))) #(+ y (* r (cos %)))) a))

(defn star [[x y] ro ri color]
  (stroke 236 240 241)
  (stroke-weight 2)
  (apply fill color)
  (begin-shape)
  (doseq [t (range 10)]
    (let [a (* t (/ TWO-PI 10))
          r (if (even? t) ri ro)]
      (apply vertex (rect-from-polar [x y] r a))))
  (end-shape :close))

(defn super-formula [m n1 n2 n3]
  #(pow (+ (pow (abs (cos (* m % 0.25))) n2) (pow (abs(sin (* m % 0.25))) n3)) (/ -1 n1)))

(defn super-shape [r t s m n1 n2 n3]
  (-> t (+ (* s TWO-PI)) ((super-formula m n1 n2 n3)) (* r)))

(defn parametric-circle [r t]
  (rect-from-polar [0 0] r t))

(defn parametric-heart [r t s]
  ((juxt #(* r 16 (pow (sin %) 3))
         #(* r (+ (* 13 (cos %))
                  (* -5 (cos (* 2 %)))
                  (* -2 (cos (* 3 %)))
                  (* -1 (cos (* 4 %)))))) (+ t (* s TWO-PI))))

(defn my-blend [[x1 y1] [x2 y2] p]
  (let [x (+ (* x1 p) (* x2 (- 1 p)))
        y (+ (* y1 p) (* y2 (- 1 p)))]
    [x y]))

(defn stars [how-many b]
  (for [t (range how-many)]
    (let [[x1 y1] (parametric-circle 0 (* t (/ TWO-PI how-many)))
          [x2 y2] (parametric-heart 15 (* t (/ TWO-PI how-many)) (* 0.3 b))
          [x  y ] (my-blend [x1 y1] [x2 y2] b)]
      [(+ x (/ (width) 2)) (- (/ (width) 2) y)])))

(defn colors
  ([] (colors 0))
  ([offset]
   (drop offset (cycle [[  52 152 219]
                        [ 231  76  60]
                        [  46 204 113]
                        [ 241 196  15]
                        [ 155  89 182]]))))

(defn setup []
  (smooth)
  (frame-rate 30)
  (let [interp (cycle (map (fn [t] (-> t sin (+ 1) (/ 2)))
                           (range-incl 0 TWO-PI 0.07)))]
    (set-state! :interp (seq->stream interp))))

(defn draw []
  (background 34 49 63)
  (doseq
    [[position color] (map vector (stars 30 ((state :interp))) (colors))]
    (star position 15 9 color)))

(defn -main []
  (defsketch codiguito
    :title "Codiguito"
    :setup setup
    :draw draw
    :size [600 600]))

(-main)
