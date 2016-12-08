(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn spawn-food
  "Spawn food at interval 0.5"
  [state]
  (if (< (rand) 0.5)
    (update state :food conj [(rand-int (:width state)) (rand-int (:width state))])
    state))

(defn remove-food
  "Remove food when eaten"
  [state]
  (update state :food disj (first (:snakee state))))

;; SEM -- use mod instead of recur
#_ (defn wrap
  "Wrap a snake around the board"
  [i m]
  (loop [x i]
    (cond (< x 0) (recur (+ x m))
          (>= x m) (recur (- x m))
          :else x)))


(defn turn
  "Change direction for the snake to move"
  [state event]
  (let [direction (:direction state)]
    (case (:key event)
      (:w :up) (if (not= [0 1] direction) (assoc state :direction [0 -1]) state)
      (:s :down) (if (not= [0 -1] direction) (assoc state :direction [0 1]) state)
      (:a :left) (if (not= [1 0] direction) (assoc state :direction [-1 0]) state)
      (:d :right) (if (not= [-1 0] direction) (assoc state :direction [1 0]) state)
      (case (:key-code event)
        ;; space, return, newline, tab
        (32 13 10 9) (update state :pause not)
        state))))

(defn reset
  "Reset a snake if it touches itself"
  [state]
  (let [snakee (:snakee state)]
    (if (apply distinct? snakee)
      state
      (assoc state :snakee (or (list (first snakee)) (list [50 50]))))))

;; returns string for display
(defn score
  "Show score"
  [state]
  (str "Score: " (count (:snakee state))))

(defn paint-food
  "Create a fancy colour for the snake"
  [state]
  (let [x (rand-int 255)
        y (rand-int 255)
        z (rand-int 255)]
    (assoc state :colour [x y z])))

(defn move
  "Move a snake"
  [state]
  (if (:pause state)
    state
    (let [snakee (:snakee state)
          [dx dy] (:direction state)
          new-point [(mod (+ (ffirst snakee) dx) (state :width))
                     (mod (+ (second (first snakee)) dy) (state :height))]]
      (if (contains? (:food state) (first snakee))
        (-> state
            (remove-food)
            (paint-food)
            (update :snakee conj new-point))
        (-> state
            (update :snakee butlast)
            (update :snakee conj new-point))))))

(defn setup []
  (q/smooth)
  (q/frame-rate 10)
  ;; state map
  {:width 100
   :height 100
   :snakee (list [50 50])
   :food #{}
   :direction [1 0]
   :colour [255 13 169]})

;; SEM: Clojure already has `update` so renamed this one
(defn update-state [state]
  (-> state
      (spawn-food)
      (move)
      (reset)))

(defn draw [state]
  (let [colour (:colour state)
        w (quot (q/width) (:width state))
        h (quot (q/height) (:height state))]
    (q/background 0 0 0)

    (doseq [[x y] (:food state)]
      ;; better to `apply` to full colour
      (apply q/fill colour)
      (apply q/stroke colour)
      (q/rect (* w x) (* h y) w h))

    (doseq [[x y] (:snakee state)]
        (q/fill (rand-int 255) (rand-int 255) (rand-int 255))
        (q/stroke (rand-int 255) (rand-int 255) (rand-int 255))
        (q/rect (* w x) (* h y) w h)))

  (q/fill 100 255 100)
  (q/text-size 20)
  (q/text (score state) 10 30 ))

(q/defsketch snake
  :title "Snake"
  :size [700 700]
  :setup setup
  :update update-state
  :draw draw
  :key-pressed turn
  :features [:keep-on-top]
  :middleware [m/fun-mode])
