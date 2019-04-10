(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; changed snakee representation to a vector (instead of a list) for slightly better performance

(defn spawn-food
  "Spawn food at interval 0.5"
  [state]
  (if (< (rand) 0.5)
    (update state :food conj [(rand-int (:width state)) (rand-int (:width state))])
    state))

(defn remove-food
  "Remove food when eaten"
  [state point]
  (update state :food disj point))

(def opposite-direction {[0 1]  [0 -1]
                         [0 -1] [0 1]
                         [1 0]  [-1 0]
                         [-1 0]  [1 0]})

(defn update-direction
  "Update direction to new-direction unless it is the opposite of current direction"
  [state new-direction]
  (if (= new-direction (opposite-direction (:direction state)))
    ;; do not change
    state
    (assoc state :direction new-direction)))
  
(defn turn
  "Change direction for the snake to move"
  [state event]
  (let [direction (:direction state)]
    (case (:key event)
      (:w :up)    (update-direction state [0 -1])
      (:s :down)  (update-direction state [0 1])
      (:a :left)  (update-direction state  [-1 0])
      (:d :right) (update-direction state [1 0])
      (case (:key-code event)
        ;; space, return, newline, tab -- toggle pause
        (32 13 10 9) (update state :pause not)
        ;; default ignore key
        state))))

(defn reset
  "Reset a snake if it touches itself"
  [state]
  (let [snakee (:snakee state)]
    ;; check if head is touching rest of snake
    (if (some #{(peek snakee)} (pop snakee))
      (assoc state :snakee [(peek snakee)])
      state)))

;; returns string for display, doesn't affect state
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
    ;; no movement
    state
    (let [snakee (:snakee state)
          head (peek snakee)
          [dx dy] (:direction state)
          new-head [(mod (+ (first head) dx) (state :width))
                    (mod (+ (second head) dy) (state :height))]]
      (if (contains? (:food state) head)
        (-> state
            (remove-food head)
            (paint-food)
            (update :snakee conj new-head))
        (-> state
            (update :snakee subvec 1)
            (update :snakee conj new-head))))))

(defn setup []
  "Return initial state of game"
  (q/smooth)
  (q/frame-rate 10)
  ;; state map
  {:width 100
   :height 100
   :snakee [[50 50]]
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
      ;; better to `apply` colour than to pull apart RGB args
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
