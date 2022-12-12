(def filename "resources/day9")

(def example "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse-moves
  [input]
  (map #(clojure.string/split % #" ") (clojure.string/split-lines input)))

(defn move
  [position movement]
  (case (movement 0)
    "U" (vector (position 0) (+ (position 1) (parse-long (movement 1))))
    "D" (vector (position 0) (- (position 1) (parse-long (movement 1))))
    "L" (vector (- (position 0) (parse-long (movement 1))) (position 1))
    "R" (vector (+ (position 0) (parse-long (movement 1))) (position 1))))

(defn move-tail
  [head-position tail-position movement]
  (let [y-delta (abs (- (tail-position 1) (head-position 1)))
        x-delta (abs (- (tail-position 0) (head-position 0)))] 
    (case (movement 0)
      "U" (if (> y-delta 1)
            (vector (if (> x-delta 0)
                      (head-position 0)
                      (tail-position 0)) 
                    (+ (tail-position 1) (dec (parse-long (movement 1)))))
            tail-position)
      "D" (if (> y-delta 1) 
            (vector (if (> x-delta 0)
                      (head-position 0)
                      (tail-position 0)) 
                    (- (tail-position 1) (dec (parse-long (movement 1)))))
            tail-position)
      "L" (if (> x-delta 1) 
            (vector (- (tail-position 0) (dec (parse-long (movement 1)))) 
                    (if (> y-delta 0)
                      (head-position 1)
                      (tail-position 1)))
            tail-position)
      "R" (if (> x-delta 1)
            (vector (+ (tail-position 0) (dec (parse-long (movement 1)))) 
                    (if (> y-delta 0)
                      (head-position 1)
                      (tail-position 1)))
            tail-position))))

(defn get-visited
  [new-head old-tail new-tail movement]
  (case (movement 0)
    "U" (for [y (range (old-tail 1) (inc (new-tail 1)))
              :let [position (str (new-tail 0) y)]] position)
    "D" (for [y (range (new-tail 1) (inc (old-tail 1)))
              :let [position (str (new-tail 0) y)]] position)
    "L" (for [x (range (new-tail 0) (inc (old-tail 0)))
              :let [position (str x (new-tail 1))]] position)
    "R" (for [x (range (old-tail 0) (inc (new-tail 0)))
              :let [position (str x (new-tail 1))]] position)))


(defn part1
  [input]
  (loop [[movement & other-moves] (parse-moves input)
         head-position [0 0]
         tail-position [0 0]
         visited (conj #{} (apply str tail-position))]
    (if (empty? other-moves)
      (do
        (println visited)
        (count visited))
      (let [new-head (move head-position movement)
            new-tail (move-tail new-head tail-position movement)
            new-visited (clojure.set/union visited 
                                           (set (get-visited tail-position new-tail movement)))]
        (recur other-moves new-head new-tail new-visited)))))


;; fred overflow

(defn unroll-directions
  [input]
  (for [[_ lrud n] (re-seq #"(L|R|U|D) (\d+)" input)
        direction (repeat (parse-long n) ({\L [-1 0]
                                           \R [+1 0]
                                           \U [0 -1]
                                           \D [0 +1]} (nth lrud 0)))]
    direction))


(defn plus [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn minus [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(def forces1
  {[-2 0] [-1 0]
   [+2 0] [+1 0]
   [0 -2] [0 -1]
   [0 +2] [0 +1]
   [-2 -1] [-1 -1]
   [-2 +1] [-1 +1]
   [+2 -1] [+1 -1]
   [+2 +1] [+1 +1]
   [-1 -2] [-1 -1]
   [+1 -2] [+1 -1]
   [-1 +2] [-1 +1]
   [+1 +2] [+1 +1]})

(defn part1f
  [input]
  (loop [head [0 0]
         tail head
         visited #{}
         [direction & directions] (unroll-directions input)]
    (let [visited (conj visited tail)]
      (if-not direction
        (count visited)
        (let [head (plus head direction)
              delta (minus head tail)
              force (forces1 delta)
              tail (if force (plus tail force) tail)]
          (recur head tail visited directions))))))

(def force2
  (assoc forces1
         [-2 -2] [-1 -1]
         [-2 +2] [-1 +1]
         [+2 -2] [+1 -1]
         [+2 +2] [+1 +1]))

(defn follow
  [[moved-head first-tail & more-tails :as rope]]
  (if-not first-tail
    rope
    (let [delta (minus moved-head first-tail)
          force (force2 delta)]
      (if-not force
        rope
        (cons moved-head (follow (cons (plus first-tail force) more-tails)))))))

(defn part2f
  [input] 
  (loop [[head & tails] (repeat 10 [0 0])
         visited #{}
         [direction & directions] (unroll-directions input)]
    (let [visited (conj visited (last tails))]
      (if-not direction
        (count visited)
        (let [head (plus head direction)]
          (recur (follow (cons head tails)) visited directions))))))
