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
  [old-tail new-tail movement]
  (case (movement 0)
    "U" (for [y (range (old-tail 1) (inc (new-tail 1)))
              :let [position (str (new-tail 0) y)]] position)
    "D" (for [y (range (new-tail 1) (inc (old-tail 1)))
              :let [position (str (new-tail 0) y)]] position)
    "L" (for [x (range (new-tail 0) (inc (old-tail 0)))
              :let [position (str (new-tail 1) x)]] position)
    "R" (for [x (range (old-tail 0) (inc (new-tail 0)))
              :let [position (str (new-tail 1) x)]] position)))


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
        (println (str "movement" movement "head" head-position " " new-head "tail" tail-position " " new-tail))
        (recur other-moves new-head new-tail new-visited)))))

(part1 example)
