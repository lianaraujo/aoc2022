(def filename "resources/day8")

(def example "30373
25512
65332
33549
35390")

(defn parse-matrix
  [input]
  (mapv (fn [string-list] (mapv parse-long string-list)) 
        (mapv #(clojure.string/split % #"")
              (clojure.string/split-lines input))))

(defn calculate-visible-perimeter
  [input]
  (let [tree-field input
        side-size(count tree-field)]
    (- (* 2 (* 2 side-size)) 4)))

(defn transpose [m]
  (apply mapv vector m))

; (matrix row-index)
(defn north-south-visibility
  [index row-index matrix]
  (let [column ((transpose matrix) index)
        head-vec (subvec column 0 row-index)
        tail-vec (subvec column (inc row-index))]
    (or (not (some #(>= % (column row-index)) head-vec))
        (not (some #(>= % (column row-index)) tail-vec)))))

(defn count-visible-trees 
  [row row-index matrix]
  (loop [index 1
         head-vec (subvec row 0 index)
         tail-vec (subvec row (inc index))
         visible 0]
    (if (= (inc index) (count row))
      visible
      (let [new-index (inc index)
            new-head (subvec row 0 new-index)
            new-tail (subvec row (inc new-index))
            visible? (or (not (some #(>= % (row index)) head-vec))
                         (not (some #(>= % (row index)) tail-vec))
                         (north-south-visibility index row-index matrix))]
        (recur new-index new-head new-tail (if visible? 
                                             (inc visible) 
                                             visible))))))

(defn part1 
  [input]
  (let [matrix (parse-matrix input)]
    (loop [[row & rows] (drop 1 matrix)
           row-index 1
           visible-trees (calculate-visible-perimeter matrix)]
      (if (empty? rows)
        visible-trees
        (recur 
         rows 
         (inc row-index) 
         (+ visible-trees (count-visible-trees row row-index matrix)))))))



;;fred overflow

(defn string-to-digits
  [input]
  (mapv #(Character/digit % 10) input))

(defn parse-grid
  [input]
  (mapv string-to-digits (clojure.string/split input #"\n *")))

(defn visible?
  [[tree & other-trees]]
  (every? #(< % tree) other-trees))

(defn part1f
  [input]
  (let [grid (parse-grid input)
        dirg (transpose grid)
        length (count grid)
        width (count (grid 0))]
    (count
     (for [y (range 0 length)
           x (range 0 width)
           :let [a (println (str y " " x))]
           :when (or
                  (visible?       (subvec (grid y) x         ))
                  (visible? (rseq (subvec (grid y) 0 (inc x))))
                  (visible?       (subvec (dirg x) y         ))
                  (visible? (rseq (subvec (dirg x) 0 (inc y)))))]
       nil))))

(defn calculate-score
  [[tree & other-trees]]
  (let [smaller-trees (take-while #(< % tree) other-trees)]
    (min
     (count other-trees)
     (inc (count smaller-trees)))))

(defn score-list
  [input]
  (let [grid (parse-grid input)
        dirg (transpose grid)
        length (count grid)
        width (count (grid 0))]
    (for [y (range 0 length)
          x (range 0 width)] 
      (* 
       (calculate-score       (subvec (grid y) x         ))
       (calculate-score (rseq (subvec (grid y) 0 (inc x))))
       (calculate-score       (subvec (dirg x) y         ))
       (calculate-score (rseq (subvec (dirg x) 0 (inc y))))))))

