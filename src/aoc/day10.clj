(def filename "resources/day10")

;;addx two cycles
;;noop one

(def example "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop")

(defn parse-instructions
  [input]
  (for [[_ instruction _ argument] (re-seq #"(\w+)( (-?\d+))?" input)]
    [instruction  (some-> argument parse-long)]))

(defn X-register-values
  [input]
  (loop [result (transient [])
         X 1
         [[instruction argument] & instructions] (parse-instructions input)]
    (if-not instruction
      (persistent! (conj! result X))
      (case instruction
        "noop" (recur (conj! result X)
                      X
                      instructions)
        "addx" (recur
                (reduce conj! result (repeat 2 X))
                (+ X argument)
                instructions)))))

(defn part1
  [input]
  (transduce (comp (drop 19) (take-nth 40))
             +
             0
             (map * (range 1 Long/MAX_VALUE) (X-register-values input))))


(defn screen-pixel
  [crt-position sprite-middle]
  (case (- crt-position sprite-middle)
    (-1 0 +1) \# \.))

(defn part2
  [input]
  (->> (map screen-pixel (cycle (range 0 40)) (X-register-values input))
       (partition 40)
       (map clojure.string/join)))


