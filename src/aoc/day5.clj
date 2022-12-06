(def filename "resources/day5")

(def stacks-n-moves (clojure.string/split (slurp filename) #"\n\n"))

(def moves (clojure.string/split-lines (second stacks-n-moves)))

(def stacks (drop-last (clojure.string/split-lines (first stacks-n-moves))))

(defn get-box-line
  [string]
  (flatten (partition 1 4 (drop 1 string))))

(defn get-box-position
  [box-line]
  (for [x (range 9)
        :let [index (inc x)]]
    [index (nth box-line x)]))

(defn add-box 
  [stack-map key box]
  (update-in stack-map [key] conj box))

(defn fill-map [boxes]
  (reduce (fn [box-map [key value]] 
            (if (= \space value)
              box-map
              (add-box box-map key value))) {} 
          (apply concat (map get-box-position (map get-box-line boxes)))))

(defn update-values [m f & args]
 (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(def box-stacks
  (update-values (fill-map stacks) vec))

(defn add-boxes 
  [stack-map key box]
  (update-in stack-map [key] #(apply conj % box)))

(defn stack-empty? 
  [stack-map key]
  (empty? (get stack-map key)))

(defn drop-vec [input times]
  (vec (drop-last times input)))


(defn move-box [stack-map times start-key end-key]
  (let [removed-boxes (take-last times (get stack-map start-key)) 
        map-without-box 
        (if (stack-empty? stack-map start-key)
          stack-map
          (update-in stack-map [start-key] drop-vec times))]
    (if removed-boxes
      (add-boxes map-without-box end-key (vec (flatten [removed-boxes])))
      stack-map)))


(defn generate-instruction 
  [string]
  (let [[_ move _ start _ end] (clojure.string/split string #" ")]
    [(parse-long move) (parse-long start) (parse-long end)]))

(def parsed-moves (map generate-instruction moves))

(defn final-form [boxes]
  (reduce (fn [stacks [times start end]] (move-box stacks times start end)) boxes
       (parsed-moves)))

;; fred-overflow

(defn move1 
  [stacks [n from to]]
  (loop [source (stacks from)
         target (stacks to)
         n      n]
    (if (pos? n)
      (recur
       (pop source)
       (conj target (peek source))
       (dec n))
      (assoc stacks
             from source
             to   target))))

(defn part1
  [stacks moves]
  (->>
   (reduce move1 stacks moves)
   (map peek)
   (apply str)))

(defn move2 
  [stacks [n from to]]
  (loop [source (stacks from)
         temp   ()
         n      n]
    (if (pos? n)
      (recur
       (pop source)
       (conj temp (peek source))
       (dec n))
      (assoc stacks
             from source
             to   (into (stacks to) temp)))))

(defn part2
  [stacks moves]
  (->>
   (reduce move2 stacks moves)
   (map peek)
   (apply str)))
