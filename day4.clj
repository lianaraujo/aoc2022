(def filename "resources/day4")

(def elf-pairs (clojure.string/split (slurp filename) #"\n"))

(defn split-by-comma [string]
  (clojure.string/split string #","))

(defn split-by-dash [string]
  (clojure.string/split string #"-"))

(defn superset?-or-subset? [first-vec second-vec]
  (or
   (clojure.set/superset? (set first-vec) (set second-vec))
   (clojure.set/subset? (set first-vec) (set second-vec))))

(defn intersect? [first-vec second-vec]
  (not (empty? (clojure.set/intersection (set first-vec) (set second-vec)))))

(defn generate-range [numbers]
  (let [splitted (split-by-dash numbers)]
    (range (Integer/parseInt (first splitted)) (inc (Integer/parseInt (last splitted))))))

(defn compare-ranges [pair condition]
  (let [splitted (split-by-comma pair)]
    (condition (generate-range (first splitted)) (generate-range (last splitted)))))


(defn count-subset-pairs-with-rule [pairs rule]
  (count (filter #(compare-ranges % rule) pairs)))

