(def filename "resources/day3")

(def rucksack-list (clojure.string/split (slurp filename) #"\n"))

(defn generate-char-sequence [first, last] 
  (map char (range (int first) (inc (int last)))))

(def item-sequence (concat (generate-char-sequence \a \z) (generate-char-sequence \A \Z)))

(defn get-item-piority [item]
  (inc (.indexOf item-sequence item)))

(defn split-rucksack-compartments [rucksack]
  (split-at (/ (count rucksack) 2) rucksack))

(defn find-duplicated [splited-rucksack]
  (first (filter #(.contains (last splited-rucksack) %) (first splited-rucksack))))

(defn rucksack->priority [rucksack]
  (get-item-piority (find-duplicated (split-rucksack-compartments rucksack))))

(defn sum-all-rucksack-priorities [rucksacks]
  (apply + (map rucksack->priority rucksacks)))

(def grouped-elves (partition 3 rucksack-list))

(defn find-duplicated-triplet [elf-triplet]
  (let [elf-group (map seq elf-triplet)]
    (first (filter (fn [arg]
                     (and 
                      (.contains (second elf-group) arg) 
                      (.contains (last elf-group) arg))) 
                   (first elf-group)))))

(defn sum-all-badges [grouped-list]
  (apply + (map #(get-item-piority (find-duplicated-triplet %)) grouped-list)))


;; fred overflow

(defn cut-in-half
  [s]
  (let [length (count s)
        middle (quot length 2)
        front  (. s substring 0 middle)
        back   (. s substring middle length)]
    [front back]))

(defn common-letter
  [strings]
  (first (apply clojure.set/intersection (map set strings))))

(def priority (zipmap item-sequence (range 1 53)))

(defn part1 []
  (transduce 
   (comp 
    (map cut-in-half)
    (map common-letter)
    (map priority)) 
   + 0 
   rucksack-list))

(defn part2 []
  (transduce 
   (comp 
    (partition-all 3)
    (map common-letter)
    (map priority)) 
   + 0 
   rucksack-list))
