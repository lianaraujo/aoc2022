(def filename "resources/day6")


(defn detect-signal [packets size]
  (loop [signal  (slurp packets)
         packet  []
         counter 0]
    (if (= size (count (set packet)))
      counter
      (recur (drop 1 signal) 
             (if (= size (count packet))
               (conj (apply vector(drop 1 packet)) (first signal))
               (conj packet (first signal)))
             (inc counter)))))

;; fred overflow

(defn part1 
  [input]
  (loop [j 4]
    (let [window (. input substring (- j 4) j)]
      (if (apply distinct? window)
        j
        (recur (inc j))))))

;; another approach
(defn part2
  [input]
  (first 
   (for [j (range 14 (count input))
              :let [window (. input substring (- j 14) j)]
              :when (apply distinct? window)]
          j)))
