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
