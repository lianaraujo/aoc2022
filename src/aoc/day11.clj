(def filename "resources/day11")

(defn parse-monkeys
  [input]
  (clojure.string/split (slurp input) #"\n\n"))

(map clojure.string/split-lines (parse-monkeys filename))
