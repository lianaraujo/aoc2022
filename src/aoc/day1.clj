(def filename "resources/inputday1")

(defn split-elves [rawinput]
  (clojure.string/split rawinput #"\n *\n *"))

(defn split-meals [elf-list]
  (map #(clojure.string/split % #"\n") elf-list))

(def elf-meals-list (split-meals (split-elves (slurp filename))))

(defn sum-all [meals-list] 
  (map #(apply + (map (fn [arg] (Integer/parseInt arg)) %)) meals-list))

(def sum-meal-list (sum-all elf-meals-list))

(defn get-highest-calories [sum-list] (apply max sum-list))

(defn get-position [calories calories-list] 
  (+ 1 (.indexOf calories-list calories)))

(def sorted-list (sort-by - sum-meal-list))

(defn get-sum-of-top-three [sorted-vector] (apply + (take 3 sorted-vector)))
