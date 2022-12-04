(def filename "resources/day2")

(def games-list (clojure.string/split (slurp filename) #"\n"))

(def play-table 
  {:X {:value 1 :A 3 :B 0 :C 6} 
   :Y {:value 2 :A 6 :B 3 :C 0} 
   :Z {:value 3 :A 0 :B 6 :C 3}})

(def play-values
  {:A {:X 3 :Y 4 :Z 8} 
   :B {:X 1 :Y 5 :Z 9}
   :C {:X 2 :Y 6 :Z 7}})


(defn string->keyword-sequence [pair-string] 
  (map keyword (clojure.string/split pair-string #" ")))

(defn get-pair-value 
  [keyword-tuple] 
  (let [values-map ((last keyword-tuple) play-table)]
    (+ (:value values-map) ((first keyword-tuple) values-map))))

(def keyworded-values (map string->keyword-sequence games-list))

(def first-rule-list (map get-pair-value keyworded-values))

(def second-rule-list 
  (map (fn [arg] ((last arg) ((first arg) play-values))) keyworded-values))

(defn get-total-score [rule-list] 
  (apply + rule-list))

