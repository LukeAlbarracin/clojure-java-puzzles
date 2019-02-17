(ns base-changer.core)

(defn convert-to-letter [num]
  (if (> num 9)
    (do
      (cond
        (= num 10) "A"
        (= num 11) "B"
        (= num 12) "C"
        (= num 13) "D"
        (= num 14) "E"
        (= num 15) "F"
        :otherwise (.throw (Exception. "Please make sure you entered your number correctly"))))
    (str num)))

(defn convert-from-letter 
  [letter new-base]
    (if (integer? (read-string letter))
      (read-string letter)
      (do
        (cond
          (= letter "A") 10
          (= letter "B") 11
          (= letter "C") 12
          (= letter "D") 13
          (= letter "E") 14
          (= letter "F") 15
          :otherwise (.throw (Exception. "Please make sure you entered your number correctly"))))))
  
(defn convert-from-decimal [total new-base new-value]
  (if (> total 0)
    (do
      (let [temp (mod total new-base)]
        (recur (quot total new-base) new-base (apply str [(convert-to-letter temp) new-value]))))
    new-value))

(defn convert-to-decimal [old-base new-base old-value]
  (let [total (atom 0) temp (apply merge (map-indexed array-map (reverse old-value)))]
    (reduce + 
      (map 
        (fn [[x y]] (* (int (Math/pow old-base x)) (convert-from-letter (str y) new-base))) temp))))

(defn convert-base [old-base new-base old-value]
  (let [total (convert-to-decimal old-base new-base old-value)]
    (convert-from-decimal total new-base "")))

(defn -main [& args]
  (println "Enter the current base: ")
  (def old-base (read-string (read-line)))

  (println "Enter the desired base: ")
  (def new-base (read-string (read-line)))

  (println "Enter the current value: ")
  (def old-value (read-line))
  
  (def new-value (str (convert-base old-base new-base old-value)))

  (println (str "The old value of base " (str old-base) ": " old-value))
  (println (str "The new value of base " (str new-base) ": " new-value)))
