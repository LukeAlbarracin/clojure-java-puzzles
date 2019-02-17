(ns base-changer.core)

(defn convert-from-decimal [total new-base new-value]
  (if (> total 0)
    (do
      (let [temp (mod total new-base)]
        (recur (quot total new-base) new-base (apply str [(str temp) new-value]))))
    new-value))

(defn convert-to-decimal [old-base new-base old-value]
  (let [total (atom 0) temp (apply merge (map-indexed array-map (reverse old-value)))]
    (int (reduce + 
      (map 
        (fn [[x y]] (* (int (Math/pow old-base x)) (read-string (str y)))) temp)))))

(defn convert-base [old-base new-base old-value]
  (let [total (convert-to-decimal old-base new-base old-value)]
    (convert-from-decimal total new-base "")))

(defn -main [& args]
  (def old-base 10)
  (def new-base 2)
  (def old-value "21")
  (println (convert-base old-base new-base old-value)))
