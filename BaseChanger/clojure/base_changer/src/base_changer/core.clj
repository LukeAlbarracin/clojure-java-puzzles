(ns base-changer.core)

(defn- convert-from-decimal
  ([total new-base]
    (if (> total 0)
      (convert-from-decimal (/ total new-base) new-base (str (mod total new-base)))
      "0"))
  ([total new-base new-value]
    (if (> total 0)
      (convert-from-decimal (/ total new-base new-base (str (mod total new-base) new-value)))
      new-value)))
      
(defn- convert-to-decimal 
  ([old-base new-base old-value]
    (let [total (int (Math/pow old-base (* (- (count old-value) 1) (read-string (str (nth old-value 0))))))]
      (convert-from-decimal (old-base new-base old-value 1 total))))
  ([old-base new-base old-value index total]
    (if (= index (- (count old-value) 1))
      (do 
        (let [new-total (int (Math/pow new-base (* (- (count old-value) 1) (read-string (str (nth old-value index))))))]
          (convert-to-decimal new-total new-base)))
      (do
        (let [new-total (int (Math/pow new-base ((- (count old-value) 1) * (read-string (str (nth old-value index))))))]
          (convert-from-decimal (old-base new-base old-value (inc index) (+ total new-total))))))))

(defn -main [& args]
  (println "Enter the current base: ")
  (def old-base (read-string (read-line)))
  (println "Enter the desired base: ")
  (def new-base (read-string (read-line)))
  (println "Enter the current value: ")
  (def old-value (read-line))
  (println (str (convert-to-decimal old-base new-base old-value))))
