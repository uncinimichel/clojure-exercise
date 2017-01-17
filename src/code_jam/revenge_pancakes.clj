(ns exercise-clojure.pancakes)

(def a-pancake ["-" "-" "+" "-"])

(defn winning-pancake
  [a-pancake]
  (every? #(= % "+") a-pancake))

(defn invert-pancake
  [pancake]
  (cond
    (= "-" pancake) "+"
    (= "+" pancake) "-"))

(defn invert-pancake-stack
  ([stack pos]
   (let [[a b] (split-at pos stack)
         a1 (->> a
              reverse
              (map invert-pancake)
              (into []))]
        (vec (flatten (conj a1 b))))))

(invert-pancake-stack ["-" "-" "+"] 3)

(map inc (hash-set 1 2 3))
(range 3)


(defn all-combination
  [a-pancake]
  (let [size (count a-pancake)]
    (for [i (range 1 (+ 1 size))]
         (invert-pancake-stack a-pancake i))))

(nth (all-combination ["-" "-"]) 0)



(defn revenge
  [i solution all-solution]
  (Thread/sleep 500)
  (println i "i")
  (println solution "solution")
  (println all-solution "all-solution")
  (if (winning-pancake solution)
    (println "WIN!!!!" i)
    (do
      (let [combinations (all-combination solution)
            new-i (inc i)]
        (for [x (range (count combinations))
              :let [test (nth combinations x)]
              :when (not (contains? all-solution test))]
             (revenge new-i test (conj all-solution solution)))))))

(revenge 0 ["-" "-" "+" "-"] #{})
(not-every? odd? `(1 1 0))

(map all-combination [["-" "-"]])
