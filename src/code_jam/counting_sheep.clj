(ns exercise-clojure.counting
  (:require
    [exercise-clojure.globals :as globals]))

(defn is-solution-outside-limit?
  [n]
  (or (< n 0)
      (> n 1000000)))

(defn is-test-outside-limit?
  [i]
  (or (< i 0)
      (> i 100)))

(defn add-if-not-present
  [track-num coll]
  (reduce (fn
            [so-far ele]
            (if-not (some (partial = ele) so-far)
              (conj so-far ele)
              so-far))
          track-num coll))

(defn is-done?
  [coll]
  (= (count coll) 10))

(defn counting-sheep
  ([n]
   (loop [i 1
          track-num []]
    ;  (Thread/sleep 1000)
     (let [solution (* i n)
           new-track-num (add-if-not-present track-num (seq (str solution)))]
       (cond
        (is-done? new-track-num) solution
        (is-solution-outside-limit? solution) "INSOMNIA"
        (is-test-outside-limit? i) "INSOMNIA"
        :else
          (recur (inc i) new-track-num))))))

(map-indexed #(str "Case #"(inc %1) ":" (counting-sheep %2))
             globals/input)
