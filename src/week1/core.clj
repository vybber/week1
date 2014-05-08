(ns week1.core
  (:gen-class))

(defn merge-and-count [ls rs]
  (loop [r []
         cnt 0
         ls ls
         rs rs]
    (let [ls (seq ls)
          rs (seq rs)
          fls (first ls)
          frs (first rs)]
      (if (or ls rs)
        (cond
           (and ls rs) (if (< fls frs)
                         (recur
                            (conj r fls)
                            cnt
                            (rest ls)
                            rs)
                         (recur
                            (conj r frs)
                            (+ cnt (count ls))
                            ls
                            (rest rs)))
           ls (recur (concat r ls) cnt '() '())
           rs (recur (concat r rs) cnt '() '()))
        {:r r :c cnt}))))

(defn sort-and-count [s]
  (let [cnt (count s)
        mid (quot cnt 2)]
    (if (<= cnt 1)
      {:c 0 :r s}
      (let [l-res (sort-and-count (subvec s 0 mid))
            r-res (sort-and-count (subvec s mid))
            m-res (merge-and-count (:r l-res) (:r r-res))]
            {:c (+ (:c l-res) (:c r-res) (:c m-res))
             :r (:r m-res)}))))

(defn read-ints [path]
  (mapv read-string
       (with-open [rdr (clojure.java.io/reader path )]
          (reduce conj [] (line-seq rdr)))))

(defn calulate-inversions [path]
  (:c (sort-and-count (read-ints path))))

(defn -main
  [& args]
  (if-not (seq args)
    (println "Usage week1 [file-path]")
    (time (println "Number of inversions: " (calulate-inversions (first args))))))
