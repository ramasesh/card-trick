(ns card-trick.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn remove-from-vector
  "Removes an element from a vector, i.e. 
  (remove-from-vector [5 3 3 0] 3) => [5 3 0]
  (remove-from-vector [5 3 0] 0]) => [5 3]
  (remove-from-vector [5 3 0] 9) => [5 3 0]"
  [orig-vector to-remove] 
  (vec 
    (concat
      (take-while (complement #(= % to-remove)) orig-vector)
      (drop 1 (drop-while (complement #(= % to-remove)) orig-vector))
      )
    )
  )

(defn min-greater-than
   "Returns the smallest element in the given
   vector which is greater than the element provided
   as the second argument, i.e.
   (min-greater-than [5 3 3 0] 2) => 3

   TODO: Eventually I would like this to not error
         if there is no element greater in the array
         (this will be relevant for the final permutation)
   (min-greater-than [5 3 3 0] 9) => nil"
   [array element]
   (apply min (filter #(> % element) array)
          )
   ) 

(def nonpos? (complement pos?))
(def nonneg? (complement neg?))

(defn length-longest-non-increasing-suffix
  "Returns the length of the longest non-increasing 
  suffix of the given vector, i.e. the suffix for which 
  no element is greater than the element to its left"
  [sequence]
  (inc (count (take-while nonpos? 
    (map - 
         (reverse sequence) 
         (rest (reverse sequence)))))))

(defn pivot-position
  "Returns the position of the 'pivot', i.e. the element
  of the vector immediately before the longest 
  non-increasing suffix begins"
  [sequ]
  (- (count sequ) 
     (length-longest-non-increasing-suffix sequ)))

(defn pivot
  [permutation]
  (nth permutation (dec (pivot-position permutation))))

(defn swap-in-and-sort
  "Swaps the given element into the vector, removing the 
  smallest element which was larger than it, and sorts 
  the new suffix, prepending the swapped-out element"
  [to-swap-in original-seq]
  (concat 
    (vector (min-greater-than original-seq to-swap-in)) 
    (sort (into 
            (remove-from-vector original-seq (min-greater-than original-seq to-swap-in)) 
            [to-swap-in]))) 
)

(defn suffix
  [permutation]
  (drop (pivot-position permutation) permutation))

(defn next-permutation 
  "Returns the lexicographically next permutation"
  [permutation]
  (concat 
    (take (dec (pivot-position permutation)) permutation)
    (swap-in-and-sort (pivot permutation) (suffix permutation))))

(defn nth-permutation
  "Returns the lexicographically nth permutation of number-of-things things"
  [number-of-things n]
  (nth 
    (iterate next-permutation (range number-of-things)) n))


