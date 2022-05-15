(ns edaa40.core
    (:use clojure.set)
)

;;
;;  testing
;;

(defn test?

  (
    [msg v]
    
    (if v
      (println msg ": passed")
      (println msg ": ERROR")
    )
  )

  (
    [msg v1 v2]

    (if (= v1 v2)
      (println msg ": passed")
      (println msg ": ERROR -- got'" v1 "', expected '" v2 "'")
    )
  )
)

;;
;;
;;

(defn third [S]
  (nth S 2)
)

(defn is-sorted-by? 
    (
        [keyfn S]
    
        (= S (sort-by keyfn S))
    )
    (
        [keyfn comp S]
        
        (= S (sort-by keyfn comp S))
    )
)


(defn abs [v]
  (if (> 0 v) (-' v) v)
)

(defn sqrt [r] (java.lang.Math/sqrt r))

(defn sqr [n] (* n n))

(defn- isqrt' [n r]
  (let [nxtr (/ (+' r (/ n r)) 2)]
    (if (< (abs (-' nxtr r)) 1)
      nxtr
      (recur n nxtr)
    )
  )
)

(defn isqrt 
  "returns the largest integer not greater than the square root of n"
  [n]
  (if (>= n 1)
    (let [r (bigint (isqrt' n n))]
      (if (<= Long/MIN_VALUE r Long/MAX_VALUE)
	(long r)
	r
      )
    )
    0
  )
)

(defn isqrt2 
  "returns a pair [a b] of two non-negative integers such that n = a^2 + b and n < (a+1)^2"
  [n]
  (let [r (isqrt n) e (-' n (*' r r))] [r e])
)


(defn gcd 
  "computes the greatest common divisor of two integers"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))
  )
)

(defn lcm
  "computes the least common multiple of two integers"
  [a b]
  (/ (* a b) (gcd a b))
)

(defn divides? 
  "returns true of a divides b evenly, i.e. without remainder"
  [a b] 
  (zero? (rem b a))
)


(defn prime? 
  "true, iff n is a prime"
  [n]
  (and
    (> n 1)
    (empty? (filter #(divides? % n) (range 2 (+ 1 (isqrt n)))))
  )
)

(defn- primes-to' [P k n]
  (if (> k n)
    P
    (recur
      (if (some #(divides? % k) P) P (conj P k))
      (inc k)
      n
    )
  )
)

(defn primes-to [n]
  (reverse (primes-to' '() 2 n))
)

(defn primes-between [a b]
  (filter #(>= % a) (primes-to b))
)

(declare Product)

(defn factorial [n]
  (Product (range 1 (inc n)))
)

(defn primorial [n]
  (Product (primes-to n))
)

(defn factors-of 
  "computes a list of all factors of an integer n"
  [n]
  (apply concat (map #(if (zero? (mod n %)) (if (= n (* % %)) (list %) (list % (/ n %))) '()) (range 1 (+ 1(isqrt n)))))
)

(defn prime-factors-of 
  "computes a list of all prime factors of an integer n"
  [n]
  (filter prime? (factors-of n))
)

(defn coprime? [a b]
  (empty? (intersection (set (prime-factors-of a)) (set (prime-factors-of b))))
)

(defn Product [S]
  (reduce *' 1 S)
)

(defn Sum [S]
  (reduce +' 0 S)
)

(defn perfect? [n]
  (= (Sum (factors-of n)) (* 2 n))
)

