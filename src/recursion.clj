(ns recursion)


(defn product [coll]
  (if (empty? coll)
    1
    (*
      (first coll)
      (product (rest coll)))))


(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(singleton? [1 nil])

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(defn max-rec [prev a-seq]
  (if (singleton? a-seq)
    (max prev (first a-seq))
    (recur (max prev (first a-seq)) (rest a-seq))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max-rec (first a-seq) (rest a-seq)))))

(max-element [2 4 1 4 4 5  6  1 3 2 3]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-seq-rec [prev-seq seqs]
  (if (singleton? seqs)
    (seq-max prev-seq (first seqs))
    (recur (seq-max prev-seq (first seqs)) (rest seqs))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (longest-seq-rec (first a-seq) (rest a-seq)))))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()

(defn only-numbers? [coll]
  (cond
   (empty? coll)
     true                        ; the empty sequence contains only numbers
   (number? (first coll))
     (only-numbers? (rest coll)) ; we got a number, let's check the rest
   :else
     false))                     ; it wasn't a number so we have our answer

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (recur elem (rest a-seq))))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (recur pred? (rest a-seq))
    :else
      a-seq))

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (not (empty? b-seq)))
      false
    (and (empty? b-seq) (not (empty? a-seq)))
      false
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (recur (rest a-seq) (rest b-seq))
    :else
      false))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [] [])             ;=> true
(seq= [1 2 nil] [1 2])   ;=> false
(seq= [1 4 2] [1 2 4])   ;=> false
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false)

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2))
      '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
  (cond
    (zero? n)
      0
    (zero? k)
      1
    :else
      (* n (power n (dec k)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0)
      '()
    :else
      (cons
        what-to-repeat
        (my-repeat (dec how-many-times) what-to-repeat))))

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (cond
    (<= up-to 0)
      '()
    :else
      (cons
        (dec up-to)
        (my-range (dec up-to)))))

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (first a-seq)
    (cons
      (seq a-seq)
      (tails (rest a-seq)))
    '(()) ))


(defn inits [a-seq]
  (if (first a-seq)
    (cons
      (seq a-seq)
      (inits (reverse (rest (reverse a-seq)))))
    '(()) ))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest
      (map
        concat
        (tails a-seq)
        (reverse (inits a-seq))))))

(rotations [])
(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (let [item (first a-seq)
        old-val (get freqs item)
        new-val (if old-val (inc old-val) 1)
        new-freqs (assoc freqs item new-val)]
    (if (singleton? a-seq)
      new-freqs
      (recur new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(first (vals {:a 3}))
(first (keys {:a 3}))

(repeat 3 :a)

(defn un-frequencies-helper [un-freqs freqs]
  (let [repeats (first (vals freqs))
        item (first (keys freqs))
        new-un-freqs (concat (repeat repeats item) un-freqs)]
    (if (singleton? freqs)
      new-un-freqs
      (un-frequencies-helper
        new-un-freqs
        (rest freqs)))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (un-frequencies-helper '() a-map)))

(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}


(defn my-take [n coll]
  (cond
    (empty? coll)
      '()
    (<= n 0)
      '()
    :else
      (cons
        (first coll)
        (my-take (dec n) (rest coll)))))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)
(my-take 0 [1 2])
(my-take 5 [])
(my-take 5 [1 2])

(defn my-drop [n coll]
  (cond
    (empty? coll)
      '()
    (<= n 0)
      coll
    :else
      (recur (dec n) (rest coll))))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()
(my-drop 0 [1 3 4])

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        first-half (take half a-seq)
        second-half (drop half a-seq)]
    [first-half second-half]))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]
(halve [])

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      '()
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge '(4) '(1 2 6 7))        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)
(seq-merge '(1) '(2 3))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (singleton? a-seq)
      a-seq
    :else
      (let [halved (halve a-seq)
            first-half (first halved)
            second-half (first (rest halved))]
        (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(merge-sort [])                 ;=> ()
(merge-sort [1])                 ;=> ()
(merge-sort [2])                 ;=> ()
(merge-sort [2 1])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(defn split-into-monotonics [a-seq])

(defn permutations [a-set])

(defn powerset [a-set])

