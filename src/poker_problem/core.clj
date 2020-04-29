(ns poker-problem.core
  (:gen-class))
(require '[clojure.string :as cs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.4clojure.com/problem/128
;; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9} . 
;; A ten will always be represented with the single character "T", rather than the two characters "10" .
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Def a function to take the card and split the input into suit/rank (assume first character is always suit, second always rank.
(defn split-2-suit-rank [card]
  (let [[suit rank] (cs/upper-case card)] ;note - forced an upper case to avoid possible bugs
    [suit rank] 
    ))

(assert (= (split-2-suit-rank "H5") [\H \5]))

; 2. Def a suit map which will be used to convert the suit character into a :suit

(def suits {\H :heart
            \D :diamond
            \S :spade
            \C :club})

; 3. Def a rank map which will be used to convert the rank character into a :rank
(def ranks {\2 0  ;; AP-- same comments as above
               \3 1
               \4 2
               \5 3
               \6 4
               \7 5
               \8 6
               \9 7
               \T 8
               \J 9
               \Q 10
               \K 11
               \A 12})

; 4. Now, wrap it all up:

(defn convert-card
  "Take a single card in format 'SJ' and convert it into a hash-map output in format {:suit :spade, :rank 9}"
  [card]
  (->
   (let [[suit rank] (split-2-suit-rank card)]
     (hash-map :suit (suits suit) :rank (ranks rank)))))

(assert (= (convert-card "SJ") {:suit :spade, :rank 9}))

(assert (= (range 13) (map (comp :rank convert-card str)
                           '[S2 S3 S4 S5 S6 S7
                             S8 S9 ST SJ SQ SK SA])))
