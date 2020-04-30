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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.4clojure.com/problem/178 
;; Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards. The hand rankings are listed below for your convenience:
;; 1. Straight flush: All cards in the same suit, and in sequence
;; 2. Four of a kind: Four of the cards have the same rank
;; 3. Full House: Three cards of one rank, the other two of another rank
;; 4. Flush: All cards in the same suit
;; 5. Straight: All cards in sequence (aces can be high or low, but not both at once)
;; 6. Three of a kind: Three of the cards have the same rank
;; 7. Two pair: Two pairs of cards have the same rank
;; 8. Pair: Two cards have the same rank
;; 9. High card: None of the above conditions are met
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. write a in-sequence? function (aces can be high or low, but not both at once)

(defn in-sequence?
  "Determine in all cards in hand are in sequence (aces can be high or low, but not both at once).
   This function firstly does a matrix subtraction of the lowest rank from all other ranks. It
   then takes the sum of the remaining vector of ranks. This must sum to 10 for the high ace case.
   For the low ace case, the function removes the high rank from the list, before doing the matrix
   subtraction, in which case the sum of the vector must be 6."
  [hand]
  (let [[& hand-ranks] (map :rank (map convert-card hand))]
    (or (= (apply + (map #(- % (apply min hand-ranks)) hand-ranks)) 10) ; high ace case
        (if (= (apply max hand-ranks) 12)                       ; low ace case
          (let [new-ranks (remove #(= % 12) hand-ranks)]
            (= (apply + (map #(- % (apply min new-ranks)) new-ranks)) 6))
          ))))

(assert (= true (in-sequence? ["HA" "HK" "HQ" "HJ" "HT"])))
(assert (= true (in-sequence? ["HA" "H2" "S3" "D4" "C5"])))
(assert (= false (in-sequence? ["HA" "D2" "H3" "C9" "DJ"])))

(assert (= false (in-sequence? ["HA" "HQ" "SJ" "DA" "HT"])))
(assert (= false (in-sequence? ["HA" "DA" "HQ" "SQ" "HT"])))
(assert (= false (in-sequence? ["HA" "DA" "CA" "HJ" "HT"])))

;; 2. write a same-suit? function

(defn same-suit?
  "Determine in all cards have the same suit."
  [hand]
  (let [[s1 s2 s3 s4 s5] (map :suit (map convert-card hand))]
    (= s1 s2 s3 s4 s5)))

(assert (= true (same-suit? ["HA" "HK" "HQ" "HJ" "HT"])))
(assert (= false (same-suit? ["HA" "D2" "H3" "C9" "DJ"])))

;; 3. write a four-same-rank? function 

(defn four-same-rank?
  "Determine if any four cards have the same rank."
  [hand]
  (let [[& all-ranks] (map :rank (map convert-card hand))]
    (or (apply = (take 4 (sort all-ranks)))
        (apply = (take 4 (reverse (sort all-ranks)))))))

(assert (= true (four-same-rank? ["HA" "DA" "CA" "SA" "DJ"])))
(assert (= false (four-same-rank? ["HA" "DK" "CQ" "SA" "DJ"])))

;; 4. write a full-house? function 

(defn full-house?
  "Determine in there is a 3 of a kind and a 2 of a kind in the hand."
  [hand]
  (let [[& all-ranks] (map :rank (map convert-card hand))]
    (or (= true (apply = (take 2 (sort all-ranks))) (apply = (take 3 (drop 2(sort all-ranks)))))      ; case 1 1 2 2 2
        (= true (apply = (take 3 (sort all-ranks))) (apply = (take 2 (drop 3 (sort all-ranks))))))))  ; case 1 1 1 2 2

(assert (= true (full-house? ["HA" "DA" "CA" "SJ" "DJ"])))
(assert (= true (full-house? ["HK" "DK" "CQ" "SQ" "DQ"])))
(assert (= false (full-house? ["HK" "DQ" "CJ" "SQ" "DQ"])))

;; 4. write a 3-same-rank? function

(defn three-of-a-kind?
  "Determine if three cards have the same rank"
  [hand]
  (let [[& all-ranks] (map :rank (map convert-card hand))]
    (or (apply = (take 3 (sort all-ranks)))
        (apply = (take 3 (reverse (sort all-ranks)))))))

(assert (= true (three-of-a-kind? ["HA" "DA" "CA" "HJ" "HT"])))
(assert (= true (three-of-a-kind? ["H2" "D2" "C2" "HJ" "HT"])))
(assert (= false (three-of-a-kind? ["HA" "DK" "CQ" "SA" "DJ"])))

;; 5. write a 2-pair-same-rank? function - needs to pick up more than one occurance in a hand

(defn two-pairs?
  "Determine in there is two pairs in the hand"
  [hand]
  (let [[& all-ranks] (map :rank (map convert-card hand))]
    (or (= true (apply = (take 2 (sort all-ranks))) (apply = (take 2 (drop 2 (sort all-ranks)))))              ; case: 1 1 2 2 3
        (= true (apply = (take 2 (sort all-ranks))) (apply = (take 2 (drop 3 (sort all-ranks)))))              ; case: 1 1 2 3 3
        (= true (apply = (take 2 (drop 1 (sort all-ranks)))) (apply = (take 2 (drop 3 (sort all-ranks))))))))  ; case: 1 2 2 3 3

(assert (= true (two-pairs? ["HA" "DA" "CJ" "ST" "DT"])))
(assert (= true (two-pairs? ["HA" "DA" "CJ" "SJ" "DT"])))
(assert (= true (two-pairs? ["HA" "DQ" "CQ" "S2" "D2"])))
(assert (= false (two-pairs? ["HA" "DK" "CQ" "SA" "DJ"])))


;; 6. write a pair? function

(defn pair?
  "Determine if any two cards have the same rank"
  [hand]
  (let [[& all-ranks] (map :rank (map convert-card hand))]
    (or (apply = (take 2 (sort all-ranks)))              ; case: 1 1 2 3 4
        (apply = (take 2 (drop 1 (sort all-ranks))))     ; case: 1 2 2 3 4
        (apply = (take 2 (drop 2 (sort all-ranks))))     ; case: 1 2 3 3 4
        (apply = (take 2 (drop 3 (sort all-ranks)))))))  ; case: 1 2 3 4 4

(assert (= true (pair? ["HA" "DA" "CJ" "ST" "DT"])))
(assert (= true (pair? ["HA" "DK" "CQ" "SA" "DJ"])))
(assert (= false (pair? ["HA" "DK" "CQ" "SJ" "DT"])))

;; 7. write a straight-flush? function by joining the two requirements 

(defn straight-flush?
  "Determine if the hand meets the requirements of both being in-sequence and all of the same-suit?"
  [hand]
  (= true (in-sequence? hand) (same-suit? hand)) ; is combining these functions an alternative solution for this?
  )

(assert (= true (straight-flush? ["HA" "HK" "HQ" "HJ" "HT"])))
(assert (= false (straight-flush? ["HA" "DK" "CQ" "SJ" "DT"])))


;; 9. wrap it all up:

;; This list is ordered from best to worst hand. A high card is purposely missing from the bottom of the list as 
;; it becomes the default for the best-nad function.

(def hand-types '((:straight-flush straight-flush?)
                 (:four-of-a-kind four-same-rank?)
                 (:full-house full-house?)
                 (:flush same-suit?)
                 (:straight in-sequence?)
                 (:three-of-a-kind three-of-a-kind?)
                 (:two-pair two-pairs?)
                 (:pair pair?)))



(defn best-hand
  "Find the best hand from the cards by reducing the ordered hand-types list from worst to best and returning the last matching hand (i.e. the best match). 
   If no hand matches, then best-hand returns the default :high-card"
  [hand]
  (reduce (fn [best input]
            (if (= ((resolve (symbol (second input))) hand) true)
              (first input)
              best
              ))
          :high-card
          (reverse hand-types)))


(assert (= :high-card (best-hand ["HA" "D2" "H3" "C9" "DJ"])))
(assert (= :pair (best-hand ["HA" "HQ" "SJ" "DA" "HT"])))
(assert (= :two-pair (best-hand ["HA" "DA" "HQ" "SQ" "HT"])))
(assert (= :three-of-a-kind (best-hand ["HA" "DA" "CA" "HJ" "HT"])))
(assert (= :straight (best-hand ["HA" "DK" "HQ" "HJ" "HT"])))
(assert (= :straight (best-hand ["HA" "H2" "S3" "D4" "C5"])))
(assert (= :flush (best-hand ["HA" "HK" "H2" "H4" "HT"])))
(assert (= :full-house (best-hand ["HA" "DA" "CA" "HJ" "DJ"])))
(assert (= :four-of-a-kind (best-hand ["HA" "DA" "CA" "SA" "DJ"])))
(assert (= :straight-flush (best-hand ["HA" "HK" "HQ" "HJ" "HT"])))
