(ns poker-problem.number-maze
  (:gen-class))
(require '[clojure.string :as cs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.4clojure.com/problem/128
;; Given a pair of numbers, the start and end point, find a path between the two using only three possible operations: 
;; double
;; halve (odd numbers cannot be halved)
;; add 2
;; 
;; Find the shortest path through the "maze". Because there are multiple shortest paths, you must return the length of the shortest path, not the path itself.
;; 
;; Example output:
;; (= 9 (__ 9 2))  ; 9 18 20 10 12 6 8 4 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def magic-n 2) ;; the problem uses 2 as the default, but this abstraction makes it useable for other scenarios

(def mult-n (partial * magic-n))
(def add-n (partial + magic-n))
(defn div-n [x] (/ x magic-n))
;; - couldnt get partial for / to work properly??
;; - this returns a N (BigInt) not Long: 
;;   (def div-n (partial * (/ 1 magic-n)))

(assert (= (add-n 5) 7))
(assert (= (div-n 4) 2))
(assert (= (mult-n 4) 8))


(defn my-last 
  "Will take either an integer or vector. Output is either the integer or (last cols)"
 [in]
 (if (vector? in)
   (last in)
   in))

(assert (= (my-last [1 2 3]) 3))
(assert (= (my-last [1 2 3 4 5 6 -19]) -19))
(assert (= (my-last 1) 1))
(assert (= (my-last 101) 101))

(defn create-new-path
  "creates the 3 new paths for the maze from the one point"
  [this-path]
  (let [in (my-last this-path)]
    (if (odd? in)
    (conj [] (add-n in) (mult-n in))
    (conj [] (add-n in) (mult-n in) (div-n in)))))

(assert (= (create-new-path [2]) [4 4 1]))
(assert (= (set (create-new-path 1)) (set [2 3])))

(defn new-list
  "Takes the starting path and new path options as inputs. 
   Outputs the conj vectors with the 3 new path options."
  [branch tree]
  (let [new-tree [tree tree tree]]
    (map conj new-tree branch)))

(assert (= (new-list [1 2 3] [1 2 6 7 8]) '([1 2 6 7 8 1] [1 2 6 7 8 2] [1 2 6 7 8 3])))
(assert (= (new-list [2 3] [33 44 55]) '([33 44 55 2] [33 44 55 3])))


(defn- check-end
  "Determine if the program has found the solution by looking at the last entry in all new paths."
  [end cols]
   (set (filter #(= (last %) end) cols)))

(assert (seq (check-end 3 '([1 2 3] [23 4] [3])))) ;; note: (= (seq x) (not (empty? x)))
(assert (empty? (check-end 23 '([1 2 3] [23 4] [3]))))

;; consider adding futures here to speed up

(defn maze-find
  "Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:
       - double
       - halve (odd numbers cannot be halved)
       - add 2
  Finds the shortest path through the 'maze'. Return the length of the shortest path, not the path itself."
  [start end]
  (if (= start end) 1 ;; case where no computation is required
      (let [maze-start (vector start)
            found? (promise)
            all-paths (atom (new-list (create-new-path maze-start) maze-start)) ;; set initial val as first branch
            best-path (atom [])]

        ;; Check if the initial val is a solution before entering while loop
        (if (seq (check-end end @all-paths))
          (do (reset-vals! best-path (check-end end @all-paths))
              (deliver found? true)))

        (while (not (realized? found?))
          (let [current-path @all-paths]
            (reset-vals! all-paths  []) ; throw out all unseccesful paths - maintaning history with let above.
            (doseq [current current-path]
              (swap! all-paths into (set (new-list (create-new-path current) current)))
              (let [checked-end (check-end end @all-paths)]
                (if (seq checked-end)
                  (do (reset-vals! best-path checked-end)
                      (deliver found? true)))))))
        (println @best-path) ;; prints the best paths for reference
        (apply min (map count @best-path))))) ;; Can be multiple solutions

(assert (= 1 (maze-find 1 1)))
(assert (= 2 (maze-find 1 2)))
(assert (= 3 (maze-find 3 12)))
(assert (= 3 (maze-find 12 3)))
(assert (= 3 (maze-find 5 9)))
(assert (= 9 (maze-find 9 2)))
(assert (= 5 (maze-find 9 12)))


(comment 
(maze-find 1 500) 
 - > #{[1 3 6 12 14 28 30 60 62 124 248 250 500] [1 3 5 7 14 28 30 60 62 124 248 250 500]}
  "takes quite a while.... 13 steps")