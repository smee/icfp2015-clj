(ns io.remote
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(set! *print-length* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; webservice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def token "C/1d7grJDHHZEBserpvNAckXRf2+LGVtibuDE8ukD4k=")
(defn load-problems []
  (map (comp json/read-json slurp) (filter #(.isFile %) (file-seq (io/as-file "resources")))))

(defn parse [url]
  (json/read-json (:body (http/get url))))
;;;;;;;;;;;;;;;;;;;;;;;; stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- with-index [vs]
  (map-indexed vector vs))

(defn random-gen [seed]
  (let [last (atom seed)
        mask (bit-shift-left (dec (bit-shift-left 1 (- 31 16))) 16)]
    (fn []
      (let [res (bit-shift-right (bit-and @last mask) 16)]
        (swap! last (fn [v] (mod (+ 12345 (* v 1103515245)) (bit-shift-left 1 32))))
        res))))


(defn- plus [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (+ x1 x2)
   :y (+ y1 y2)})

(defn- minus [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (+ x1 x2)
   :y (+ y1 y2)})

;;;;;;;;;;;;;;;; board;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-board [{:keys [width height filled]}]
  (mapv vec
        (for [row (range height)]
         (for [col (range width)]
          (if (some #(and (= col (:x %))
                          (= row (:y %))) filled)
            1
            0)))))

(defn print-board [board]
  (doseq [[i row] (with-index board) :let [odd? (= 1 (mod i 2))]]
    (when odd? (print "  ")) ; two space because my font seems to be proportional :(
    (doseq [cell row]
      (print ([\u2B21 \u2B22] cell) ""))
    (println))
  (println))

;;;;;;;;;;;;;;;; units ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- unit->matrix [{:keys [members]}]
  (let [max-x (apply max (map :x members))
        max-y (apply max (map :y members))]    
    (mapv vec
          (for [y (range (inc max-y))]
            (for [x (range (inc max-x))
                  :let [coord {:x x :y y}]]
              (if (some #(= % coord) members)
                1
                0))))))


(defn print-unit [{:keys [pivot] :as unit}] 
  (let [coords (unit->matrix unit)] 
    (doseq [[i row] (with-index coords) 
            :let [odd? (= 1 (mod i 2))]]
      (when odd? (print "  ")) ; two space because my font seems to be proportional :(
      (doseq [[j cell] (with-index row)
              :let [pivot? (= pivot {:x j :y i})
                    chars (if pivot? [\u25ef \u25cf] [\u2B21 \u2B22])]]
        (print (chars cell) ""))
      (println))))

(defrecord State [board unit])

(defn spawn-unit 
  "Convert local coordinates into board coordinates
so the unit is centered with its top on the top row of the board.
Returns shifted unit"
  [task {:keys [members pivot] :as unit}]
  (let [board-width (:width task)
        coords (unit->matrix unit)
        unit-top-row (first coords)
        unit-width (count unit-top-row)
        offset-left (count (take-while zero? unit-top-row))
        offset-right (count (take-while zero? (reverse unit-top-row)))
        unit-top-width (- unit-width offset-left offset-right)
        offset {:y 0 :x (- (int (/ (- board-width unit-top-width) 2)) offset-left)}]
    {:pivot (plus pivot offset)
     :members (mapv (partial plus offset) members)}))

(defn add-to-board [board {ms :members}]
  (reduce (fn [board {col :x row :y}]
            (update-in board [row col] inc)) 
          board ms))

(defn overlaps-anything? 
  "If the unit would get added to the board, would any cells overlap?"
  [board unit]
  (some #{2} (apply concat (add-to-board board unit))))

;;;;;;;;;;;;;;; coordinates ;;;;;;;;;;;;;;;;;;
(defn grid->cube [{col :x row :y}]
  (let [x (- col (/ (- row (bit-and row 1)) 2))
        z row
        y (- (- x) z)] 
    [x y z]))

(defn cube->grid-even-row [[x y z]]
  (let [col (+ x (/ (- z (bit-and z 1)) 2))
        row z] 
    {:x col :y row}))

(defn cube->grid-odd-row [[x y z]]
  (let [col (+ x (/ (+ z (bit-and z 1)) 2))
        row z]
    {:x col :y row}))

(defn cube-rotate-cw [[x y z]]
  [(- z) 
   (- x) 
   (- y)])

(defn cube-rotate-ccw [[x y z]]
  [(- y) 
   (- z) 
   (- x)])

(defn rotate-around [pivot cell rotator]
  (let [pc (grid->cube pivot)
        cc (grid->cube cell)]
    (cube->grid-even-row (map + pc (rotator (map - cc pc))))))


;;;;;;;;;;;;;; movements of units ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-offset [{:keys [members pivot] :as unit} offset]
  (-> unit
    (update :pivot plus offset)
    (update :members (partial mapv (partial plus offset)))))

(defn east [unit]
  (add-offset unit {:x 1 :y 0}))

(defn west [unit]
  (add-offset unit {:x -1 :y 0}))

(defn south-east [unit]
  (add-offset unit {:x 1 :y 1}))

(defn south-west [unit]
  (add-offset unit {:x -1 :y 1}))

(defn rotate-cw [{:keys [members pivot] :as unit}]
  (let [pivot-cube (grid->cube pivot)]
    (assoc unit :members (for [cell members] (rotate-around pivot cell cube-rotate-cw)))))

(defn rotate-ccw [{:keys [members pivot] :as unit}]
  (let [pivot-cube (grid->cube pivot)]
    (assoc unit :members (for [cell members] (rotate-around pivot cell cube-rotate-ccw)))))

;;;;;;;;;;;;;;; commands;;;;;;;;;;;;;;;;;;;;;;

(def W (vec "p'!.03"))
(def E (vec "bcefy2"))
(def SW (vec "aghij4"))
(def SE (vec "lmno 5"))
(def CW (vec "dqrvz1"))
(def CCW (vec "kstuwx"))


;;;;;;;;;;;;;;;;;; simulator ;;;;;;;;;;;;;;;;;;;;;;;;
(defn locking? [board unit]
  (overlaps-anything? board unit))

(defn out-of-bounds? 
  "Does this unit fit within the boundary of the board?"
  [board unit]
  (not (and (every? #(< -1 (:x %) (count (first board))) (:members unit))
           (every? #(< -1 (:y %)) (:members unit)))))

(defn bottom-row-reached? [board unit]
  (let [height (count board)]
    (some #(= height (:y %)) (:members unit))))

(defn create-source 
  "Create units in order of seeds using the deterministic random generator"
  [task seed]
  (->> seed
    random-gen
    (repeatedly (:sourceLength task))
    (map #(mod % (count (:units task))))
    (map #(nth (:units task) %))))

(defn run-one-command [{:keys [board unit source task] :as state} cmd]
  (let [unit' (cmd unit)]
    (cond
      (out-of-bounds? board unit') (reduced :out-of-bounds)
      (or (bottom-row-reached? board unit')
          (locking? board unit')) (let [new-unit (spawn-unit task (first source))] 
                                    (println "locked!")
                                    (if (locking? board new-unit)
                                      (reduced :board-full)
                                      {:board (add-to-board board unit)
                                       :unit new-unit
                                       :task task
                                       :source (rest source)}))
      :else (do 
              (print-board (add-to-board board unit'))
              {:board board
               :unit unit'
               :task task
               :source source}))))

(defn run-commands [task commands seed]
  (let [source (create-source task seed)] 
    (reduce run-one-command {:board (create-board task)
                             :source (rest source)
                             :task task
                             :unit (spawn-unit task (first source))} commands)))

(comment
  (def problems (load-problems))
  (def task (nth problems 2))
  (def b (board task))
  (def units (mapv create-unit (:units task)))
  (doseq [[task-idx task] (map vector (range) problems)]
    (println "Task #" task-idx)
    (println "--------------")
    (dorun(map-indexed (fn [i u] 
                       (println "Unit" i ":")
                       (print-unit u)) (map unit->matrix (:units task)))))
  
  (let [task (rand-nth problems)
        task (first problems)] 
    (run-commands task (repeatedly 20 #(rand-nth [south-east south-west ])) (first (:sourceSeeds task))))
  )