(ns io.remote
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(def token "C/1d7grJDHHZEBserpvNAckXRf2+LGVtibuDE8ukD4k=")
(defn load-problems []
  (map (comp json/read-json slurp) (filter #(.isFile %) (file-seq (io/as-file "resources")))))

(defn parse [url]
  (json/read-json (:body (http/get url))))

(defn random-gen [seed]
  (let [last (atom seed)
        mask (bit-shift-left (dec (bit-shift-left 1 (- 31 16))) 16)]
    (fn []
      (let [res (bit-shift-right (bit-and @last mask) 16)]
        (swap! last (fn [v] (mod (+ 12345 (* v 1103515245)) (bit-shift-left 1 32))))
        res))))

;;;;;;;;;;;;;;;; board;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn board [{:keys [width height filled]}]
  (mapv vec
        (for [row (range height)]
         (for [col (range width)]
          (if (some #(and (= col (:x %))
                          (= row (:y %))) filled)
            1
            0)))))

(defn print-board [board]
  (doseq [[i row] (map vector (range) board) :let [odd? (= 1 (mod i 2))]]
    (when odd? (print "  ")) ; two space because my font seems to be proportional :(
    (doseq [cell row]
      (print ([\u2B21 \u2B22] cell) ""))
    (println))
  (println))

;;;;;;;;;;;;;;;; units ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Unit [coords pivot])

(defn unit->matrix [{:keys [members pivot]}]
  (let [max-x (apply max (:x pivot) (map :x members))
        max-y (apply max (:y pivot) (map :y members))
        unit (for [y (range (inc max-y))]
               (for [x (range (inc max-x))
                     :let [coord {:x x :y y}]]
                 (if (some #(= % coord) members)
                   1
                   0)))]    
    (->Unit (mapv vec unit) [(:y pivot) (:x pivot)])))


(defn print-unit [{:keys [coords pivot]}] 
  (doseq [[i row] (map vector (range) coords) 
          :let [odd? (= 1 (mod i 2))]]
    (when odd? (print "  ")) ; two space because my font seems to be proportional :(
    (doseq [[j cell] (map vector (range) row)
            :let [pivot? (= pivot [i j])
                  chars (if pivot? [\u25ef \u25cf] [\u2B21 \u2B22])]]
      (print (chars cell) ""))
    (println)))

(defrecord State [board unit])

(defn spawn-unit 
  "Convert local coordinates into board coordinates
so the unit is centered with its top on the top row of the board.
Returns shifted unit"
  [board {:keys [coords pivot]}]
  (let [board-width (count (first board))
        unit-top-row (first coords)
        unit-width (count unit-top-row)
        offset-left (count (take-while zero? unit-top-row))
        offset-right (count (take-while zero? (reverse unit-top-row)))
        unit-top-width (- unit-width offset-left offset-right)
        offset (- (int (/ (- board-width unit-top-width) 2)) offset-left)
        offset-v (vec (repeat offset 0))]
    (->Unit (mapv (partial into offset-v) coords) 
      (update pivot 1 + offset))))

;;;;;;;;;;;;;;; coordinates ;;;;;;;;;;;;;;;;;;
(defn grid->cube [[col row]]
  (let [x (- col (/ (- row (bit-and row 1)) 2))
        z row
        y (- (- x) z)] 
    [x y z]))

(defn cube->grid-even-row [[x y z]]
  (let [col (+ x (/ (- z (bit-and z 1)) 2))
        row z] 
    [col row]))

(defn cube->grid-odd-row [[x y z]]
  (let [col (+ x (/ (+ z (bit-and z 1)) 2))
        row z]
    [col row]))

(defn rotate-cw [[x y z]]
  [(- z) 
   (- x) 
   (- y)])

(defn rotate-ccw [[x y z]]
  [(- y) 
   (- z) 
   (- x)])

(defn rotate-around [pivot cell rotator]
  (let [pc (grid->cube pivot)
        cc (grid->cube cell)]
    (cube->grid-odd-row (map + pc (rotator (map - cc pc))))))


;;;;;;;;;;;;;;; commands;;;;;;;;;;;;;;;;;;;;;;

(def W (vec "p'!.03"))
(def E (vec "bcefy2"))
(def SW (vec "aghij4"))
(def SE (vec "lmno 5"))
(def CW (vec "dqrvz1"))
(def CCW (vec "kstuwx"))





(comment
  (def problems (load-problems))
  (def task (nth problems 2))
  (def b (board task))
  (doseq [[task-idx task] (map vector (range) problems)]
    (println "Task #" task-idx)
    (println "--------------")
    (dorun(map-indexed (fn [i u] 
                       (println "Unit" i ":")
                       (print-unit u)) (map unit->matrix (:units task)))))
  )