(ns io.remote
  (:require [clojure.data.json :as json]
            [clojure.set :as sets]
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
  (doseq [[i row] (with-index board)]
    (when (= 1 (mod i 2)) (print "  ")) ; two space because my font seems to be proportional :(
    (doseq [cell row]
      (print ([\u2B21 \u2B22 \u26A1] cell) ""))
    (println))
  (println))

(defn count-full-rows [board]
  (count (filter (partial every? #{1}) board)))

(defn clear-full-rows [board]
  (mapv vec
        (reduce (fn [b row]
                  (if (every? #{1} row)
                    (cons (repeat (count row) 0) b)
                    (concat b [row]))) [] board)))

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
  [board {:keys [members pivot] :as unit}]
  (when unit 
    (let [board-width (count (first board))
         coords (unit->matrix unit)
         unit-top-row (first coords)
         unit-width (count unit-top-row)
         offset-left (count (take-while zero? unit-top-row))
         offset-right (count (take-while zero? (reverse unit-top-row)))
         unit-top-width (- unit-width offset-left offset-right)
         offset {:y 0 :x (- (int (/ (- board-width unit-top-width) 2)) offset-left)}]
     {:pivot (plus pivot offset)
      :members (mapv (partial plus offset) members)})))

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
  (let [offsets [{:x 0 :y 1}
                 {:x 1 :y 1}]
        f #(plus % (offsets (mod (:y %) 2)))] 
    (-> unit
      (update :pivot f)
      (update :members (partial mapv f)))))

(defn south-west [unit]
  (let [offsets [{:x -1 :y 1}
                 {:x 0 :y 1}]
        f #(plus % (offsets (mod (:y %) 2)))] 
    (-> unit
      (update :pivot f)
      (update :members (partial mapv f)))))

(defn rotate-cw [{:keys [members pivot] :as unit}]
  (let [pivot-cube (grid->cube pivot)]
    (assoc unit :members (for [cell members] (rotate-around pivot cell cube-rotate-cw)))))

(defn rotate-ccw [{:keys [members pivot] :as unit}]
  (let [pivot-cube (grid->cube pivot)]
    (assoc unit :members (for [cell members] (rotate-around pivot cell cube-rotate-ccw)))))

;;;;;;;;;;;;;;; commands;;;;;;;;;;;;;;;;;;;;;;
(def all-commands {:east east :west west :south-east south-east 
                   :south-west south-west :rotate-cw rotate-cw 
                   :rotate-ccw rotate-ccw})

(defn remove-mini-loops [cmds]
  (map first (remove #{[:east :west]
                       [:west :east]
                       [:rotate-cw :rotate-ccw]
                       [:rotate-ccw :rotate-cw]} (partition 2 1 cmds))))
(def char->command (reduce merge (map (fn [[s cmd]]
                                        (into {} (map #(vector % cmd) s))) 
                                      [["p'!.03" west]
                                       ["bcefy2" east]
                                       ["aghij4" south-west]
                                       ["lmno 5" south-east]
                                       ["dqrvz1" rotate-cw]
                                       ["kstuwx" rotate-ccw]])))
(defn string->commands [s]
  (keep char->command s))


;;;;;;;;;;;;;;;;;; simulator ;;;;;;;;;;;;;;;;;;;;;;;;
(defn locking? [board unit]
  (overlaps-anything? board unit))

(defn out-of-bounds? 
  "Does this unit fit within the boundary of the board?"
  [board unit]
  (not (and (every? #(< -1 (:x %) (count (first board))) (:members unit))
           (every? #(<= 0 (:y %) (count board)) (:members unit)))))

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

(defn- stop [state reason]
  (assoc state :running false :status reason))

(defn run-one-command [{:keys [board unit source seen] :as state} cmd]
  (cond 
    (nil? unit) (stop state :no-more-units)
    (locking? board unit) (stop state :board-full)
    (seen (add-to-board board unit)) (stop state :duplicate-board)
    :else (let [unit' (cmd unit)]
            (cond
              (out-of-bounds? board unit') (stop state :out-of-bounds)
              (or (bottom-row-reached? board unit')
                  (locking? board unit')) (let [board' (add-to-board board unit)
                                                 lines-cleared (count-full-rows board')
                                                 board' (clear-full-rows board')
                                                 unit' (first source)] 
                                             (assoc state 
                                                    :board board'
                                                    :unit (spawn-unit board' unit')
                                                    :source (rest source)
                                                    :lines-cleared (+ lines-cleared (:lines-cleared state))
                                                    :seen #{}))
              :else (-> state 
                     (assoc :unit unit')
                     (update :seen conj (add-to-board board unit)))))))

(defn initial-state [task seed]
  (let [source (create-source task seed)
        board (create-board task)
        unit (spawn-unit board (first source))]
    {:board board
     :source (rest source)
     :unit unit
     :lines-cleared 0
     :status :running
     :seen #{}}))

(defn run-commands 
  "Create sequence of states for each command. Ends if there is an invalid move,
board is full, commands run out, units run out"
  [task seed commands]
  (reductions (fn [state cmd]
                (let [res (run-one-command state (all-commands cmd))]
                  (if (not (= :running (:status res)))
                    (reduced res)
                    res))) 
              (initial-state task seed) commands))



;;;;;;;;;;;;;;;;;; genetic algorithm ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn generate-individual [length]
  (repeatedly length #(rand-nth (keys all-commands))))


(defn fitness [task seed individual]
  (if-let [old-fitness (-> individual meta :fitness)]
    old-fitness
    (let [states (run-commands task seed individual)
        n (count states)
        res (last states)]
    (+ n 
       (* 50 (:lines-cleared res))
       (if (= :no-more-units (:status res)) 500 0)))))

(defn cross-over [a b]
  (let [[a1 a2] (split-at (rand (count a)) a)
        [b1 b2] (split-at (rand (count b)) b)]
    [(concat a1 b2)
     (concat b1 a2)]))

(defn mutate [individual]
  (for [cmd individual]
    (if (< (rand) 0.05)
      (rand-nth (keys all-commands))
      cmd)))

(defn run-generation [task seed population]
  (let [population (map remove-mini-loops population)
        fitnesses (pmap (partial fitness task seed) population)
        sorted (reverse (sort-by first (map vector fitnesses population)))
        best-fitness (ffirst sorted)
        n (count population)
        parents (map #(with-meta (second %) {:fitness (first %)}) (take (int (/ n 3)) sorted))
        children (apply concat (pmap cross-over (shuffle parents) (shuffle parents)))]
    (with-meta (concat parents (map mutate children)) 
      {:best-fitness best-fitness
       :best-individual (first parents)})))
(comment
  (let [seed 0
        n 100
        population (repeatedly n #(generate-individual 1000))
        next-generation (run-generation task seed population)]
    (-> next-generation meta :best-fitness))
  
  
(require '[incanter 
           [core :refer [view]] 
           [charts :as ch]])
(let [seed 0
      n 100
      population (repeatedly n #(generate-individual 1000))] 
  (def results (map meta (iterate (partial run-generation task seed) population))))

(time (view (ch/xy-plot (range) (map :best-fitness (take 500 results)))))
(last (map #(dissoc % :seen) (run-commands task 0 (:best-individual (nth results 500)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;; backtracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rotational-symmetric? [unit]
  true)

(defn- find-applicable-commands [commands unit last-command]
  (sort-by
    {:south-east 0
     :south-west 0
     :east 1
     :west 1
     :rotate-cw 2
     :rotate-ccw 2}
    (if (nil? last-command)
     (keys all-commands)
     (let [cs (set commands)
           cs (condp = last-command
                :west (disj cs :east)
                :east (disj cs :west)
                :rotate-cw (disj cs :rotate-ccw)
                :rotate-ccw (disj cs :rotate-cw)
                cs)]
       (if (rotational-symmetric? unit)
        (disj cs :rotate-cw :rotate-ccw)
        cs)))))

(defn generate-via-backtracking [task seed max-steps]
  (let [cmds (list)
        tried (list #{})
        stack (list ) 
        f (fn find-next-command [state last-cmd n]
            (if (= n max-steps)
              :done
              (some (fn [cmd] ;(println n "trying" cmd)
                      (let [state (run-one-command state (all-commands cmd))]
;                        (println "status:" (:status state))
                        (condp = (:status state)
                          :no-more-units (list cmd) ;done
                          :board-full nil ;done or backtrack? tricky...
                          :duplicate-board nil
                          :out-of-bounds nil
                          (let [res (find-next-command state cmd (inc n))] 
                            (condp = res 
                              nil nil
                              :done (list cmd)
                              (conj res cmd)))))) 
                   (find-applicable-commands (keys all-commands) (:unit state) last-cmd))))]
    (f (initial-state task seed) nil 0)))

(comment
  (def trivial-task (assoc task :width 2 
                           :height 5 
                           :units [{:members [{:x 0, :y 0}],
                                    :pivot {:x 0, :y 0}}]))
  (time (def cmds (generate-via-backtracking task seed 500)))
  (println (count cmds))
  (let [s (last (run-commands task seed cmds))] 
    (println "cleared:" (:lines-cleared s))
    (print-board (add-to-board (:board s) (:unit s))))
  )

(comment
  (def problems (load-problems))
  (def task (nth problems 0))
  (def seed 0)
  (def b (board task))
  (def units (mapv create-unit (:units task)))
  (doseq [[task-idx task] (map vector (range) problems)]
    (println "Task #" task-idx)
    (println "--------------")
    (dorun(map-indexed (fn [i u] 
                       (println "Unit" i ":")
                       (print-unit u)) (map unit->matrix (:units task)))))
  
  (let [task (rand-nth problems)
        task (first problems)
        seed (first (:sourceSeeds task))
        seed 0] 
    (last (run-commands (assoc task :sourceLength 5) seed (repeatedly #(rand-nth [:south-east :south-west ])))))
  ;; from video
  (def task (parse "http://icfpcontest.org/problems/problem_6.json"))
  (let [
        seed 0
        commands (string->commands "iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeeemmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiiipiimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm")] 
    (map (comp print-board :board) (butlast (take-last 5 (run-commands task commands seed)))))
  )