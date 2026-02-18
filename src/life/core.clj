(ns life.core
  (:require
   [hyperfiddle.rcf :as rcf]))

(rcf/enable!)

(defn count-where [f coll]
  (reduce (fn [memo i]
            (if (f i)
              (inc memo)
              memo))
          0
          coll))

(rcf/tests
 (count-where true? [false true false true])
 := 2)

(defn cell-next-state [alive? neighbors]
  (let [live-neighbor-count (count-where true? neighbors)]
    (cond
      ;; Any live cell with fewer than two live neighbours dies, as if by underpopulation.
      (and alive? (< live-neighbor-count 2))
      false

      ;; Any live cell with two or three live neighbours lives on to the next generation.
      (and alive? (<= 2 live-neighbor-count 3))
      true

      ;; Any live cell with more than three live neighbours dies, as if by overpopulation.
      (and alive? (< 3 live-neighbor-count))
      false

      ;; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      (and (not alive?) (= 3 live-neighbor-count))
      true

      :else
      false)))

(rcf/tests
 (cell-next-state true [true true false true false true false true])
 := false

 (cell-next-state true [false false false false false false false false])
 := false)

(defn neighbor-coordinates
  [grid [target-x target-y]]
  (let [width (count (first grid))
        height (count grid)]
    (for [x (range (max (dec target-x) 0) (min (+ 2 target-x) height))
          y (range (max (dec target-y) 0) (min (+ 2 target-y) width))
          :when (not (and (= x target-x)
                          (= y target-y)))]
      [x y])))

(rcf/tests
 (set (neighbor-coordinates [[:a :b :c]
                             [:d :e :f]
                             [:g :h :i]]
                            [1 1]))
 := #{[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]}

 (set (neighbor-coordinates [[:a :b :c]
                             [:d :e :f]
                             [:g :h :i]]
                            [0 0]))
 := #{[0 1] [1 0] [1 1]})

(defn neighbors
  [grid coords]
  (->> (neighbor-coordinates grid coords)
       (map (fn [[x y]]
              (get-in grid [x y])))))

(rcf/tests
 (set (neighbors [[:a :b :c]
                  [:d :e :f]
                  [:g :h :i]]
                 [1 1]))
 := #{:a :b :c :d :f :g :h :i}

 (set (neighbors [[:a :b :c]
                  [:d :e :f]
                  [:g :h :i]]
                 [0 0]))
 := #{:b :d :e})

(defn grid-next-state
  [grid]
  (reduce (fn [new-grid [x y]]
            (update-in new-grid [x y]
                       cell-next-state
                       (neighbors grid [x y])))
          grid
          (for [x (range (count grid))
                y (range (count (first grid)))]
            [x y])))

(rcf/tests
 (grid-next-state [[true true false]
                   [true true false]
                   [false false false]])
 := [[true true false]
     [true true false]
     [false false false]])

(defn render [grid]
  (->> grid
       (map (fn [row]
              (map {true "█" false " "} row)))
       (map (fn [row]
              (str (apply str row) "\n")))
       (apply str)))

(rcf/tests
 (render [[true true false]
          [true true false]
          [false false false]])
 := "██ \n██ \n   \n")

(defn -main []
  (loop [grid (read-string (slurp "grid.edn"))]
    ;; clear screen and move cursor to top-left
    (print "\u001b[2J\u001b[H")
    (print (render grid))
    (flush)
    (Thread/sleep 50)
    (recur (grid-next-state grid))))
