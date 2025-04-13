(ns ^:no-doc antq.cli.table
  "Table support modified from babashka.cli but with multiline cell support"
  (:require
   [clojure.string :as str]))

(defn- str-width
  "Width of `s` when printed, i.e. without ANSI escape codes."
  [s]
  (let [strip-escape-codes #(str/replace %
                                         (re-pattern "(\\x9B|\\x1B\\[)[0-?]*[ -\\/]*[@-~]") "")]
    (count (strip-escape-codes s))))

(defn- pad
  [len s]
  (str s (apply str (repeat (- len (str-width s)) " "))))

(defn- pad-cells
  "Adapted from bb cli"
  [rows widths]
  (let [pad-row (fn [row]
                  (map (fn [width cell] (pad width cell)) widths row))]
    (map pad-row rows)))

(defn- cell-widths
  [rows]
  (reduce
   (fn [widths row]
     (map max (map str-width row) widths)) (repeat 0) rows))

(defn- expand-multiline-cells
  [rows]
  (let [col-cnt (count (first rows))]
    (->> (for [row rows]
           (let [row-lines (mapv str/split-lines row)
                 max-lines-cell (reduce max (mapv count row-lines))]
             (for [line-ndx (range max-lines-cell)]
               (for [col-ndx (range col-cnt)]
                 (get-in row-lines [col-ndx line-ndx] "")))))
         (mapcat identity))))

(defn format-table
  [{:keys [rows]}]
  (let [rows (expand-multiline-cells rows)
        widths (cell-widths rows)
        rows (pad-cells rows widths)]
    (->> rows
         (map (fn [row]
                (str " "
                     (apply str (interpose "  " row)))))
         (map str/trimr)
         (str/join "\n"))))

(comment
  (-> (format-table {:rows [["r1c1\nr1c1 l2" "r1c2" "r1c3"]
                             ["r2c1 wider" "r2c2\nr2c2 l2\nr2c2 l3" "r2c3\nr2c3 l2"]
                             ["r3c1" "r3c2 wider" "r3c3\nr3c3 l2\nr3c3 l3"]]})
      str/split-lines)
  ;; => [" r1c1        r1c2        r1c3"
  ;;     " r1c1 l2"
  ;;     " r2c1 wider  r2c2        r2c3"
  ;;     "             r2c2 l2     r2c3 l2"
  ;;     "             r2c2 l3"
  ;;     " r3c1        r3c2 wider  r3c3"
  ;;     "                         r3c3 l2"
  ;;     "                         r3c3 l3"]

  :eoc)
