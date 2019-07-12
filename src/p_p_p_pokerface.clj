(ns p-p-p-pokerface)

;;;;;;;;;;;;;;;;;;; HANDS FOR TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;

(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rank [card]
  (let [[rank suit] card] 
    (if (Character/isDigit rank) 
      (Integer/valueOf (str rank))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank))))
    

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn amount-of-values [hand]
  (vals (frequencies (map rank hand))))

(defn amount-of-suits [hand]
  (vals (frequencies (map suit hand))))

(defn amount-of-a-kind [amount hand]
  (let [values (amount-of-values hand)]
    (if (= (apply max values) amount)
      true
      false)))


(defn pair? [hand]
  (amount-of-a-kind 2 hand))


(defn three-of-a-kind? [hand]
  (amount-of-a-kind 3 hand))


(defn four-of-a-kind? [hand]
  (amount-of-a-kind 4 hand))


(defn flush? [hand]
  (let [values (amount-of-suits hand)]
    (if (= (apply max values) 5)
      true
      false)))


(defn full-house? [hand]
  (let [values (amount-of-values hand)]
    (if (and (= (apply max values) 3) (= (apply min values) 2))
      true
      false)))

(defn two-pairs? [hand]
  (amount-of-a-kind 2 hand))

;;;;;;;;;;;;;

(two-pairs? two-pairs-hand)      ;=> true
(two-pairs? pair-hand)           ;=> false
(two-pairs? four-of-a-kind-hand) ;=> true

;;;;;;;;;;;;;;

(defn straight? [hand]
  (let [values (str (map rank hand))]
    (values)))

;;;;;;;;;;

(straight? two-pairs-hand)             ;=> false
(straight? straight-hand)              ;=> true
(straight? low-ace-straight-hand)      ;=> true
(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
(straight? high-ace-straight-hand)     ;=> true

;;;;;;;;;;

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
