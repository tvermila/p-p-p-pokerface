(ns p-p-p-pokerface)


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
    (= (apply max values) amount)))

(defn high-card? [hand]
  (let [values (map rank hand)]
    (apply max values)))

(defn pair? [hand]
  (amount-of-a-kind 2 hand))


(defn three-of-a-kind? [hand]
  (amount-of-a-kind 3 hand))


(defn four-of-a-kind? [hand]
  (amount-of-a-kind 4 hand))


(defn flush? [hand]
  (let [cards (amount-of-suits hand)]
    (= (apply max cards) 5)))


(defn full-house? [hand]
  (let [values (amount-of-values hand)]
    (and (= (apply max values) 3) (= (apply min values) 2))))

(defn two-pairs? [hand]
  (let [values (amount-of-values hand)]
    (or (= (first values) (second values) 2)
        (= (first values) 4))))

(defn contains-ace? [hand]
  (let [values (map rank hand)] 
    (= (apply max values) 14)))


(defn straight? [hand]
  (let [sorted-values (sort (map rank hand))
        min-val (first sorted-values)
        max-val (last sorted-values)
        min-val-ace 1
        max-val-ace (nth sorted-values 3)]
    (cond
      (contains-ace? hand)
      (or (= (- max-val-ace min-val-ace) 4) (= (- max-val min-val) 4))
      :else (= (- max-val min-val) 4))))

(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(straight? ["2H" "3H" "3D" "4H" "6H"])

(straight? straight-hand)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))


