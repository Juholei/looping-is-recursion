(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                  acc
                  (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) acc
      :else (recur (inc acc) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         size 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ acc size)
      (recur (+ acc (first a-seq)) (inc size) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         a-seq a-seq]
    (cond
      (empty? a-seq) acc
      :else (recur (toggle acc (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [prev 0
         cur 1
         n n]
    (cond
      (zero? n) prev
      :else (recur cur (+ prev cur) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         return []
         a-seq a-seq]
    (cond
      (empty? a-seq) return
      (contains? seen (first a-seq)) return
      :else (recur (conj seen (first a-seq)) (conj return (first a-seq)) (rest a-seq)))))

