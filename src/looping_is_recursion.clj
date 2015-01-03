(ns looping-is-recursion)

(defn power [base exp]
  (letfn [(helper [exp acc]
                  (cond
                   (zero? exp) 1
                   (= 1 exp) acc
                   :else (recur (dec exp) (* base acc))))]
    (helper exp base)))

(defn last-element [a-seq]
  (letfn [(helper [a-seq acc]
                  (cond
                   (empty? a-seq) acc
                   :else (recur (rest a-seq) (first a-seq))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (or (and (empty? seq1) (empty? seq2))
      (and
       (= (count seq1) (count seq2))
       (= (first seq1) (first seq2))
       (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         index 0]
    (cond
      (empty? acc) nil
      (pred (first acc)) index
      :else (recur (rest acc) (inc index)))))


(defn avg [a-seq]
  (loop [acc a-seq
         len 0
         sum 0]
    (if (empty? acc) (if (zero? len) 0
                       (/ sum len))
      (let [head (first acc)]
        (recur (rest acc) (+ 1 len) (+ head sum))))))

(defn parity [a-seq]
  (loop [eat a-seq
         acc #{}]
    (if (empty? eat) acc
      (recur
        (rest eat)
        (let [head (first eat)]
          (if (contains? acc head) (disj acc head)
            (conj acc head)))))))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (loop [c n
                 n1 0
                 n2 1]
            (if (= 1 c) n2
              (recur (- c 1) n2 (+ n1 n2))))))

(defn cut-at-repetition [a-seq]
  (loop [tail a-seq
         head []
         so-far #{}]
    (if (empty? tail) head
      (let [f (first tail)]
        (if (contains? so-far f) head
          (recur (rest tail) (conj head f) (conj so-far f)))))))


