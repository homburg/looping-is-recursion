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
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

