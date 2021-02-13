(ns com.tabuyos.mahjong.util.Mahjong)
(import java.util.Random)
(use '[clojure.string :only (join split)])

;; 筒
(def dot {:1 [1 1 1 1], :2 [2 2 2 2], :3 [3 3 3 3], :4 [4 4 4 4], :5 [5 5 5 5], :6 [6 6 6 6], :7 [7 7 7 7], :8 [8 8 8 8], :9 [9 9 9 9]})
;; 条
(def bam {:1 [1 1 1 1], :2 [2 2 2 2], :3 [3 3 3 3], :4 [4 4 4 4], :5 [5 5 5 5], :6 [6 6 6 6], :7 [7 7 7 7], :8 [8 8 8 8], :9 [9 9 9 9]})
;; 万
(def cha {:1 [1 1 1 1], :2 [2 2 2 2], :3 [3 3 3 3], :4 [4 4 4 4], :5 [5 5 5 5], :6 [6 6 6 6], :7 [7 7 7 7], :8 [8 8 8 8], :9 [9 9 9 9]})
;; 牌池
(def pool {:dot dot, :bam bam, :cha cha})
;; 花色
(def stl (keys pool))
;; 手牌的权重
(def weight 1)

(def wall {})

(def current ())

(def result ())

(def all ())

(def temp ())

(def rnd
  (Random.))

(defn rnd-int
  "random int."
  ([]
   (rnd-int Integer/MAX_VALUE))
  ([len]
   (.nextInt rnd len))
  )

(defn get-with-lack
  "get lack brand."
  []
  (let [len (count pool), st (nth stl (rnd-int len))]
    (dissoc pool st)
    )
  )

(defn get-one-suit
  "get one suit in pool."
  []
  (let [st (nth stl (rnd-int (count pool)))]
    (st pool)
    )
  )

(defn get-wall
  "get wall in pool."
  [flag]
  (if flag
    (get-one-suit)
    (get-with-lack))
  )

(defn in?
  "true if coll contains elm"
  [coll elm]
  (contains? (set coll) elm))

;; 麻将实际上是由四个搭子+一个将牌所组成的
;; 搭子又分: 碰牌, 杠牌, 连牌(顺子)
;; 胡牌不能胡在碰牌或者杠牌(当然不能了!)上, 即只能胡在手牌中
;; 因此我们抽牌时只能在抽手牌中的某一张
;; 0: 顺 1: 碰

(defn go-left-with-nine
  "go to left."
  [wl, ik]
  (def wall wl)
  (let [first ik, ck1 (keyword (str first)), cv1 (ck1 wl)]
    (if (< (count cv1) 1)
      (go-left-with-nine wl (- first 1))
      (let [second (- first 1), ck2 (keyword (str second)), cv2 (ck2 wl)]
        (if (< (count cv2) 1)
          (go-left-with-nine wl (- second 1))
          (let [third (- second 1), ck3 (keyword (str third)), cv3 (ck3 wl)]
            (if (< (count cv3) 1)
              (go-left-with-nine wl (- third 1))
              (do
                (def current (conj current first))
                (def current (conj current second))
                (def current (conj current third))
                (def wall (assoc wall ck1 (subvec cv1 1)))
                (def wall (assoc wall ck2 (subvec cv2 1)))
                (def wall (assoc wall ck3 (subvec cv3 1)))
                ))
            ))
        )
      )
    )
  )

(defn go-right-with-one
  "go to right."
  [wl, ik]
  (def wall wl)
  (let [first ik, ck1 (keyword (str first)), cv1 (ck1 wl)]
    (if (< (count cv1) 1)
      (go-right-with-one wl (+ first 1))
      (let [second (+ first 1), ck2 (keyword (str second)), cv2 (ck2 wl)]
        (if (< (count cv2) 1)
          (go-right-with-one wl (+ second 1))
          (let [third (+ second 1), ck3 (keyword (str third)), cv3 (ck3 wl)]
            (if (< (count cv3) 1)
              (go-right-with-one wl (+ third 1))
              (do
                (def current (conj current first))
                (def current (conj current second))
                (def current (conj current third))
                (def wall (assoc wall ck1 (subvec cv1 1)))
                (def wall (assoc wall ck2 (subvec cv2 1)))
                (def wall (assoc wall ck3 (subvec cv3 1)))
                ))
            ))
        )
      )
    )
  )

(defn go-center
  "go to center."
  [wl, ik]
  (def wall wl)
  (if (< ik 2)
    (go-left-with-nine wl 9)
    (let [first ik, ck1 (keyword (str first)), cv1 (ck1 wl)]
      (if (< (count cv1) 1)
        (go-center wl (- first 2))
        (let [second (- first 1), ck2 (keyword (str second)), cv2 (ck2 wl)]
          (if (< (count cv2) 1)
            (go-center wl (- second 2))
            (let [third (+ first 1), ck3 (keyword (str third)), cv3 (ck3 wl)]
              (if (< (count cv3) 1)
                (go-center wl (- third 2))
                (do
                  (def current (conj current second))
                  (def current (conj current first))
                  (def current (conj current third))
                  (def wall (assoc wall ck1 (subvec cv1 1)))
                  (def wall (assoc wall ck2 (subvec cv2 1)))
                  (def wall (assoc wall ck3 (subvec cv3 1)))
                  ))
              ))
          )
        )
      )
    )
  )

(defn get-pong
  "pong"
  [wl]
  (def wall wl)
  (let [cl (count wl),
        index (rnd-int cl),
        ck (nth (keys wl) index),
        cv (ck wl)]
    (if (< (count cv) 3)
      (get-pong wl)
      (do (dotimes [index 3]
            (def current (conj current (get cv index)))
            )
          (def wall (assoc wall ck (subvec cv 3))))
      )
    )
  )

(defn get-kong
  "kong"
  [wl]
  (println wl)
  )

(defn get-sequence0
  "get a sequence"
  [wl ik]
  (case ik
    1 (go-right-with-one wl 1)
    9 (go-left-with-nine wl 9)
    (go-center wl ik)
    )
  )

(defn get-sequence
  "sequence"
  [wl]
  (let [cl (count wl),
        index (rnd-int cl),
        ck (nth (keys wl) index),
        cv (ck wl)]
    (case (get cv 0)
      1 (go-right-with-one wl 1)
      9 (go-left-with-nine wl 9)
      (go-center wl (get cv 0))
      )
    )
  )

(defn get-jong
  "jong in mahjong."
  [wl]
  (def wall wl)
  (let [cl (count wl),
        index (rnd-int cl),
        ck (nth (keys wl) index),
        cv (ck wl)]
    (if (< (count cv) 2)
      (get-jong wl)
      (do
        (def current (conj current (get cv 0)))
        (def current (conj current (get cv 0)))
        (def wall (assoc wall ck (subvec cv 2)))
        ))
    )
  )

(defn deal0
  "origin deal."
  [pool]
  (def wall pool)
  (dotimes [_ 4]
    (if (< (/ (rnd-int 10) 10) weight)
      (get-sequence wall)
      (get-pong wall)
      )
    )
  (get-jong wall)
  ;(println (shuffle current))
  ;(println (sort current))
  (println current)
  (println wall)
  )

(defn random-deal
  "deal wall to somebody after shuffle."
  []
  (let [pool (get-wall true)]
    (deal0 pool)
    (def current ())
    (def wall {})
    )
  )

(defn get-jong0
  "jong of mahjong."
  [wl ik]
  (def wall wl)
  (if (> ik 9)
    (get-jong0 wl 1)
    (let [ck (keyword (str ik)), cv (ck wl)]
      (if (< (count cv) 2)
        (get-jong0 wl (inc ik))
        (do
          (def current (conj current (get cv 0)))
          (def current (conj current (get cv 0)))
          (def wall (assoc wall ck (subvec cv 2)))
          ))
      )
    )
  )

(defn get-pong0
  "pong"
  [wl ik]
  (def wall wl)
  (if (< ik 10)
    (let [ck (keyword (str ik)), cv (ck wl)]
      (if (< (count cv) 3)
        (get-pong0 wl (inc ik))
        (do
          (dotimes [index 3]
            (def current (conj current (get cv index)))
            )
          (def wall (assoc wall ck (subvec cv 3)))
          )
        )
      )
    )
  )

(defn print-result
  "print result item."
  []
  (dotimes [index (count result)]
    (println (nth result index))
    )
  )

(defn rm
  "remove from list"
  [coll index]
  (if (and (> index -1) (< index (count coll)))
    (let [temp (atom (list))]
      (dotimes [it (count coll)]
        (if (not (= it index))
          (reset! temp (conj @temp (nth coll it)))
          )
        )
      (reverse @temp)
      )
    )
  )

(defn write-to-file
  "write to file."
  ([filename]
   (write-to-file filename result))
  ([filename coll]
   (with-open [wr (clojure.java.io/writer (str filename ".tabuyos") :append false)]
     (dotimes [index (count coll)]
       (let [it (nth coll index)]
         (dotimes [ik (count it)]
           (.write wr (str (sort (rm it ik)) "|" (nth it ik) "\n"))
           )
         )
       )
     ))
  )

(defn integrate
  "integrate with conj"
  [coll line]
  (let [sp (split line #"\|")]
    (conj coll {:wall (nth sp 0) :ready (list (nth sp 1))})
    )
  )

(defn into-all
  "insert into all."
  [srt]
  (let [first (nth srt 0), len (count srt) wl (atom (:wall first)), ry (atom ())]
    (dotimes [i len]
      (let [cr (nth srt i)]
        (if (= @wl (:wall cr))
          (do
            (compare-and-set! ry @ry (conj @ry (Integer/parseInt (nth (:ready cr) 0))))
            )
          (do
            (def all (conj all {:wall @wl, :ready @ry}))
            (compare-and-set! wl @wl (:wall cr))
            (compare-and-set! ry @ry (conj () (Integer/parseInt (nth (:ready cr) 0))))
            )
          )
        )
      )
    )
  )

(defn read-from-file
  "read from file."
  [filename]
  (with-open [rr (clojure.java.io/reader (str filename ".tabuyos"))]
    (reduce integrate () (line-seq rr))
    )
  )

(defn zero-sequence-deal
  "get zero sequence deal."
  []
  (dotimes [i 9]
    (dotimes [j 9]
      (dotimes [k 8]
        (dotimes [l 7]
          (dotimes [m 6]
            (def wall (get-wall true))
            (get-jong0 wall (+ i 1))
            (get-pong0 wall (+ j 1))
            (get-pong0 wall (+ j k 2))
            (get-pong0 wall (+ j k l 3))
            (get-pong0 wall (+ j k l m 4))
            (if (= (count current) 14)
              (do
                (if (not (in? result current))
                  (def result (conj result current))
                  )
                )
              )
            (def current ())
            )
          )
        )
      )
    )
  (println (count result))
  (write-to-file "zero")
  (def result ())
  )

(defn one-sequence-deal
  "get one sequence deal."
  []
  (dotimes [i 9]
    (dotimes [j 9]
      (if (and (> j 1) (< j 9))
        (dotimes [k 9]
          (dotimes [l 8]
            (dotimes [m 7]
              (def wall (get-wall true))
              (get-jong0 wall (+ i 1))
              (get-sequence0 wall j)
              (get-pong0 wall (+ k 1))
              (get-pong0 wall (+ k l 2))
              (get-pong0 wall (+ k l m 3))
              (if (= (count current) 14)
                (do
                  (if (not (in? result current))
                    (def result (conj result current))
                    )
                  )
                )
              (def current ())
              )
            )
          )
        )
      )
    )
  (println (count result))
  (write-to-file "one")
  (def result ())
  )

(defn two-sequence-deal
  "get two sequence deal."
  []
  (dotimes [i 9]
    (dotimes [j 9]
      (if (and (> j 1) (< j 9))
        (dotimes [k 9]
          (if (and (> k 1) (< k 9))
            (dotimes [l 9]
              (dotimes [m 8]
                (def wall (get-wall true))
                (get-jong0 wall (+ i 1))
                (get-sequence0 wall j)
                (get-sequence0 wall k)
                (get-pong0 wall (+ l 1))
                (get-pong0 wall (+ l m 2))
                (if (= (count current) 14)
                  (do
                    (if (not (in? result current))
                      (def result (conj result current))
                      )
                    )
                  )
                (def current ())
                )
              )
            )
          )
        )
      )
    )
  (println (count result))
  (write-to-file "two")
  (def result ())
  )

(defn three-sequence-deal
  "get three sequence deal."
  []
  (dotimes [i 9]
    (dotimes [j 9]
      (if (and (> j 1) (< j 9))
        (dotimes [k 9]
          (if (and (> k 1) (< k 9))
            (dotimes [l 9]
              (if (and (> l 1) (< l 9))
                (dotimes [m 9]
                  (def wall (get-wall true))
                  (get-jong0 wall (+ i 1))
                  (get-sequence0 wall j)
                  (get-sequence0 wall k)
                  (get-sequence0 wall l)
                  (get-pong0 wall (+ m 1))
                  (if (= (count current) 14)
                    (do
                      (if (not (in? result current))
                        (def result (conj result current))
                        )
                      )
                    )
                  (def current ())
                  )
                )
              )
            )
          )
        )
      )
    )
  (println (count result))
  (write-to-file "three")
  (def result ())
  )

(defn four-sequence-deal
  "get four sequence deal."
  []
  (dotimes [i 9]
    (dotimes [j 9]
      (if (and (> j 1) (< j 9))
        (dotimes [k 9]
          (if (and (> k 1) (< k 9))
            (dotimes [l 9]
              (if (and (> l 1) (< l 9))
                (dotimes [m 9]
                  (if (and (> m 1) (< m 9))
                    (do
                      (def wall (get-wall true))
                      (get-jong0 wall (+ i 1))
                      (get-sequence0 wall j)
                      (get-sequence0 wall k)
                      (get-sequence0 wall l)
                      (get-sequence0 wall m)
                      (if (= (count current) 14)
                        (do
                          (if (not (in? result current))
                            (def result (conj result current))
                            )
                          )
                        )
                      (def current ())
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  (println (count result))
  (write-to-file "four")
  (def result ())
  )

(defn write-all
  "persistence for all"
  [filename]
  (with-open [wr (clojure.java.io/writer (str filename ".tabuyos") :append false)]
    (dotimes [index (count all)]
      (let [it (nth all index)]
        (.write wr (str (:wall it) " | " (sort (:ready it)) "\n"))
        )
      )
    )
  )

(defn process
  "application for mahjong."
  []
  ;(zero-sequence-deal)
  ;(one-sequence-deal)
  ;(two-sequence-deal)
  ;(three-sequence-deal)
  ;(four-sequence-deal)

  (def temp (reduce conj temp (distinct (read-from-file "zero"))))
  (def temp (reduce conj temp (distinct (read-from-file "one"))))
  (def temp (reduce conj temp (distinct (read-from-file "two"))))
  (def temp (reduce conj temp (distinct (read-from-file "three"))))
  (def temp (reduce conj temp (distinct (read-from-file "four"))))

  (into-all (sort-by :wall (distinct temp)))
  (write-all "all")
  )

(defn get-input
  "get input"
  [msg]
  (do
    (print msg)
    (flush)
    (read-line)
    )
  )

(defn trim
  "trim string"
  [string]
  (.trim (.toString string))
  )

(defn random-one
  "random one wall from file"
  []
  (if (= (count all) 0)
    (def all (reduce conj all (distinct (read-from-file "all"))))
    )
  (let [in (atom "Y")]
    (while (not (.equalsIgnoreCase @in "N"))
      (let [line (nth all (rnd-int (count all))), wl (trim (:wall line)), ry (trim (nth (:ready line) 0))]
        (println "wall:" wl)
        (if (not (.equalsIgnoreCase (get-input "show(y/enter)?") "N"))
          (println "ready:" ry)
          )
        )
      (println)
      (reset! in (get-input "continue(y/enter)/quit(n)?"))
      )
    )
  )
