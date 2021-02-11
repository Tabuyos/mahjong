(ns com.tabuyos.mahjong.util.Mahjong)
(import java.util.Random)

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
(def weight 0.5)

(def wall {})

(def current ())

(def rnd
  (Random.))

(defn rnd-int
  "random int."
  [len]
  (.nextInt rnd len))

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
      (get-sequence pool)
      (get-pong pool)
      )
    )
  (get-jong pool)
  ;(println (shuffle current))
  ;(println (sort current))
  (println current)
  )

(defn random-deal
  "deal wall to somebody after shuffle."
  []
  (let [pool (get-wall true)]
    (deal0 pool)
    )
  )

(defn one-sequence-deal
  "get one sequence deal."
  []
  )

(defn two-sequence-deal
  "get two sequence deal."
  []
  )

(defn three-sequence-deal
  "get three sequence deal."
  []
  )

(defn four-sequence-deal
  "get four sequence deal."
  []
  )
