(ns com.tabuyos.mahjong.application.MahjongApplication)
(require '[com.tabuyos.mahjong.util.Mahjong :as mahjong])
(use '[com.tabuyos.mahjong.util.Moon :only [moon moon1]])
(use '[com.tabuyos.mahjong.util.Container :only [get-instance]])
(import [java.util Calendar])

(defn -main-old [& args]
  (println args)
  (println (:1 mahjong/dot))
  (moon)
  (get-instance)
  (moon1)
  (println (Calendar/getInstance))
  (println (.toUpperCase "tabuyos"))
  (println (. "tabuyos" indexOf "u"))
  (prn "this is prn method, now you saw the message with double quote.")
  (println "hello, tabuyos."))

(defn -main [& _]
  ;(dotimes [_ 2000]
  ;  (mahjong/random-deal)
  ;  )
  (mahjong/app)
  )