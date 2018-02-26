(ns mijn-energie.core
  (:gen-class))

(require '[clojure.xml :as xml]
         '[incanter.core :as i]
         '[incanter.charts :as c]
         '[clojure.zip :as zip]
         '[clj-time.core :refer [date-time]]
         'incanter.datasets)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;(def data (xml/parse "/users/dickbarends/desktop/consumpties.xml"))

;;------------------------------------------------------------------------------
;; De functie die de energie hoeveelheid, totaal, berekend voor een dag.
;;
;; Deze functie moet nog verder worden geoptimaliseerd. Zoals die nu is is
;; hij compleet onleesbaar
;;
;;          input            ->          output
;; output van parse commando -> waarde van energie element
;;
;;------------------------------------------------------------------------------
(defn mijnEnergie
   [x]
   [(:Van (:attrs x)) 
    (reduce + 0
            (map read-string
                 (flatten
                  (map :content
                       (map (fn [x] (get x 0))
                            (map :content
                                 (map (fn [x] (get x 2))
                                      (map :content
                                           (:content x)))))))))
    ])


;(def bestanden (mapv str (filter #(.isFile %) (file-seq (clojure.java.io/file "/users/dickbarends/Desktop/MijnVoeding")))))

(def bestanden
  (filter #(clojure.string/ends-with? % "xml" )
           (mapv str
                 (filter #(.isFile %)
                          (file-seq
                               (clojure.java.io/file "/users/dickbarends/Desktop/MijnVoeding"))))))

(defn energieVector
  [bestanden]
  (for [x bestanden]
    (mijnEnergie (xml/parse x))))

;;------------------------------------------------------------------------------
(defn datumConversie
  "Conversie van string dd-mm-yyyy naar miliseconden sinds 1970.
   dit is nog voor de time-series-plot (JFreeChart object)

   string -> int"
  [dck-datum]
  (.getMillis (date-time (Integer/parseInt (subs dck-datum 6 10))
             (Integer/parseInt (subs dck-datum 3 5 ))
             (Integer/parseInt (subs dck-datum 0 2 )))))

(defn energieVectorVoorPlot
  [bestanden]
  (let [ev (clojure.core.matrix/transpose (into [] (energieVector bestanden)))]
    [(mapv datumConversie (first ev))
     (second ev)]))

;; Maak de graph --------------------------------------------------------------- 
(defn dck-timeSeriesPlot
  "     Input     ->    output
       [[x][y]]   ->   JChart object"
  [data]
  (c/time-series-plot
   (first data)
   (second data)
   :title "Dick Barends Voeding\nEnergie opname"
   :x-label "datum"
   :y-label "Energie opname [kcal]"))


(def plot (dck-timeSeriesPlot (energieVectorVoorPlot bestanden)))

(i/view (c/set-theme plot :light))
