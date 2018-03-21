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


;; alternatief -----------------------------------------------------------------
;;
;; TODO: return the sum for the whole day
;;
(defn mijnVoedingsstof
  [x stof]
  (->> x
       zip/xml-zip
       zip/down
       zip/down
       zip/rightmost
       zip/children
       (map (fn [x] [(:tag x) (:content x)]))
       (into {})
       stof
       ))


;; Nog een alternatief -------------------------------------------------------

;; (def dckxml (xml/parse "/Users/dickbarends/Desktop/voedsel.xml"))
;; (def dckzip (zip/xml-zip dckxml))

(defn dckDagVoedStofHoev
  "Bereken voor een Dag de VoedingsStof (supplement) vector"
  [dckxml supplement]
  (cond
    (zip/end? dckxml) []
    (= (:tag (zip/node dckxml)) supplement) (cons (read-string (first (:content (zip/node dckxml))))
                                                  (dckDagVoedStofHoev (zip/next dckxml) supplement)) 
    :else (dckDagVoedStofHoev (zip/next dckxml) supplement)))

;; Vector met supplementen

(def supplementen [:Energie :Vet :VerzadigdVet :Eiwit :Vezels :Zout :Alcohol :Water :Natrium :Calcium
                   :Magnesium :IJzer :Selenium :Zink :VitamineA :VitamineD :VitamineE :VitamineB1
                   :VitamineB2 :VitamineB6 :Foliumzuur :VitamineB12 :Nicotinezuur :VitamineC :Jodium])

;; Dagelijkse voedselstoffen hoeveelheid

(defn dckJouw Dickefn dckJouw DickDagVoedStoffenHoev
  "Bereken voor een Dag de VoedingStoffen Hoeveelheid"
  [bestand]
  (map #(reduce + 0 %)
       (map #(dckDagVoedStofHoev (zip/xml-zip (xml/parse bestand)) %)
            supplementen)))

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

(defn dckVoedStoffenMatrix
  [bestanden]
  (for [x bestanden]
    (dckDagVoedStoffenHoev x)))

(def dckTijdmsec (first (energieVectorVoorPlot bestanden)))
(def dckMatrix (conj (clojure.core.matrix/transpose (dckVoedStoffenMatrix bestanden))
                     dckTijdmsec))

(def dckMatrixCompleet
  (into {} (mapv (fn [x y] [x y]) (conj supplementen :tijd) dckMatrix)))

;; Maak de graph --------------------------------------------------------------- 
(defn dck-timeSeriesPlot
  "     Input     ->    output
       [[x][y]]   ->   JChart object"
  [data]
  (c/time-series-plot
   (:tijd data)
   (:Energie data)
   :title "Dick Barends Voeding\nEnergie opname"
   :series-label "Energie"
   :legend true
   :x-label "datum"
   :y-label "Energie opname [kcal]"))


(def plot (dck-timeSeriesPlot dckMatrixCompleet))

(c/add-lines plot  (:tijd dckMatrixCompleet)
             (:Eiwit dckMatrixCompleet)
             :series-label :Eiwit)

(i/view (c/set-theme plot :light))
