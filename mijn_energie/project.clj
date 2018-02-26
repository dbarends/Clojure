(defproject mijn_energie "0.1.0-SNAPSHOT"
  :description "Applicatie waarin de gegevens van mijnVoedingscentrum grafisch worden gepresenteerd"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.7"]]
  :main ^:skip-aot mijn-energie.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
