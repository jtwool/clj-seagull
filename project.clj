(defproject clj-seagull "0.1.0"
  :description "Seagull: A seeded graph classifier for lexical communities."
  :url "http://jtwolohan.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aysylu/loom "1.0.2"]]
  :main ^:skip-aot clj-seagull.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
