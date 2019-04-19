(defproject sketch "0.1.0-SNAPSHOT"
  :description "animated fibonacci spiral"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "3.0.0"]]
  :main sketch.core
  :aot [sketch.core])
