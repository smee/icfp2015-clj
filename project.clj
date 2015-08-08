(defproject hexa-tetris-clj "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.8.0-alpha4"]
                 [org.clojars.smee/common "1.2.9-SNAPSHOT"]
                 [org.clojure/data.json "0.1.2"]
                  [clj-http "0.7.4"]]
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :jvm-opts ["-Xmx1g"]
  :profiles {:dev {:dependencies [[incanter/incanter-core "1.5.5"]
                                  [incanter/incanter-charts "1.5.5"
                                   :exclusions [incanter/jfreechart]]
                                  [chart-utils "1.1.0-SNAPSHOT"
                                   :exclusions [[org.jzy3d/jzy3d-api]
                                                [org.jzy3d/jzy3d-jdt-core]]]]}})
