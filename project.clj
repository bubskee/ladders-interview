(defproject ladders-interview "0.1.0-SNAPSHOT"
  :description "this app is a tool for skills-practice, steal code at your own risk!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.async "0.3.442"]
                 [medley "0.8.4"]]
  :main ladders-interview.javafx-init
  :aot [ladders-interview.javafx-init]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

