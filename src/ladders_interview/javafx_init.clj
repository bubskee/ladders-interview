(ns ladders-interview.javafx-init
  (:require [fn-fx-ui.core :as core])
  (:gen-class
    :extends javafx.application.Application))

(defn -start [app stage]
  (core/start {:root-stage? false}))

(defn -main [& args]
  (javafx.application.Application/launch ladders_interview.javafx_init
                                         (into-array String args)))
