(defpackage topten/tests
  (:use :cl
        :metabang-bind
        :arrow-macros
        :fiveam
        :topten)
  (:export :all))
(in-package :topten/tests)

(def-suite all
  :description "Tests")

(setf fiveam:*on-failure* :backtrace)
(setf fiveam:*on-error* :backtrace)

(log:config (topten.config:value :log-level))
(log:config :sane2)
(log:config :nofile)

