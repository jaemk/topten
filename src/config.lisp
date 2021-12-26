(defpackage topten.config
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :value
    :*values*
    :load-values))
(in-package :topten.config)
(named-readtables:in-readtable :interpol-syntax)


(defun s->log-level (s)
  (alexandria:eswitch (s :test #'equal)
    ("TRACE" :trace)
    ("DEBUG" :debug)
    ("INFO" :info)
    ("WARN" :warn)
    ("ERROR" :error)
    ("FATAL" :fatal)))


(defun env (var &key (default nil) (required nil))
  (or
    (->
      (some->
        (sb-ext:posix-getenv var)
        (topten.utils:trim-to-nil))
      ((lambda (v)
         (if (and required (not v))
           (error (format nil "env var is nil for required var ~a" var))
           v))))
    default))


(defun parse-boolean (s)
  (bind ((b (some-> s str:trim string-downcase)))
    (or
      (equal b "true")
      (equal b "t"))))


(defmacro get-version ()
  (when (uiop:probe-file* "commit_hash.txt")
    (->
      (str:from-file "commit_hash.txt")
      (str:trim)
      ((lambda (s)
         (format t "~&WARNING: loaded commit hash: ~a - should only be at build time!~%" s)
         s)))))


(defvar *values* nil)

(defun reset-values ()
  (setf *values* nil))

(defun print-values ()
  (maphash (lambda (k v) (print (list k v))) *values*))

(defun load-values ()
  (cl-dotenv:load-env ".env")
  (->
    (list
      (list :log-level
        (some-> (env "LOG_LEVEL" :default "info") (string-upcase) (s->log-level)))
      (list :version
        (get-version))
      (list :ranking-file
        (env "RANKING_FILE" :default "local/ranking.txt"))
      )
    ((lambda (vals)
       (setf *values* (make-hash-table))
       (loop for (k v) in vals do
         (setf (gethash k *values*) v))))))


(defun value (key)
  (when (null *values*)
    (load-values))
  (gethash key *values*))

