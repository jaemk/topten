(defpackage topten
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main
    :run-main))
(in-package :topten)
(named-readtables:in-readtable :interpol-syntax)


(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar (lambda (line)
              (bind ((line (->> (str:split "|" line) (mapcar #'str:trim)))
                     (person (first line))
                     (album-artists (rest line)))
                (list
                  person
                  (loop for aa in album-artists
                        for score from 1
                        collect (list
                                  (mapcar
                                    (lambda (s)
                                      (-> (str:trim s) (string-downcase)))
                                    (str:split "-" aa))
                                  score))))))))

(defun input (filepath)
  (->>
    (str:from-file filepath)
    (parse)))

(defun sort-hash (m)
  (->
    (topten.utils:hashtable->list m)
    (sort (lambda (a b) (bind (((_ score-a) a)
                               ((_ score-b) b))
                          (> score-a score-b))))))

(defun rank-all (rankings)
  (bind ((scored-albums (make-hash-table :test #'equal))
         (scored-artists (make-hash-table :test #'equal))
         (scored-artists-count (make-hash-table :test #'equal))
         )
    (loop for (person p-rankings) in rankings do
          (loop for ((album artist) score) in p-rankings do
                (progn
                  (if (gethash album scored-albums)
                    (incf (gethash album scored-albums) score)
                    (setf (gethash album scored-albums) score))
                  (if (gethash artist scored-artists-count)
                    (incf (gethash artist scored-artists-count))
                    (setf (gethash artist scored-artists-count) 1))
                  (if (gethash artist scored-artists)
                    (incf (gethash artist scored-artists) score)
                    (setf (gethash artist scored-artists) score)))))
    (values
      (list
        (sort-hash scored-albums)
        (sort-hash scored-artists-count)
        (sort-hash scored-artists))
      (list
        scored-albums
        scored-artists-count
        scored-artists))))

(defun rank-people-by-album (rankings scored-albums)
  (bind ((scored-people (make-hash-table :test #'equal)))
    (loop for (person p-rankings) in rankings do
          (loop for ((album artist) score) in p-rankings do
                (bind ((album-score (gethash album scored-albums)))
                  (if (gethash person scored-people)
                    (incf (gethash person scored-people) album-score)
                    (setf (gethash person scored-people) album-score)
                    ))))
    (values
      (sort-hash scored-people)
      scored-people)))

(defun run-main ()
  (log:info "ranking from: ~a" (topten.config:value :ranking-file))
  (bind ((rankings (input (topten.config:value :ranking-file)))
         ((:values sorted hashed) (rank-all rankings))
         ((all-albums all-artists-count all-artists) sorted)
         ((hashed-albums hashed-artists-count hashed-artists) hashed)
         (people-by-album (rank-people-by-album rankings hashed-albums)))
    (format t "~&===== RANK: ALBUMS =====~%~{~{~a: ~a~}~&~}~%" all-albums)
    (format t "~&===== RANK: ARTISTS BY ALBUM SCORE =====~%~{~{~a: ~a~}~&~}~%" all-artists)
    (format t "~&===== RANK: ARTISTS BY ALBUM COUNT =====~%~{~{~a: ~a~}~&~}~%" all-artists-count)
    (format t "~&===== RANK PEOPLE BY ALBUM SCORE =====~%~{~{~a: ~a~}~&~}~%" people-by-album)))


(defun main (argvs)
  ;; handle any errors if they aren't caught by the catch-all handler in 'main
  (setf
    *debugger-hook*
    (lambda (c old-hook)
      (declare (ignore old-hook))
      (format *error-output* "~&Unhandled error: ~a~%" (topten.utils:get-error-backtrace c))
      (sb-ext:quit :unix-status 1)))

  (handler-bind
    (
      ;; C-c
      (sb-sys:interactive-interrupt
        (lambda (c)
          (format t "~&Aborting...~%")
          (sb-ext:quit :unix-status 1)))

      ;; everything else
      (error
        (lambda (c)
          (format *error-output* "~&Error: ~a~%" (topten.utils:get-error-backtrace c))
          (sb-ext:quit :unix-status 1)))
    )
      (progn
        (log:config (topten.config:value :log-level))
        (log:config :sane2)
        (log:config :nofile)
        (log:debug "args: ~a" argvs)
        (run-main))))

