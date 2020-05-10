;;; jacket.el --- Major mode for J  -*- lexical-binding: t -*-

(defun the-NuVoc ()
  (read (format "(progn %s)"
                (with-temp-buffer
                  (insert-file-contents "data/j.sexp")
                  (buffer-string)))))

(load "~/code/jdoc/NuVoc.el")

(defun j-find-thing (thing)
  "Find information about thing (exact match)"
  (interactive "sthing: ")
  (seq-find #'(lambda (jentity)
                (member thing (cdadr jentity)))
            j-nuvoc))

(defun j-urls (thing)
  "Look up urls related to a thing (exact match)"
  (let ((entity (j-find-thing thing)))
    (if entity
        (seq-map #'(lambda (info)
                     ;; guaranteed fields
                     (cons (cadr (assoc 'description (cdr info)))
                           (cdr (assoc 'url (cdr info)))))
                 (seq-filter #'(lambda (kv)
                                 (equal (car kv) 'info))
                             (cdr entity)))
      'thing-not-found)))

(defun j-names (thing)
  "Look up english names for thing"
  (seq-map #'car (j-urls thing)))

(j-urls "~.")
(j-names "%.")

