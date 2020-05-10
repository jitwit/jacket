;;; jacket.el --- Major mode for J  -*- lexical-binding: t -*-

(defun the-NuVoc ()
  (read (format "(progn %s)"
                (with-temp-buffer
                  (insert-file-contents "data/j.sexp")
                  (buffer-string)))))

(require 'NuVoc "~/code/jacket/NuVoc.el")
(require 'popup)
(require 'browse-url)

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
                     (append (cdr (assoc 'description (cdr info)))
                             (cdr (assoc 'url (cdr info)))))
                 (seq-filter #'(lambda (kv)
                                 (equal (car kv) 'info))
                             (cdr entity)))
      nil)))

(defun j-names (thing)
  "Look up english names for thing"
  (seq-map #'car (j-urls thing)))

(defun joogle (thing)
  "Present a popup with links to information about thing"
  (interactive)
  (let ((urls (seq-map #'(lambda (url)
                           (popup-make-item (seq-elt url 0) :value (seq-elt url 1)))
                       (j-urls thing))))
    (when urls
      (browse-url (popup-menu* urls)))))

(joogle "^:")
(j-urls "o.")

(defvar jacket-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j g") 'joogle)
    map)
  "Jacket Keymap")

(provide 'jacket)
