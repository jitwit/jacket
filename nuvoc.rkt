#lang racket

(require html-parsing
         sxml/sxpath
         sxml
         webscraperhelper)

(define jsoftware.com "https://code.jsoftware.com")
(define data/Nuvoc "data/Nuvoc")
(define data/jdoc "data/jdoc")

(define nuvoc
  (with-input-from-file data/Nuvoc
    (lambda ()
      (html->xexp (current-input-port)))))

;;;; Parse they key table
;;; Look for the color codes to use to tag J entities later
(define parse-key
  (compose (sxpath '(// td))
           (node-pos 2)
           (sxpath '(// table))))

(define jdoc-key
  (filter-map (lambda (x)
                (match x
                  (`(td (@ (style ,css)) ,blah)
                   (let ((speech-type (string-trim blah))
                         (item-color (string-trim (last (string-split css ":")) ";")))
                     (match speech-type
                       ("KEY:" #f)
                       ("" #f)
                       (_  (cons item-color (string->symbol speech-type))))))
                  (_ #f)))
              (parse-key nuvoc)))

;;;; Parse the main table. Pass 1
;;; groups td's together that are identified as something J-relvant
;;; and tags them with tag J1.
;; todo: would feel nicer to use sxt over explicit named let...
(define (jdoc-entity? node) ;; doesn't catch end of row?
  (match (assoc 'style (sxml:attr-list node))
    (`(style ,css) (and (string-contains? css "border-right:none")
                        (not (string-contains? css "border-left:none"))))
    (_ #f)))

(define (group nodes)
  (let loop ((nodes nodes) (xs '()) (xss '()))
    (match nodes
      ('() (cons '*TOP* (reverse xss)))
      (`(,x ,nodes ...)
       (if (jdoc-entity? x)
         (loop nodes (list x) (cons (cons 'J1 (reverse xs)) xss))
         (loop nodes (cons x xs) xss))))))

(define parse1
  (compose group
           (sxpath '(// td))
           (node-pos 3)
           (sxpath '(// table))))

;;;; Parse main table. Pass 2
;;; Classify J1 nodes according to style sheet in first td. Retag as
;;; J2.
(define (css->speech css)
  (let ((speech (assf (lambda (color)
                        (string-contains? css color))
                      jdoc-key)))
    (and speech (cdr speech))))

(define (J-speech td)
  (match td
    (`(td (@ (style (*text* ,css))) . ,rem)
     (cons (css->speech css) rem))
    (`(td (@ ,row-span (style (*text* ,css))) . ,rem)
     (cons (css->speech css) rem))
    (_ '(bug-jspeech))))

(define (clean-td td)
  (match td
    ('(td
       (@ (style (*text* "border-left:none;border-right:none")))
       (*text* "•\n"))
     #f)
    ('(td (*text* "•\n"))
     #f)
    (`(td (@ . ,atts) .
          ,blah)
     `(td ,@(filter (lambda (b)
                      (not (or (equal? b '(*text* "  "))
                               (equal? b '(*text* "\n"))
                               (equal? b '(*text* ", ")))))
                    blah)))
    (_ td)))

(define (J1->speech j1 . tds)
  (match tds
    ('() #f)
    (`(,td . ,tds)
     `(J2 ,(J-speech td) ,@(filter-map clean-td tds)))
    (_ `(bug-j1speech))))

(define (parse-a a as . bod)
  (match as
    (`(@ . ,as)
     `(about ;; mild danger in assuming cdr
       (url ,@(cdr (assq 'href as)))
       ,@bod))
    (_ '(bug-parsea))))

(define (parse2 nodes)
  (pre-post-order
   ((compose (sxml:modify '("//br" delete))
             (sxml:modify '("//p" delete-undeep)))
    nodes)
   `((*text* . ,(lambda x x))
     (*default* . ,(lambda x x))
     (a . ,parse-a)
     (J1 . ,J1->speech))))

;;;; Parse main table. Pass 3
;; Clean up td and tt nodes and whitespace text
(define clean-tt-ws
  (sxml:modify '("//tt" delete-undeep)))

(define (parse3 nodes)
  (pre-post-order
   (clean-tt-ws (filter identity nodes))
   `(
     (J2 . ,(lambda x x))
     (*text* . ,(lambda (_ x) x))
     (*default* . ,(lambda x x)))))

;;;; 
(define (dump-jdoc)
  (when (file-exists? "data/jdoc")
    (delete-file "data/jdoc"))
  (with-output-to-file "data/jdoc"
    (lambda ()
      (pretty-print ((compose parse2 parse1) nuvoc)))))

(define parse
  (compose parse2 parse1))

