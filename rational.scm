#!/usr/bin/guile -s
!#
;; Transpiler for the Rational Programming Language
;; © 2023 Sarthak Shah (shahsarthakw@gmail.com)
;; Transpiles programs from Rational to Unlambda

;; Example usage:
;; ./rational.scm "1A.A3"

(use-modules (ice-9 match)
             (srfi srfi-1))

(define default-parser
  (let ((ht (make-hash-table)))
    (hash-set! ht 0 3)
    (hash-set! ht 1 "S")
    (hash-set! ht 2 "K")
    (hash-set! ht 3 "")
    ht))

(define (chop-string str chunksize)
  (let [(strlen (string-length str))]
    (cond 
      [(= strlen 0) '()]
      [(< strlen chunksize)
       (cons str '())]
      [else (cons (substring str 0 chunksize) (chop-string (substring str chunksize) chunksize))])))

(define (pad-to-four str)
  (string-append
    (zerostr (- 4 (string-length str)))
    str))

(define (generate-parser reference-string)
  (let* ([plist (apply append
                       (map (lambda (x)
                              (match (chop-string x 2)
                                     [(left right)
                                      (list (string->number (number->string (string->number left 2) 4))
                                            (string->number (number->string (string->number right 2) 4)))]))
                            (map (lambda (x) 
                                   (pad-to-four (number->string (string->number (string x) 16) 2)))
                                 (string->list reference-string))))]

         ;; make sublists delimited by 0s
         [sublists (fold
                     (lambda (x y)
                       (case x
                         ((1 2 3)
                          (match y
                                 [(a . rest)
                                  (cons (cons x a) rest)]))
                         (else
                           (cons '() y))))
                     (list '())
                     (reverse plist))]
         [strlist (map
                    (lambda (lst)
                      (apply string-append
                             (filter string?
                                     (map (lambda (num)
                                            (hash-ref default-parser 
                                                      (1- num)))
                                          lst))))
                    sublists)]
         [ht (make-hash-table)])
    (let loop ((i 1)
               (lst strlist))
      (if (null? lst)
        ;; 0 -> always "0"
        ;; so we use it to store the size of the ht
        (and (hash-set! ht 0 i)
             ht)
        (and (hash-set! ht i (car lst))
             (loop (1+ i)
                   (cdr lst)))))))

(define (find-bitsize num)
  ;; given a num, find the nearest power of 2
  (inexact->exact (ceiling (/ (log num)
                              (log 2)))))

(define (zerostr num)
  (if (< num 1)
    ""
    (string-append "0" (zerostr (1- num)))))

(define (break-into-bitsize bitsize hexstr)
  (let* ([binstr (number->string (string->number hexstr 16) 2)]
         [strlen (string-length binstr)]
         [padding-size (modulo strlen bitsize)]
         [padded-string (if (not (= padding-size strlen))
                          (string-append (zerostr padding-size) 
                                         binstr)
                          binstr)]
         [cstrlst (chop-string padded-string bitsize)])
    (map (lambda (x)
           (string->number x 2))
         cstrlst)))

(define (parse parser init-str)
  (let* ([bitsize
           (find-bitsize (hash-ref parser 0))]
         [nlst (break-into-bitsize bitsize init-str)])
    (apply string-append
           (map (lambda (x)
                  (if (= x 0)
                    "`"
                    (hash-ref parser x)))
                nlst))))

(let [(output
        (let [(input (cadr (command-line)))]
          (if (eq? (substring input 0 1) #\.)
            (set! input (cons #\0 input)))
          (match (string-split input #\.)
                 [(string-pre)
                  (parse default-parser string-pre)]
                 [(string-pre (? null? string-post))
                  (parse default-parser string-pre)]
                 [(string-pre string-post)
                  (parse (generate-parser string-post)
                         string-pre)])))]
  (let loop [(clen (length (filter (lambda (x) (not (eq? x #\`)))
                                   (string->list output))))]
    (if (< clen 2)
      (display output)
      (and (display "`")
           (loop (1- clen))))))
(newline)
