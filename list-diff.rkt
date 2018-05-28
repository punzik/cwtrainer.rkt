;; Copyright (c) 2018 Nikolay Puzanov <punzik@gmail.com>
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket

(provide diff
         diff-stat)

;;; Cyclic shift left
(define (shift-left lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

;;; Find longest overlap of two lists
;;; Returns (values <begin of overlap> <length of overlap>)
(define (max-overlap la lb same? [beg #f] [len #f] [n 0])
  (if (or (null? la) (null? lb)) (values beg len)
      (let-values (((pfx rla rlb) (split-common-prefix la lb (lambda (x y) (and x y (same? x y))))))
        (let ((l (length pfx)))
          (if (and (not (null? pfx)) (or (not len) (> l len)))
              (max-overlap rla rlb same? n l (+ n l))
              (max-overlap (cdr la) (cdr lb) same? beg len (+ n 1)))))))

;;; Shift lists relative to each other and find maximum correlation (overlap).
;;; Returns (values <a> <shifted b> <begin of overlap> <length of overlap>)
(define (correlation a b same? [n (length a)])
  (let-values (((ob ol) (max-overlap a b same?)))
    (if (zero? n)
        (values a b ob ol)
        (let-values (((an bn obn oln)
                      (correlation a (shift-left b) same? (- n 1))))
          (if (or (not ol)
                  (and oln (> oln ol)))
              (values an bn obn oln)
              (values a b ob ol))))))

;;; Split list to 3 pieces
(define (split3 lst beg len)
  (values (take lst beg)
          (take (drop lst beg) len)
          (drop (drop lst beg) len)))

;;; Mark difference of lists:
;;; 'a - only in list a
;;; 'b - only in list b
;;; 'r - replace
;;; '= - equal
(define (mark-difference a b same?)
  (foldr (lambda (ia ib out)
           (cond
            ((and (not ia) (not ib)) out)
            ((not ia) (cons (list ia ib 'b) out))
            ((not ib) (cons (list ia ib 'a) out))
            ((not (same? ia ib)) (cons (list ia ib 'r) out))
            (else (cons (list ia ib '=) out))))
         '() a b))

;;; Calculate lists difference
;;; All #f elements will be removed
(define (diff list-a list-b [same? equal?])
  ;; Remove false items
  (let ((list-a (filter values list-a))
        (list-b (filter values list-b)))
    ;; If empty lists then return '()
    (if (and (null? list-a) (null? list-b)) '()
        ;; Prepare lists for compare and shift
        (let* ((la (length list-a))
               (lb (length list-b))
               (a (append (make-list lb #f) list-a (make-list lb #f)))
               (b (append list-b (make-list la #f) (make-list lb #f))))
          ;; Find longest continous overlap of lists
          (let-values (((a b ob ol) (correlation a b same?)))
            (if ob
                ;; If found overlap split lists to 3 pieces -
                ;; <before overlap> <overlap> <after overlap>
                ;; and repeat diff for <before> and <after>
                (let-values (((a1 a2 a3) (split3 a ob ol))
                             ((b1 b2 b3) (split3 b ob ol)))
                  (append (diff a1 b1 same?)
                          (mark-difference a2 b2 same?)
                          (diff a3 b3 same?)))
                ;; If no overlap simple mark difference
                (mark-difference
                 (append list-a (make-list lb #f))
                 (append list-b (make-list la #f))
                 same?)))))))

;;; Remove item from list and return (values rest item)
(define (extract lst pred [default #f])
  (let ((ret
         (let/cc return
                 (let loop ((ltail lst)
                            (lhead '()))
                   (if (null? ltail) (cons lhead default)
                       (let ((h (car ltail))
                             (t (cdr ltail)))
                         (if (pred h) (return (cons (append lhead t) h))
                             (loop t (cons h lhead)))))))))
    (values (car ret) (cdr ret))))

;;; Return diff statistics:
;;; '(all-hits-percent
;;;   (item hits misses hits-percent)
;;;   ...
;;;   (item hits misses hits-percent))
;;; Misses are calculated relative to list A
;;;
;;; (same? a b) - list item (car diff item) equality check
;;; (skip? a) - exclude item from statiscic calculation
(define (diff-stat df [same? equal?] [skip? not])
  (let ((stat
         ;; Add hits percentage to stats
         (map (lambda (stat-item)
                (let ((item (car stat-item))
                      (hits (cadr stat-item))
                      (miss (caddr stat-item)))
                  (list item hits miss (exact->inexact (* 100 (/ hits (+ hits miss)))))))
              ;; Calculate hits and mishits
              (foldl (lambda (di stat-set)
                       (let ((a (car di))
                             (b (cadr di))
                             (m (caddr di)))
                         (if (skip? a) stat-set
                             (let-values (((stat-set si) (extract stat-set
                                                                  (lambda (x) (same? a (car x)))
                                                                  `(,a 0 0))))
                               (let ((hits (cadr si))
                                     (miss (caddr si)))
                                 (cons (if (eq? m '=)
                                           (list a (+ 1 hits) miss)
                                           (list a hits (+ 1 miss)))
                                       stat-set))))))
                     '() df))))
    ;; Calculate all-hits percentage
    (cons
     (let ((hits/miss
            (foldl (lambda (x hm)
                     (cons (+ (car hm) (cadr x))
                           (+ (cdr hm) (caddr x))))
                   '(0 . 0) stat)))
       (let ((hits (car hits/miss))
             (miss (cdr hits/miss)))
         (exact->inexact (* 100 (/ hits (+ hits miss))))))
     stat)))
