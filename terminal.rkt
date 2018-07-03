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

(provide
 ;; Call getchar (and other input) without wait for enter press
 with-nowait-input

 ;; Read line
 read-line

 ;; Parameter. If #t (c) return color, "" otherwise
 colorize

 ;; Return escape sequence for color
 c

 ;; Back character
 char-back)

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(define-ffi-definer define-libc (ffi-lib "libc.so.6"))

(define ICANON 2)
(define ECHO 8)
(define TCSANOW 0)
(define STDIN_FILENO 0)

;; typedef unsigned char cc_t;
;; typedef unsigned int	speed_t;
;; typedef unsigned int	tcflag_t;
;;
;; #define NCCS 32
;;
;; struct termios
;; {
;;     tcflag_t c_iflag;    /* input mode flags */
;;     tcflag_t c_oflag;    /* output mode flags */
;;     tcflag_t c_cflag;    /* control mode flags */
;;     tcflag_t c_lflag;    /* local mode flags */
;;     cc_t c_line;         /* line discipline */
;;     cc_t c_cc[NCCS];     /* control characters */
;;     speed_t c_ispeed;    /* input speed */
;;     speed_t c_ospeed;    /* output speed */
;; };
;;
;; #define VTIME 5
;; #define VMIN 6

(define-cstruct _termios
  ([c_iflag  _int]
   [c_oflag  _int]
   [c_cflag  _int]
   [c_lflag  _int]
   [c_line   _uint8]
   [c_cc_top (_array _uint8 5)]
   [c_cc_VTIME _uint8]
   [c_cc_VMIN _uint8]
   [c_cc_bot (_array _uint8 25)]
   [c_ispeed _int]
   [c_ospeed _int]))

;;; int tcgetattr (int __fd, struct termios *__termios_p);
(define-libc tcgetattr
  (_fun _int
        (tios : (_ptr o _termios))
        -> (r : _int) -> ((lambda (r x) (if (zero? r) x (error "tcgetattr error: ~a" r))) r tios)))

;;; int tcsetattr (int __fd, int __optional_actions, const struct termios *__termios_p);
(define-libc tcsetattr
  (_fun _int
        _int
        (_ptr i _termios)
        -> (r : _int) -> ((lambda (r) (unless (zero? r) (error "tcsetattr error: ~a" r))) r)))

(define-syntax-rule (with-nowait-input body ...)
  (let ([tio.bak #f])
    (dynamic-wind (lambda ()
                    (let ((tio (tcgetattr STDIN_FILENO)))
                      (set! tio.bak (struct-copy termios tio))
                      (set-termios-c_lflag! tio (bitwise-and (termios-c_lflag tio)
                                                             (bitwise-not (bitwise-ior ICANON ECHO))))
                      (set-termios-c_cc_VTIME! tio 0)
                      (set-termios-c_cc_VMIN! tio 1)
                      (tcsetattr STDIN_FILENO TCSANOW tio)))
                  (lambda () body ...)
                  (lambda ()
                    (tcsetattr STDIN_FILENO TCSANOW tio.bak)))))

;; LOCAL. Escape sequences
(define escape-sequences
  '(((27 91 65) up)
    ((27 91 66) down)
    ((27 91 67) right)
    ((27 91 68) left)
    ((27 91 70) end)
    ((27 91 72) home)
    ((27 91 51 126) del)
    ((27 91 53 126) pgup)
    ((27 91 54 126) pgdwn)
    ((27 79 80) f1)
    ((27 79 81) f2)
    ((27 79 82) f3)
    ((27 79 83) f4)
    ((27 91 49 53 126) f5)
    ((27 91 49 55 126) f6)
    ((27 91 49 56 126) f7)
    ((27 91 49 57 126) f8)
    ((27 91 50 48 126) f9)
    ((27 91 50 49 126) f10)))

;; LOCAL. Control codes
(define ctrls
  '((27  esc)
    (127 bs)
    (9   tab)
    (10  cr)))

;; List hd is head of list lst
(define (head-of? lst hd)
  (let loop ((l lst)
             (h hd))
    (if (null? h) #t
        (if (null? l) #f
            (if (equal? (car l) (car h))
                (loop (cdr l) (cdr h))
                #f)))))

;;; Read char with parse escape sequences
(define (read-char-esc)
  (let restart ((buffer '())
                (seqs escape-sequences))
    (let* ((char (read-char))
           (buffer (append buffer (list (char->integer char))))
           (seqs (filter (lambda (item) (head-of? (car item) buffer)) seqs)))
      (if (null? seqs)
          (let ((ctrl (assq (char->integer char) ctrls)))
            (if ctrl (cadr ctrl) char))
          (if (and (null? (cdr seqs))
                   (equal? (caar seqs) buffer))
              (cadr (car seqs))
              (restart buffer seqs))))))

;;; Backspace character
(define char-back (integer->char 8))

;;; CTRL-D character
(define ctrl-d (integer->char 4))

;;; Read line
(define (read-line [stop? #f])
  (with-nowait-input
   (let loop ((before-cur '())
              (after-cur '()))
     (let ((ch (read-char-esc)))
       (let ((return (if stop?
                         (stop? ch)
                         (or  (equal? ch ctrl-d)
                              (equal? ch 'cr)))))
         (if (eq? return 'skip)
             (loop before-cur after-cur)
             (if return
                 (begin
                   (printf "\n")
                   (list->string (append (reverse before-cur) after-cur)))
                 (apply
                  loop
                  (begin
                    (cond
                     ((eq? ch 'bs)               ; Back space
                      (if (not (null? before-cur))
                          (begin
                            (display char-back)
                            (for-each display after-cur)
                            (display " ")
                            (for-each (lambda (x) (display char-back)) after-cur)
                            (display char-back)
                            (flush-output)
                            (list (cdr before-cur) after-cur))
                          (list before-cur after-cur)))

                     ((eq? ch 'left)             ; Left
                      (if (not (null? before-cur))
                          (begin
                            (printf "~a" char-back)
                            (flush-output)
                            (list (cdr before-cur) (cons (car before-cur) after-cur)))
                          (list before-cur after-cur)))

                     ((eq? ch 'right)            ; Right
                      (if (not (null? after-cur))
                          (begin
                            (let ((ch (car after-cur)))
                              (printf "~a" ch)
                              (flush-output)
                              (list (cons ch before-cur) (cdr after-cur))))
                          (list before-cur after-cur)))

                     ((eq? ch 'del)              ; Delete
                      (if (not (null? after-cur))
                          (let ((after-cur (cdr after-cur)))
                            (for-each display after-cur)
                            (display " ")
                            (for-each (lambda (x) (display char-back)) after-cur)
                            (display char-back)
                            (flush-output)
                            (list before-cur after-cur))
                          (list before-cur after-cur)))

                     ((symbol? ch)               ; Unused control symbol
                      (list before-cur after-cur))

                     (else
                      (display ch)
                      (for-each display after-cur)
                      (for-each (lambda (x) (display char-back)) after-cur)
                      (flush-output)
                      (list (cons ch before-cur) after-cur))))))))))))

;;; Return escaped string
(define (esc str)
  (string-append (list->string (list (integer->char 27))) str))

;;; ANSI color escape sequences
(define ansi-colors
  `((none               ,(esc "[0m"))
    ;; Foreground
    (fg-black           ,(esc "[0;30m"))
    (fg-red             ,(esc "[0;31m"))
    (fg-green           ,(esc "[0;32m"))
    (fg-orange          ,(esc "[0;33m"))
    (fg-blue            ,(esc "[0;34m"))
    (fg-purple          ,(esc "[0;35m"))
    (fg-cyan            ,(esc "[0;36m"))
    (fg-light-gray      ,(esc "[0;37m"))
    (fg-gray            ,(esc "[1;30m"))
    (fg-light-red       ,(esc "[1;31m"))
    (fg-light-green     ,(esc "[1;32m"))
    (fg-yellow          ,(esc "[1;33m"))
    (fg-light-blue      ,(esc "[1;34m"))
    (fg-light-purple    ,(esc "[1;35m"))
    (fg-light-cyan      ,(esc "[1;36m"))
    (fg-white           ,(esc "[1;37m"))
    ;; Background
    (bg-black           ,(esc "[40m"))
    (bg-red             ,(esc "[41m"))
    (bg-green           ,(esc "[42m"))
    (bg-yellow          ,(esc "[43m"))
    (bg-blue            ,(esc "[44m"))
    (bg-purple          ,(esc "[45m"))
    (bg-cyan            ,(esc "[46m"))
    (bg-gray            ,(esc "[47m"))))

(define colorize (make-parameter #t))

;;; Return escape sequence for color
(define (c clr)
  (if (colorize)
      (let ((x (assoc clr ansi-colors)))
        (if x (cadr x) (error "No color" clr)))
      ""))
