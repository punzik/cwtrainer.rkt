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

(require rnrs/io/ports-6)
(require "audio.rkt")
(require "terminal.rkt")
(require "list-diff.rkt")

;;; -------- READ CONFIGURATION PARAMETERS --------
(define CONFIG_FILE "cwtrainer.conf")

(define (read-config file)
  (let ((config
         (if (not (file-exists? file)) #f
             (call-with-input-file file read))))
    (lambda (symname default)
      (if (not config) default
          (let ((as (assoc symname config)))
            (if as (cadr as)
                default))))))

(define config (read-config CONFIG_FILE))

;;; Output audio sample rate
(define SAMPLE_RATE (config 'SAMPLE_RATE 44100))

;;; CW tone in HZ
(define TONE (config 'TONE 750))

;;; Volume (0..1)
(define VOLUME (config 'VOLUME 0.5))

;;; Symbol front and back fading in percent of dit length
(define FADING (config 'FADING 10))

;;; CW character speed in WPM
(define CHAR_SPEED (config 'CHAR_SPEED 25))

;;; CW characer space in dits (default 3)
(define CHAR_SPACE (config 'CHAR_SPACE 3))

;;; CW word spacing in Dits
(define WORD_SPACE (config 'WORD_SPACE 7))

;;; Dah length in dits (default 3)
(define DAH_LENGTH (config 'DAH_LENGTH 3))

;;; Koch method letters sequence
(define KOCH_CHARS (config 'KOCH_CHARS "kmrsuaptlowi.njef0yv,g5/q9zh38b?427c1d6x"))

;;; Current Koch step
(define KOCH_STEP (config 'KOCH_STEP (string-length KOCH_CHARS)))

;;; Length of the lesson (letters incluse spaces)
(define KOCH_LENGTH (config 'KOCH_LENGTH 200))

;;; Minimum length of word
(define KOCH_MINL (config 'KOCH_MINL 1))

;;; Maximum length of word
(define KOCH_MAXL (config 'KOCH_MAXL 5))

;;; Show character before or after keying
(define SHOW_CHAR (config 'SHOW_CHAR 'after))

;;; New letters is prefered for random generation
(define KOCH_NEW_CHAR_PREF (config 'KOCH_NEW_CHAR_PREF #f))
;;; -----------------------------------------------

;;; Calculated constants
(define DIT_LEN_SEC (/ 1.2 CHAR_SPEED))
(define CHAR_SPACE_SEC (* CHAR_SPACE DIT_LEN_SEC))
(define WORD_SPACE_SEC (* WORD_SPACE DIT_LEN_SEC))
(define FADING_SEC (* DIT_LEN_SEC (/ FADING 100)))

;;; Number to exact integer
(define (float->int n)
  (inexact->exact (round n)))

;;; How many samples in duration
(define (samples-count samplerate duration)
  (float->int (* samplerate duration)))

;;; Make sine wave
(define (make-sine samplerate tone volume len)
  (let ((angle-per-sample (/ (* pi 2) (/ samplerate tone))))
    (build-list len (lambda (n) (* volume (sin (* n angle-per-sample)))))))

;;; Like map but proc with additional argument of item number
(define (mapn proc lst)
  (let loop ((li lst) (lo '()) (n 0))
    (if (null? li) (reverse lo)
        (loop (cdr li) (cons (proc (car li) n) lo) (+ n 1)))))

;;; Add fading
(define (fade fading-length sym)
  (let ((len (length sym)))
    (mapn (lambda (item num)
            (cond
             ((< num fading-length) (* item (/ num fading-length)))
             ((> num (- len fading-length)) (* item (/ (- len num) fading-length)))
             (else item))) sym)))

;;; Make playable CW symbol
;;;  playback - (audio/make-playback)
;;;  tone     - symbol tone in Hz
;;;  duration - symbol duration in seconds
;;;  fading   - front and back fading in seconds
;;;  volume   - audio volume from 0 (mute) to 1 (max)
;;; Returns lambda ()
(define (make-cw-symbol playback tone duration [fading 0] [volume 1])
  (let* ((symbol-samples (float->int (* (playback 'samplerate) duration)))
         (fading-samples (float->int
                          (let ((fs (* (playback 'samplerate) fading))
                                (half-sym (/ symbol-samples 2)))
                            (if (> fs half-sym) half-sym fs))))
         (sym (fade fading-samples
                    (make-sine (playback 'samplerate)
                               tone
                               volume
                               symbol-samples))))
    (lambda () ((playback 'play) sym))))

;;; Make space symbol
;;; Returns lambda ()
(define (make-cw-space duration)
  (lambda () (sleep duration)))

;;; Make CW sequence player
;;;  dit     - dit player
;;;  dah     - dah player
;;;  space   - space player
;;; Returns lambda (seq), where
;;;  seq     - string with chars '.', '-' and ' ' (e.g. ". - - .")
(define (make-cw-seq-player dit dah space)
  (lambda (seq)
    (for-each
     (lambda (sym)
       (cond
        ((char=? sym #\.) (dit))
        ((char=? sym #\-) (dah))
        (else (space))))
     (string->list seq))))

;;; Make CW text player
;;; abet        - morse alphabet (assoc list with chars and corresponding sequence)
;;; char-space  - inter-char space player
;;; word-space  - inter-word space player
;;; play-cw-seq - morse sequence player
;;; Returns lambda (text), where
;;;  text       - text to play
(define (make-cw-text-player abet char-space word-space play-cw-seq
                             #:before [before-play-char not]
                             #:after [after-play-char not])
  (lambda (text)
    (call/cc
     (lambda (return)
       (let play-next ((text (string->list text)))
         (when (not (null? text))
           (let ((char (car text))
                 (rest (cdr text)))
             (if (char=? char #\space)
                 (begin
                   (before-play-char #\space)
                   (word-space)
                   (after-play-char #\space))
                 (let ((morse (assoc char abet (lambda (a b) (member a b char-ci=?)))))
                   (when (not (null? morse))
                     (before-play-char char)
                     (play-cw-seq (cadr morse))
                     (after-play-char char)
                     (when rest (char-space)))))
             ;; Check thread message
             (case (thread-try-receive)
               ((stop) (return #f))
               ((toggle)
                (let loop ()
                  (if (equal? (thread-receive) 'toggle)
                      (char-space)
                      (loop)))))
             ;; Loop
             (play-next rest))))))))

;;; Morse alphabet
(define morse-alphabet
  '(((#\a #\а) ". -")
    ((#\b #\б) "- . . .")
    ((#\c #\ц) "- . - .")
    ((#\d #\д) "- . .")
    ((#\e #\е #\ё) ".")
    ((#\f #\ф) ". . - .")
    ((#\g #\г) "- - .")
    ((#\h #\х) ". . . .")
    ((#\i #\и) ". .")
    ((#\j #\й) ". - - -")
    ((#\k #\к) "- . -")
    ((#\l #\л) ". - . .")
    ((#\m #\м) "- -")
    ((#\n #\н) "- .")
    ((#\o #\о) "- - -")
    ((#\p #\п) ". - - .")
    ((#\q #\щ) "- - . -")
    ((#\r #\р) ". - .")
    ((#\s #\с) ". . .")
    ((#\t #\т) "-")
    ((#\u #\у) ". . -")
    ((#\v #\ж) ". . . -")
    ((#\w #\в) ". - -")
    ((#\x #\ь #\ъ) "- . . -")
    ((#\y #\ы) "- . - -")
    ((#\z #\з) "- - . .")
    ((#\ч) "- - - .")
    ((#\ш) "- - - -")
    ((#\э) ". . - . .")
    ((#\ю) ". . - -")
    ((#\я) ". - . -")
    ((#\1) ". - - - -")
    ((#\2) ". . - - -")
    ((#\3) ". . . - -")
    ((#\4) ". . . . -")
    ((#\5) ". . . . .")
    ((#\6) "- . . . .")
    ((#\7) "- - . . .")
    ((#\8) "- - - . .")
    ((#\9) "- - - - .")
    ((#\0) "- - - - -")
    ((#\.) ". . . . . .")
    ((#\,) ". - . - . -")
    ((#\:) "- - - . . .")
    ((#\;) "- . - . - .")
    ((#\() "- . - - . -")
    ((#\)) "- . - - . -")
    ((#\") ". - . . - .")
    ((#\') ". - . . - .")
    ((#\-) "- . . . . -")
    ((#\\) "- . . - .")
    ((#\/) "- . . - .")
    ((#\?) ". . - - . .")
    ((#\!) "- - . . - -")
    ((#\@) ". - - . - .")))

;;; Generate random text
(define (gen-random-text abet alen len minl maxl [new-pref #f])
  (let loop ((text '()))
    (if (>= (length text) len) (list->string text)
        (loop (append text (build-list (random minl (+ maxl 1))
                                       (if new-pref
                                           (lambda (x) (string-ref
                                                   abet
                                                   (let ((r (random)))
                                                     (inexact->exact (truncate (* alen (sqrt r)))))))
                                           (lambda (x) (string-ref abet (random alen)))))
                      '(#\space))))))

;;; Print diff
(define (print-diff d)
  (display "Sent: ")
  (for-each
   (lambda (x)
     (let ((ch (car x)))
       (if (not ch)
           (display " ")
           (display ch))))
   d)
  (newline)

  (display "Rcvd: ")
  (for-each
   (lambda (x)
     (let ((ch (cadr x))
           (st (caddr x)))
       (display
        (cond
         ((eq? st 'a) (c 'bg-red))
         ((eq? st 'b) (c 'bg-yellow))
         ((eq? st 'r) (c 'fg-red))
         (else "")))
       (if (not ch)
           (display " ")
           (display ch))
       (display (c 'none))))
   d)
  (newline))

;;; Print statistics
(define (print-stat stat)
  (define (~% x)
    (string-append
     (cond
      ((>= x 95) "")
      ((>= x 85) (c 'fg-yellow))
      (else (c 'fg-red)))
     (~r x #:precision 1) "%"
     (c 'none)))

  (printf "Overall accuracy: ~a\n" (~% (car stat)))
  (for-each (lambda (x) (printf "   ~a: ~a/~a (~a)\n"
                           (car x)
                           (cadr x)
                           (caddr x)
                           (~% (cadddr x))))
            (cdr stat)))

;;; ----------------------------- TRAINING ---------------------------
;;; Receive training
(define (train-receive text)
  (let* ((pb   (audio/make-playback "CW" SAMPLE_RATE 1))
         (dit  (make-cw-symbol pb TONE DIT_LEN_SEC FADING_SEC VOLUME))
         (dah  (make-cw-symbol pb TONE (* DAH_LENGTH DIT_LEN_SEC) FADING_SEC VOLUME))
         (play (make-cw-text-player morse-alphabet
                                    (make-cw-space CHAR_SPACE_SEC)
                                    (make-cw-space WORD_SPACE_SEC)
                                    (make-cw-seq-player dit dah (make-cw-space DIT_LEN_SEC)))))

    (printf "\nBe ready to input text (~a symbols)... " (string-length text))
    (for-each (lambda (n)
                (display n)
                (flush-output)
                (sleep 1)
                (display char-back))
              '(3 2 1))
    (printf "Go!\n>> ")
    (sleep 0.5)

    (let ((diff
           (let ((thrd (thread (lambda () (play text)))))
             (begin0
                 (let ((intext
                        (read-line (lambda (ch)
                                     (if (equal? ch 'cr)
                                         (if (thread-running? thrd)
                                             (begin (thread-send thrd 'toggle)
                                                    'skip)
                                             #t)
                                         #f)))))
                   (diff (string->list text) (string->list intext) char-ci=?))
               (thread-wait thrd)))))
      (let ((stat
             (let ((stat (diff-stat diff char-ci=? (lambda (x) (or (not x) (equal? x #\space))))))
               (cons (car stat)
                     (sort (cdr stat)
                           (lambda (a b) (char-ci<? (car a) (car b))))))))
        (printf "\nResult:\n")
        (print-diff diff)
        (printf "\nStatistics:\n")
        (print-stat stat)
        (newline)))
    (pb 'free)))

;;; Just listening
(define (train-listen text)
  (define (show-char ch)
    (display ch) (flush-output))

  (let* ((pb   (audio/make-playback "CW" SAMPLE_RATE 1))
         (dit  (make-cw-symbol pb TONE DIT_LEN_SEC FADING_SEC VOLUME))
         (dah  (make-cw-symbol pb TONE (* DAH_LENGTH DIT_LEN_SEC) FADING_SEC VOLUME))
         (play (make-cw-text-player morse-alphabet
                                    (make-cw-space CHAR_SPACE_SEC)
                                    (make-cw-space WORD_SPACE_SEC)
                                    (make-cw-seq-player dit dah (make-cw-space DIT_LEN_SEC))
                                    #:before (if (eq? SHOW_CHAR 'before) show-char not)
                                    #:after (if (eq? SHOW_CHAR 'after) show-char not))))

    (printf "\nBe ready to listen text (~a symbols)... " (string-length text))
    (for-each (lambda (n)
                (display n)
                (flush-output)
                (sleep 1)
                (display char-back))
              '(3 2 1))
    (printf "Go!\n>> ")
    (sleep 0.5)
    (play text)
    (printf "\n\n")
    (pb 'free)))


;;; ----------------------------- MAIN ---------------------------
(define (main)
  (let ((mode
            (let ((cmds (vector->list (current-command-line-arguments))))
              (if (= (length cmds) 0) 'receive
                  (cond
                   ((string-ci=? (car cmds) "receive") 'receive)
                   ((string-ci=? (car cmds) "listen")
                    (if (< (length cmds) 2)
                        'listen
                        (cadr cmds)))
                   (else
                    (printf "Usage:\n")
                    (printf "  cwtrainer [command] [argument]\n\n")
                    (printf "Commands:\n")
                    (printf "  receive           Default. Training to receive Morse code\n")
                    (printf "  listen [string]   Only listen [string] or random sequence\n\n")
                    (error "Unknown argument" (car cmds))))))))
    (cond
     ((eq? mode 'receive) (train-receive (gen-random-text KOCH_CHARS KOCH_STEP KOCH_LENGTH KOCH_MINL KOCH_MAXL KOCH_NEW_CHAR_PREF)))
     ((eq? mode 'listen) (train-listen (gen-random-text KOCH_CHARS KOCH_STEP KOCH_LENGTH KOCH_MINL KOCH_MAXL KOCH_NEW_CHAR_PREF)))
     ((string? mode) (train-listen mode)))))

(main)
