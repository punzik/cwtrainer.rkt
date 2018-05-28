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

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(provide audio/make-playback)

(define-ffi-definer define-pulse-simple (ffi-lib "libpulse-simple"))
(define-ffi-definer define-pulse (ffi-lib "libpulse"))

;;; Minimal PulseAudio wrappers infrastructure.
;;; With use YAGNI principle (You aren't gonna need it).

(define PA_CHANNELS_MAX 32)
(define PA_STREAM_PLAYBACK 1)
(define PA_SAMPLE_FLOAT32LE 5)

;;; Map of pa_simple_spec struct
(define-cstruct _pa_sample_spec
  ([format _int]
   [rate _uint32]
   [channels _uint8]))

;;; Map of pa_buffer_attr
(define-cstruct _pa_buffer_attr
  ([maxlength _uint32]
   [tlength _uint32]
   [prebuf _uint32]
   [minreg _uint32]
   [fragsize _uint32]))

;;; Pointer to pa_simple struct
(define _pa_simple_pointer (_cpointer 'pa_simple))

;;; Wrapper for pa_simple_new
(define-pulse-simple pa_simple_new
  (_fun _string                         ; server. NULL for default
        _string                         ; name
        _int                            ; pa_stream_direction
        _string                         ; device. NULL for default
        _string                         ; stream_name
        (_ptr i _pa_sample_spec)        ; pa_sample_spec. Not NULL
        _gcpointer                      ; pa_channel_map. May be NULL (by default is NULL)
        _gcpointer                      ; pa_buffer_attr. May be NULL
        (_ptr o _int)
        -> _pa_simple_pointer))

;; (define-pulse pa_strerror
;;   (_fun _int) -> _string)

(define (pa_strerror err)
  "ERROR")

(define (pa-check ret perr)
  (when (< ret 0)
    (error (pa_strerror (ptr-ref perr _int)))))

;;; Wrapper for pa_simple_write
(define-pulse-simple pa_simple_write
  (_fun _pa_simple_pointer              ; pa_simple pointer
        _gcpointer                      ; data for write (void*)
        _size                           ; size of data in bytes?
        (e : (_ptr o _int))
        -> (r : _int) -> (pa-check r e)))

;;; int pa_simple_drain (pa_simple *s, int *error)
(define-pulse-simple pa_simple_drain
  (_fun _pa_simple_pointer
        (e : (_ptr o _int))
        -> (r : _int) -> (pa-check r e)))

;;; void pa_simple_free (pa_simple *s)
(define-pulse-simple pa_simple_free
  (_fun _pa_simple_pointer
        -> _void))

;;; Example:
;; (let* ((ss     (make-pa_sample_spec PA_SAMPLE_FLOAT32LE 44100 1))
;;        (stream (pa_simple_new #f "App name" PA_STREAM_PLAYBACK #f "Stream name" ss #f #f)))
;;   (pa_simple_write stream (f32vector->cpointer vec) (* 4 (f32vector-length vec)))
;;   (pa_simple_drain stream)
;;   (pa_simple_free stream))

;;; API

(define (audio/make-playback name samplerate channels)
  (let ((pa-stream (pa_simple_new #f name PA_STREAM_PLAYBACK #f ""
                                  (make-pa_sample_spec PA_SAMPLE_FLOAT32LE samplerate channels)
                                  #f #f)))
    (define (play seq)
      (let ((f32vec (make-f32vector (stream-length seq))))
        (for/fold ((n 0)) ((val seq))
          (f32vector-set! f32vec n (exact->inexact val))
          (+ n 1))
        (pa_simple_write pa-stream (f32vector->cpointer f32vec) (* 4 (f32vector-length f32vec)))
        (pa_simple_drain pa-stream)))

    (lambda (method)
      (case method
        ((samplerate) samplerate)
        ((channels) channels)
        ((free) (pa_simple_free pa-stream))
        ((play) play)
        (else (error "Audio playback has no method" method))))))
