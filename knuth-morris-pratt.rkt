#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MP

(struct nil ())
(struct node (top next rest))

(define (morris-pratt pattern text)
  (define (make t r)
    (define n
      (delay
        (cond
          [(equal? t "") (nil)]
          [else (make (string-rest t) (step r (string-first t)))])))
    (node t n r))
  (define init (make pattern (nil)))
  (define (step acc x)
    (match acc
      [(nil) init]
      [(node t n r) (if (check? acc x) (force n) (step r x))]))
  (reset! init pattern)
  (fold-until init step done? text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KMP

(define (knuth-morris-pratt pattern text)
  (define (make t r)
    (define n
      (delay
        (cond
          [(equal? t "") (nil)]
          [else (make (string-rest t) (step r (string-first t)))])))
    (define r*
      (cond
        [(equal? t "") r]
        [(check? r (string-first t)) (node-rest r)]
        [else r]))
    (node t n r*))
  (define init (make pattern (nil)))
  (define (step acc x)
    (match acc
      [(nil) init]
      [(node t n r) (if (check? acc x) (force n) (step r x))]))
  (reset! init pattern)
  (fold-until init step done? text))

(define (done? acc)
  (match acc
    [(nil) false]
    [(node t _ _) (equal? t "")]))

(define (check? acc x)
  (match acc
    [(nil) false]
    [(node t _ _) (and (not (equal? t ""))
                       (equal (string-first t) x))]))

(define (fold-until acc step done? text)
  (cond
    [(done? acc) true]
    [(equal? text "") false]
    [else (define acc* (step acc (string-first text)))
          (fold-until acc* step done? (string-rest text))]))

(define (string-first s) (string-ref s 0))
(define (string-rest s) (substring s 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imperative (reference) MP

(define-syntax-rule (while e b ...)
  (let loop ()
    (when e b ... (loop))))

(define (morris-pratt-reference pattern text)
  (define table (morris-pratt-vector pattern))
  (define n (string-length text))
  (define m (string-length pattern))
  (define i 0)
  (define j 0)
  (let/ec return
    (while (< j n)
      (while (and (> i -1) (not (equal (@ pattern i) (@ text j))))
        (set! i (vector-ref table i)))
      (set! i (add1 i))
      (set! j (add1 j))
      (when (>= i m)
        (return true)))
    (return false)))

(define (morris-pratt-vector pattern)
  (define m (string-length pattern))
  (define result (make-vector (add1 m) 0))
  (define i 0)
  (define j -1)
  (vector-set! result 0 -1)
  (while (< i m)
    (while (and (> j -1) (not (equal? (@ pattern i) (@ pattern j))))
      (set! j (vector-ref result j)))
    (set! i (add1 i))
    (set! j (add1 j))
    (vector-set! result i j))
  result)

(define @ string-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imperative (reference) KMP

(define (knuth-morris-pratt-reference pattern text)
  (define table (knuth-morris-pratt-vector pattern))
  (define n (string-length text))
  (define m (string-length pattern))
  (define i 0)
  (define j 0)
  (let/ec return
    (while (< j n)
      (while (and (> i -1) (not (equal (@ pattern i) (@ text j))))
        (set! i (vector-ref table i)))
      (set! i (add1 i))
      (set! j (add1 j))
      (when (>= i m)
        (return true)))
    (return false)))

(define (knuth-morris-pratt-vector pattern)
  (define m (string-length pattern))
  (define result (make-vector (add1 m) 0))
  (define i 0)
  (define j -1)
  (vector-set! result 0 -1)
  (while (< i m)
    (while (and (> j -1) (not (equal? (@ pattern i) (@ pattern j))))
      (set! j (vector-ref result j)))
    (set! i (add1 i))
    (set! j (add1 j))
    (if (and (< i m) (equal? (@ pattern i) (@ pattern j)))
        (vector-set! result i (vector-ref result j))
        (vector-set! result i j)))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(define trace (make-parameter null))

(define (equal x y)
  (trace (cons (list x y) (trace)))
  (equal? x y))

(define (reset! init pattern)
  (for/fold ([acc (force init)])
            ([x (in-string pattern)])
    (force (node-next acc)))
  (trace null))

(module+ test
  (require rackunit)

  (define (all-strings<= lower upper size)
    (cond
      [(zero? size) null]
      [else
       (append (all-strings= lower upper size)
               (all-strings<= lower upper (sub1 size)))]))

  (define (all-strings= lower upper size)
    (cond
      [(zero? size) (list "")]
      [else
       (define others (all-strings= lower upper (sub1 size)))
       (for*/list ([c (in-chars-inclusive lower upper)]
                   [other (in-list others)])
         (string-append (char->string c) other))]))

  (define (in-chars-inclusive lower upper)
    (define nat-range
      (in-inclusive-range (char->integer lower)
                          (char->integer upper)))
    (sequence-map integer->char nat-range))

  (define (char->string c)
    (make-string 1 c))

  (define (->bool x)
    (and x true))

  (define TEXT-LOWER #\a)
  (define TEXT-UPPER #\c)
  (define TEXT-LENGTH 5)
  (define TEXTS (all-strings<= TEXT-LOWER TEXT-UPPER TEXT-LENGTH))

  (define PATTERN-LOWER #\a)
  (define PATTERN-UPPER #\c)
  (define PATTERN-LEN 3)
  (define PATTERNS (all-strings<= PATTERN-LOWER PATTERN-UPPER PATTERN-LEN))

  (for* ([text (in-list TEXTS)]
         [pattern (in-list PATTERNS)])
    (parameterize ([trace null])
      (define result (string-contains? text pattern))

      (define mp-ref-result (morris-pratt-reference pattern text))
      (define mp-ref-trace (trace))
      (trace null)

      (define mp-result (morris-pratt pattern text))
      (define mp-trace (trace))
      (trace null)

      (define kmp-ref-result (knuth-morris-pratt-reference pattern text))
      (define kmp-ref-trace (trace))
      (trace null)

      (define kmp-result (knuth-morris-pratt pattern text))
      (define kmp-trace (trace))
      (trace null)

      (check-equal? result mp-ref-result)
      (check-equal? result mp-result)
      (check-equal? result kmp-ref-result)
      (check-equal? result kmp-result)

      (check-equal? mp-ref-trace mp-trace)
      (check-equal? kmp-ref-trace kmp-trace)
      )))
