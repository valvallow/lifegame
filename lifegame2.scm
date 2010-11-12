(use srfi-1) ; iota
(use srfi-27) ; randam-integer
(use srfi-43) ; vector-map

(define console-symbol (cons '@  '_))

(define (random-bit)
  (random-integer 2))

(define-syntax define-same-params
  (syntax-rules ()
    ((_ (params ...)(name body ...))
     (define (name params ...)
       body ...))
    ((_ (params ...)(name body ...) x ...)
     (begin
       (define-same-params (params ...)(name body ...))
       (define-same-params (params ...) x ...)
       (undefined)))))

(define-same-params (idx square-width)
  (edge-top? (<= idx (dec square-width)))
  (edge-bottom? (<= (- (square square-width) square-width) idx))
  (edge-left? (zero? (remainder idx square-width)))
  (edge-right?  (= (remainder idx square-width)(dec square-width)))
  (corner-top-left? (zero? idx))
  (corner-top-right? (=  (dec square-width) idx))
  (corner-bottom-left? (= (- (square square-width) square-width) idx))
  (corner-bottom-right? (= (dec (square square-width)) idx)))

(define (neighborhood-indices square-width)
  (let* ((square-size (square square-width))
         (v (make-vector square-size))
         (row-max (dec square-width))
         (square-max (dec square-size)))
    (vector-map
     (lambda (idx e)
       (cond ((corner-top-left? idx square-width) ; idx -> (x y) -> (0 0)
              (list square-max ; (-1 -1)
                    (- square-max row-max) ; (0 -1)
                    (inc (- square-max row-max)) ; (1 -1)
                    row-max ; (-1 0)
                    (inc idx) ; (1 0)
                    (inc (+ row-max row-max)) ; (-1 -1)
                    (inc row-max) ; (-1 0)
                    (inc (inc row-max))))
             ((corner-top-right? idx square-width)
              (list (dec (dec square-size))
                    square-max
                    (- (dec square-size) idx)
                    (dec idx)
                    0
                    (+ idx idx)
                    (+ idx (inc idx))
                    (inc idx)))
             ((corner-bottom-left? idx square-width)
              (list (dec idx)
                    (- idx square-width)
                    (- idx row-max)
                    square-max
                    (inc idx)
                    row-max
                    0
                    1))
             ((corner-bottom-right? idx square-width)
              (list (dec (- idx square-width))
                    (- idx square-width)
                    (- idx square-width row-max)
                    (dec idx)
                    (- idx row-max)
                    (dec row-max)
                    row-max
                    0))
             ((edge-top? idx square-width)
              (list (- square-size (- square-width idx))
                    (dec (- square-size (- square-width idx)))
                    (inc (- square-size (- square-width idx)))
                    (dec idx)
                    (inc idx)
                    (+ idx row-max)
                    (inc (+ idx row-max))
                    (inc (inc (+ idx row-max)))))
             ((edge-bottom? idx square-width)
              (list (dec (- idx square-width))
                    (- idx square-width)
                    (- idx row-max)
                    (dec idx)
                    (inc idx)
                    (dec (- row-max (- square-max idx)))
                    (- row-max (- square-max idx))
                    (inc (- row-max (- square-max idx)))))
             ((edge-left? idx square-width)
              (list (dec idx)
                    (- idx square-width)
                    (- idx row-max)
                    (+ idx row-max)
                    (inc idx)
                    (+ idx row-max square-width)
                    (+ idx square-width)
                    (inc (+ idx square-width))))
             ((edge-right? idx square-width)
              (list (dec (- idx square-width))
                    (- idx square-width)
                    (- idx square-width row-max)
                    (dec idx)
                    (- idx row-max)
                    (+ idx row-max)
                    (+ idx square-width)
                    (inc idx)))
             (else (list (dec (- idx square-width))
                        (- idx square-width)
                        (inc (- idx square-width))
                        (dec idx)
                        (inc idx)
                        (dec (+ idx square-width))
                        (+ idx square-width)
                        (inc (+ idx square-width))))))
      v)))

(define (lifegame-next-step life neighborhood)
  (vector-map (lambda (idx e nh)
                (let1 cnt (count identity (map (lambda (e)
                                                 (vector-ref life e)) nh))
                  (if e
                      (<= 2 cnt 3)
                      (= cnt 3)))) life neighborhood))

(define (lifegame-random-life :optional (size 10))
  (list->vector (map (lambda (e)
                       (zero? (random-bit)))
                     (iota (square size)))))

(define (lifegame-print-console life)
  (let1 width (floor->exact (sqrt (vector-length life)))
    (vector-for-each (lambda (idx e)
                        (when (zero? (remainder idx width))
                          (newline))
                        (display ((if e car cdr) console-symbol))) life)))

(define (lifegame-stepper :key
                       (life (lifegame-random-life))
                       (printer identity)
                       (return #f))
  (let* ((width (floor->exact (sqrt (vector-length life))))
         (nh (neighborhood-indices width))
         (life life))
    (lambda _
       (let1 r (lifegame-next-step life nh)
         (printer r)
         (set! life r)
         (if return
             r
             (values))))))

(define lifegame
  (lifegame-stepper
   :life (lifegame-random-life 30)
   :printer
   (lambda (l)
    (lifegame-print-console l)
    (newline))))

(lifegame)



