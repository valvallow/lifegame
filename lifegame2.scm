(use srfi-1) ; iota
(use srfi-27) ; randam-integer
(use srfi-43) ; vector-map

(define (random-bit)
  (random-integer 2))

(define (index->xy idx len)
  (receive (y x)(quotient&remainder idx len)
    (values x y)))

(define *relatives*
  '((-1 . 1)(0 . 1)(1 . 1)(-1 . 0)
    (1 . 0)(-1 . -1)(0 . -1)(1 . -1)))

(define (neighborhood-relative-xy idx len
                                  :optional (relatives *relatives*))
  (receive (x y)(index->xy idx len)
    (map (lambda (r)
           (let ((rx (+ x (car r)))(ry (+ y (cdr r)))
                 (f (lambda (x)
                      (remainder (+ x len) len))))
             (cons (f rx)(f ry))))
         relatives)))

(define (neighborhood-relative-indices idx len)
  (let1 rel (neighborhood-relative-xy idx len)
    (map (lambda (r)
           (let ((rx (car r))(ry (cdr r)))
             (+ rx (* ry len))))
         rel)))

(define (neighborhood-indices len)
  (rlet1 vect (make-vector (square len))
         (vector-map! (lambda (idx v)
                        (neighborhood-relative-indices idx len))
                      vect)))

(define (lifegame-next-step life neighborhood)
  (vector-map (lambda (idx e nh)
                (let1 cnt (count identity (map (lambda (e)
                                                 (vector-ref life e)) nh))
                  (if e
                      (<= 2 cnt 3)
                      (= cnt 3)))) life neighborhood))

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

(define (lifegame-auto-step life :optional(step 20)(sleep 1))
  (let rec ((step step))
    (unless (zero? step)
      (life)
      (flush)
      (sys-sleep sleep)
      (rec (dec step)))))

(define (bit->bool ls)
  (map (complement zero?) ls))

(define (lifegame-print-console life :optional (sym (cons '@ '_)))
  (let1 width (floor->exact (sqrt (vector-length life)))
    (newline)
    (vector-map (lambda (idx e)
                  (display ((if e car cdr) sym))
                  (when (zero? (remainder idx width))
                    (newline)))
                life)))

;; ------------------------------------------------------------
;;  test
;; ------------------------------------------------------------

(define (lifegame-random-life :optional (size 10))
  (list->vector (map (lambda (e)
                       (zero? (random-bit)))
                     (iota (square size)))))

(define lifegame
  (lifegame-stepper
   :life (lifegame-random-life 30)
    :printer
     (lambda (l)
       (lifegame-print-console l)
       (newline))))

(lifegame-auto-step lifegame 50)

