;; life game

(use srfi-1)
(use srfi-9) ; define-record-type
(use srfi-27) ; random-integer
(use util.list) ; slices
(use gauche.parameter)

(define (make-matrix w h . keys)
  (let-optionals* keys ((seed-fun identity))
    (let1 size (* w h)
      (slices (list-tabulate size seed-fun) w))))

(define (map-matrix proc matrix)
  (map (pa$ map proc) matrix))

(define (ref-matrix matrix x y)
  (list-ref (list-ref matrix y) x))

(define (matrix-size matrix)
  (values (length matrix)(length (car matrix))))

(define (random-bit)
  (random-integer 2))

(define (make-random-bit-matrix w h)
  (make-matrix w h (lambda args (random-bit))))

(define-record-type point
  (make-point x y) point?
  (x point-x)
  (y point-y))

(define-record-type cell
  (make-cell point live?) cell?
  (point cell-point)
  (live? cell-live?))

(define (map-matrix-with-point proc matrix)
  (let ((x 0)(y 0))
    (map (lambda (row)
           (set! x 0)
           (rlet1 r (map (lambda (e)
                           (rlet1 r (proc e (make-point x y))
                                  (inc! x)))
                         row)
                  (inc! y)))
         matrix)))

(define (make-lifegame-table bit-matrix)
  (map-matrix-with-point (lambda (e p)
                           (let1 live? (complement zero?)
                             (make-cell p (live? e))))
                         bit-matrix))

(define (ref-lifegame-table table p)
  (ref-matrix table (point-x p)(point-y p)))

(define (negative-point? p)
  (any negative? (list (point-x p)(point-y p))))

(define (point-hold? p table)
  (and (not (negative-point? p))
       (receive (w h)(matrix-size table)
         (and (< (point-x p) w)
              (< (point-y p) h)))))

(define-constant RELATIVES
  `((-1 1)(0 1)(1 1)
    (-1 0)(1 0)
    (-1 -1)(0 -1)(1 -1)))

(define (add-point p1 p2)
  (make-point (+ (point-x p1)
                 (point-x p2))
              (+ (point-y p1)
                 (point-y p2))))

(define (list-repeat n obj)
  (list-tabulate n (lambda args obj)))

(define (neighborhood-points cell table)
  (filter (cut point-hold? <> table)
          (map (lambda (xy p)
                 (add-point p (make-point (car xy)(cadr xy))))
               RELATIVES
               (list-repeat (length RELATIVES)(cell-point cell)))))

(define (neighborhood-cells cell table)
  (let1 np (neighborhood-points cell table)
    (map (lambda (p)
           (ref-lifegame-table table p)) np)))

(define (next-cell-live? cell table)
  (let1 cnt-live (count cell-live? (neighborhood-cells cell table))
    (if (cell-live? cell)
        (<= 2 cnt-live 3)
        (= cnt-live 3))))

(define (point-xy p)
  (cons (point-x p)(point-y p)))

(define (next-lifegame-table table)
  (map-matrix (lambda (cell)
                (make-cell (cell-point cell)(next-cell-live? cell table)))
              table))

(define-syntax dlambda
  (syntax-rules (else)
    ((_ (msg1 (darg1 ...) dbody1 ...)(msg2 (darg2 ...) dbody2 ...) ...)
     (lambda (key . args)
       (case key
         ((msg1)(apply (lambda (darg1 ...)
                        dbody1 ...) args))
         ((msg2)(apply (lambda (darg2 ...)
                        dbody2 ...) args))
         ...
         (else key))
       ))))

(define (make-auto-step-lifegame size)
  (let1 lg (lambda ()
             (make-lifegame-table (make-random-bit-matrix size size)))
    (let ((cur (lg))(prev '()))
      (dlambda
       (:reset ()
               (set! cur (lg))
               cur)
       (:next ()
              (set! prev cur)
              (rlet1 r (next-lifegame-table cur)
                     (set! cur r)))
       (:current () cur)
       (:previouse () prev)))))

(define-record-type state-symbol
  (make-state-symbol live dead) state-symbol?
  (live state-symbol-live)
  (dead state-symbol-dead))

(define-constant DEFAULT_STATE_SYMBOL
  (make-state-symbol 'œ '›))

(define *state-symbol* (make-parameter DEFAULT_STATE_SYMBOL))

(define (print-lifegame-table table)
  (let1 ss (*state-symbol*)
    (newline)
    (for-each (lambda (row)
                (print (map (lambda (cell)
                              (if (cell-live? cell)
                                  (state-symbol-live ss)
                                  (state-symbol-dead ss)))
                            row)))
              table)))

(define (equal-lifegame? lg1 lg2)
  (let/cc hop
    (map (lambda (row1 row2)
           (map (lambda (e1 e2)
                  (rlet1 r (eq? (cell-live? e1)(cell-live? e2))
                         (if (not r)
                             (hop r))))
                row1 row2)) lg1 lg2)))

(define (endless-repeat-lifegame lifegame . args)
  (let-optionals* args ((printer print-lifegame-table))
    (lambda ()
      (printer
       (rlet1 r (lifegame :next)
              (when (equal-lifegame? (lifegame :previouse)(lifegame :current))
                (print 'restart)
                (lifegame :reset)))))))


;; test
(define lifegame (make-auto-step-lifegame 10))
(print-lifegame-table  (lifegame :next))


(define erl (endless-repeat-lifegame (make-auto-step-lifegame 30)))
(erl)


