;; life game

(use srfi-1)
(use srfi-9) ; define-record-type
(use srfi-27) ; random-integer
(use util.list) ; slices
(use gauche.parameter)
(use liv.matrix) ; *-matrix, matrix-*, cell, point, ...
(use liv.lists) ; list-repeat
(use liv.lol.dlambda) ; dlambda

(define (random-bit)
  (random-integer 2))

(define (make-random-bit-matrix w h)
  (make-matrix w h (lambda args (random-bit))))

(define (make-lifegame-table bit-matrix)
  (map-matrix-with-point (lambda (e p)
                           (let1 live? (complement zero?)
                             (make-cell p (live? e))))
                         bit-matrix))

(define (next-cell-value cell table)
  (let1 cnt-live (count cell-value (neighborhood-cells cell table))
    (if (cell-value cell)
        (<= 2 cnt-live 3)
        (= cnt-live 3))))

(define (point-xy p)
  (cons (point-x p)(point-y p)))

(define (next-lifegame-table table)
  (map-matrix (lambda (cell)
                (make-cell (cell-point cell)(next-cell-value cell table)))
              table))

(define (make-auto-step-lifegame w h . args)
  (let-optionals* args ((matrix (make-random-bit-matrix w h)))
    (let1 lg (lambda ()
               (make-lifegame-table matrix))
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
         (:previouse () prev))))))

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
                              (if (cell-value cell)
                                  (state-symbol-live ss)
                                  (state-symbol-dead ss)))
                            row)))
              table)))

(define (equal-lifegame? lg1 lg2)
  (let/cc hop
    (map (lambda (row1 row2)
           (map (lambda (e1 e2)
                  (rlet1 r (eq? (cell-value e1)(cell-value e2))
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

(define (const->auto-step-lifegame bit-matrix)
  (receive (w h)(matrix-size bit-matrix)
    (make-auto-step-lifegame  w h bit-matrix)))

