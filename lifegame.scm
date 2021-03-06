;; life game

(use srfi-1)
(use srfi-9) ; define-record-type
(use srfi-27) ; random-integer
(use util.list) ; slices
(use gauche.parameter)
(use liv.matrix) ; *-matrix, matrix-*, cell, point, ...
(use liv.lists) ; list-repeat
(use liv.point) ; point
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

(define-constant relatives
  `((-1 1)(0 1)(1 1)
    (-1 0)(1 0)
    (-1 -1)(0 -1)(1 -1)))

(define-record-type cell
  (make-cell p value) cell?
  (p cell-point)
  (value cell-value))

(define (matrix-ref-with-point matrix p)
  (matrix-ref matrix (point-x p)(point-y p)))

;; (define (map-matrix-with-point proc matrix)
;;   (let ((x 0)(y 0))
;;     (map (lambda (row)
;;            (set! x 0)
;;            (rlet1 r (map (lambda (e)
;;                            (rlet1 r (proc e (make-point x y))
;;                                   (inc! x)))
;;                          row)
;;                   (inc! y)))
;;          matrix)))

(define (map-matrix-with-point proc matrix)
  (map-matrix-with-index (lambda (e x y)
                           (proc e (make-point x y))) matrix))

(define (next-cell-value cell table)
  (let1 cnt-live (count cell-value (neighborhood-cells cell table))
    (if (cell-value cell)
        (<= 2 cnt-live 3)
        (= cnt-live 3))))

(define (point-hold? p matrix)
  (and (not (negative-point? p))
       (receive (w h)(matrix-size matrix)
         (and (< (point-x p) w)
              (< (point-y p) h)))))

(define (neighborhood-points cell matrix
                             :optional (relatives relatives))
  (filter (cut point-hold? <> matrix)
          (map (lambda (xy p)
                 (add-point p (make-point (car xy)(cadr xy))))
               relatives
               (list-repeat (length relatives)(cell-point cell)))))

(define (neighborhood-cells cell matrix)
  (let1 np (neighborhood-points cell matrix)
    (map (lambda (p)
           (matrix-ref-with-point matrix p)) np)))

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
  (make-state-symbol '@ 0))

(define *state-symbol* (make-parameter DEFAULT_STATE_SYMBOL))

;; (define (print-lifegame-table table)
;;   (let1 ss (*state-symbol*)
;;     (newline)
;;     (for-each (lambda (row)
;;                 (print (map (lambda (cell)
;;                               (if (cell-value cell)
;;                                   (state-symbol-live ss)
;;                                   (state-symbol-dead ss)))
;;                             row)))
;;               table)))
(define (print-lifegame-table table)
  (let1 ss (*state-symbol*)
    (newline)
    (print-matrix table :element-fun (lambda (cell)
                              (if (cell-value cell)
                                  (state-symbol-live ss)
                                  (state-symbol-dead ss))))))


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

