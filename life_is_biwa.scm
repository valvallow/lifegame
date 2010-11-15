;; (use srfi-1) ; iota
;; (use srfi-27) ; randam-integer
;; (use srfi-43) ; vector-map

(define *relatives*
  '((-1 . 1)(0 . 1)(1 . 1)(-1 . 0) ;; (0 . 0)
    (1 . 0)(-1 . -1)(0 . -1)(1 . -1)))

(define (dec n)
  (- n 1))

(define (random-bit)
  (random-integer 2))

(define (complement pred)
  (lambda args
    (not (apply pred args))))

(define (bit->bool bit)
  ((complement zero?) bit))

(define (square-edge square)
  (sqrt (vector-length square)))

(define (index->xy idx edge)
  (let ((m (mod idx edge))
        (q (div idx edge)))
    (values m q)))

(define (xy->index x y edge)
  (+ x (* y edge)))

(define (sphere n edge)
  (mod (+ n edge) edge))

(define (neighbor-relative-xy idx edge relatives)
  (call-with-values (lambda ()
                      (index->xy idx edge))
    (lambda (x y)
      (map (lambda (r)
             (let ((rx (+ x (car r)))
                   (ry (+ y (cdr r))))
               (cons (sphere rx edge)
                     (sphere ry edge))))
           relatives))))

(define (neighbor-relative-indices idx edge relatives)
  (let1 rel (neighbor-relative-xy idx edge relatives)
    (map (lambda (r)
           (xy->index (car r)(cdr r) edge))
         rel)))

(define-macro (rlet1 var exp . body)
  `(let ((,var ,exp))
     (unquote-splicing body)
     ,var))

(define (square x)
  (* x x))

(define (make-neighbor-indices edge relatives)
  (let1 idx 0
    (vector-map
     (lambda (v)
       (rlet1 r (neighbor-relative-indices idx edge relatives)
              (set! idx (+ idx 1))))
     (make-vector (square edge)))))


;; ------------------------------------------------------------
;;  lifegame
;; ------------------------------------------------------------

(define (pa$ proc . params)
  (lambda args
    (apply proc (append params args))))

(define (lifegame:neighbor lifegame neighbor)
  (map (pa$ vector-ref lifegame) neighbor))

(define (lifegame:random-life edge)
  (list->vector (map (lambda (e)
                       (bit->bool (random-bit)))
                     (iota (square edge)))))

(define (count pred ls)
  (let rec ((ls ls)(acc 0))
    (if (null? ls)
        acc
        (rec (cdr ls)(+ acc
                        (if (pred (car ls))
                            1
                            0))))))

(define (lifegame:live? life neighbor-life)
  (let1 cnt (count identity neighbor-life)
    (if life
        (<= 2 cnt 3)
        (= cnt 3))))

(define (lifegame:next-step lifegame neighbor)
  (vector-map (lambda (e nh)
                (lifegame:live? e (lifegame:neighbor lifegame nh)))
              lifegame neighbor))

(define (lifegame:make-stepper lifegame relatives)
  (let1 nh (make-neighbor-indices (square-edge lifegame) relatives)
    (lambda _
      (rlet1 r (lifegame:next-step lifegame nh)
             (set! lifegame r)))))

(define (lifegame:auto-step lifegame relatives step before after finally)
  (let1 next (lifegame:make-stepper lifegame relatives)
    (let rec ((l lifegame)(step step))
      (if (zero? step)
          (finally l)
          (begin
            (before l)
            (let1 r (next)
              (after r)
              (rec r (dec step))))))))

(define sym (cons '@ '_))

(define (lifegame:make-console-printer sym display)
  (lambda (lifegame)
    (let ((edge (square-edge lifegame))
          (idx 0))
      (vector-for-each (lambda (e)
                         (when (zero? (mod idx edge))
                           (newline))
                         (display ((if e car cdr) sym))
                         (set! idx (+ idx 1)))
                       lifegame)
      (newline))))


;; ------------------------------------------------------------
;;  test
;; ------------------------------------------------------------

(define game (lifegame:random-life 10))

;; (define printer (lifegame:make-console-printer (cons '@ '_) display))

(define printer (lifegame:make-console-printer
                 (cons '@ '_)
                 (lambda (val)
                   (element-update! ($ "lifegame-console") val))))

(define (start-lifegame)
  (lifegame:auto-step game *relatives* 10 printer
                      (lambda _ (sleep 1))
                      (lambda (l)
                        (set! game l))))

