(define *relatives*
  '((-1 . 1)(0 . 1)(1 . 1)(-1 . 0) ;; (0 . 0)
    (1 . 0)(-1 . -1)(0 . -1)(1 . -1)))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

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
          (finally l step)
          (begin
            (before l step)
            (let1 r (next)
              (after r step)
              (rec r (dec step))))))))

(define (lifegame:make-web-printer sym finally)
  (let1 newline (lambda (out)
                  (display "<br />" out))
    (lambda (lifegame step)
      (let ((edge (square-edge lifegame))
            (idx 0))
        (let1 s (call-with-output-string
                  (lambda (out)
                    (display step out)
                    (newline out)
                    (vector-for-each (lambda (e)
                                       (when (zero? (mod idx edge))
                                         (newline out))
                                       (display ((if e car cdr) sym) out)
                                       (set! idx (+ idx 1)))
                                     lifegame)
                    (newline out)))
          (finally s))))))

;; ------------------------------------------------------------
;;  test
;; ------------------------------------------------------------

(define (start-lifegame . args)
  (let ((live "<img border=\"0\" height=\"15\" src=\"https://sites.google.com/site/valloooooooooow/lisplogo_alien_128.png\" width=\"25\" />")
        (dead "<img border=\"0\" height=\"15\" src=\"\" width=\"25\" />")
        (step (string->number (get-content ($ "step"))))
        (interval (string->number (get-content ($ "interval"))))
        (size (string->number (get-content ($ "size"))))
        (console ($ "lifegame-console")))
  (let ((game (lifegame:random-life size))
        (printer (lifegame:make-web-printer
                  (cons live dead)
                  (lambda (val)
                    (element-update! console val)))))
    (lifegame:auto-step game *relatives* step printer
                        (lambda _ (sleep interval))
                        (lambda (l s)
                          (set! game l))))))

(add-handler! ($ "start") "click" start-lifegame)
