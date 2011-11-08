#!/usr/local/bin/gosh

(use srfi-1) ; iota
(use srfi-27) ; randam-integer
(use srfi-43) ; vector-map
(use gauche.parseopt)
(use gauche.process)

(define *relatives*
  '((-1 . 1)(0 . 1)(1 . 1)(-1 . 0) ;; (0 . 0)
    (1 . 0)(-1 . -1)(0 . -1)(1 . -1)))

(define (square x)
  (* x x))

(define (dec n)
  (- n 1))

(define (random-bit)
  (random-integer 2))

(define (bit->bool bit)
  ((complement zero?) bit))

(define (square-edge square)
  (floor->exact (sqrt (vector-length square))))

(define (index->xy idx edge)
  (receive (y x)(quotient&remainder idx edge)
    (values x y)))

(define (xy->index x y edge)
  (+ x (* y edge)))

(define (sphere n edge)
  (remainder (+ n edge) edge))

(define (neighbor-relative-xy idx edge
                                  :optional (relatives *relatives*))
  (receive (x y)(index->xy idx edge)
    (map (lambda (r)
           (let ((rx (+ x (car r)))
                 (ry (+ y (cdr r))))
             (cons (sphere rx edge)
                   (sphere ry edge))))
         relatives)))

(define (neighbor-relative-indices idx edge)
  (let1 rel (neighbor-relative-xy idx edge)
    (map (lambda (r)
           (xy->index (car r)(cdr r) edge))
         rel)))

(define (make-neighbor-indices edge)
  (rlet1 vect (make-vector (square edge))
         (vector-map! (lambda (idx v)
                        (neighbor-relative-indices idx edge))
                      vect)))

;; ------------------------------------------------------------
;;  lifegame
;; ------------------------------------------------------------

(define (lifegame:neighbor lifegame neighbor)
  (map (pa$ vector-ref lifegame) neighbor))

(define (lifegame:random-life edge)
  (list->vector (map (lambda (e)
                       (bit->bool (random-bit)))
                     (iota (square edge)))))

(define (lifegame:live? life neighbor-life)
  (let1 cnt (count identity neighbor-life)
    (if life
        (<= 2 cnt 3)
        (= cnt 3))))

(define (lifegame:next-step lifegame neighbor)
  (vector-map (lambda (_ e nh)
                (lifegame:live? e (lifegame:neighbor lifegame nh)))
              lifegame neighbor))

(define (lifegame:make-stepper lifegame)
  (let* ((edge (square-edge lifegame))
         (nh (make-neighbor-indices edge)))
    (lambda _
      (rlet1 r (lifegame:next-step lifegame nh)
             (set! lifegame r)))))

(define (lifegame:auto-step lifegame :key
                            (step -1)
                            (before identity)(after identity)
                            (finally identity))
  (let1 next (lifegame:make-stepper lifegame)
    (let rec ((l lifegame)(step step))
      (if (zero? step)
          (finally l)
          (begin
            (before l)
            (let1 r (next)
              (after r)
              (rec r (dec step))))))))

(define (lifegame:make-console-printer :optional (sym (cons '@ '_)))
  (lambda (lifegame)
    (let1 edge (square-edge lifegame)
      (vector-for-each (lambda (idx e)
                         (when (zero? (remainder idx edge))
                           (newline))
                         (display ((if e car cdr) sym)))
                       lifegame)
      (newline)
      (flush))))


;; ;; ------------------------------------------------------------
;; ;;  test
;; ;; ------------------------------------------------------------

;; (define game (lifegame:random-life 30))
;; (lifegame:auto-step game
;;                     :before (lifegame:make-console-printer)
;;                     :after (lambda _ (sys-sleep 1))
;;                     :finally (lambda (l)
;;                                (set! game l)
;;                                (values)))

(define (main args)
  (let-args (cdr args)
      ((width "w|width=i" 10)
       (sleep "s|sleep=i" 300)
       (generation "g|generation=i" -1)
       (live "l|live=s")
       (dead "d|dead=s")
       (file "f|file=s")
    (let1 game (if file
                   (lifegame:random-life width) ; TODO : read from file
                   (lifegame:random-life width))
      (dynamic-wind
        (^ _ (run-process '(tput "civis"))
           (run-process '(tput "clear")))
        (^ _ (lifegame:auto-step
              game
              :step generation
              :before (if (and live dead)
                          (lifegame:make-console-printer (cons live dead))
                          (lifegame:make-console-printer))
              :after (^ _ (run-process '(tput "cup" 0 0))
                        (sys-nanosleep (* sleep 1000000)))
              :finally (^l (set! game l)
                           (values))))
        (^ _ (run-process '(tput "cnorm")))))))
