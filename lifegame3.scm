#!/usr/local/bin/gosh

(use gauche.parseopt)
(use gauche.process)
(use srfi-1)
(use srfi-27)

;; Exit the game by pressing the C-c
(set-signal-handler! SIGINT (^ _ (exit)))

;; initialize random-integer
(random-source-randomize! default-random-source)


;;
;; util
;;
(define (get-tput-val . args)
  (process-output->string `(tput ,@args)))

(define (tput . args)
  (run-process `(tput ,@args)))

(define (return-to-top)
  (tput 'cup 0 0))

(define (with-full-screen thunk)
  (dynamic-wind
    (^ _ (tput 'civis)(tput 'clear))
    thunk
    (^ _ (tput 'cnorm))))

(define (sleep-milliseconds n)
  (sys-nanosleep (* n 1000000)))

(define (random-bit)
  (random-integer 2))

(define (list-set! ls n obj)
  (let/cc hop
    (let rec ((l ls)(n n))
      (if (zero? n)
          (begin (set! (car l) obj)
                 (hop ls))
          (rec (cdr l)(- n 1))))))
(set! (setter list-ref) list-set!)

;; 生きているcellのインデックスを集める
(define (filter-alive world :optional (alive? (complement zero?)))
  (filter-map (^(cell index)
                (and (alive? cell) index))
              world
              (iota (length world))))

(define (create-random-life-world world-size)
  (list-tabulate world-size (^ _ (random-bit))))

(define (create-empty-world world-size)
  (list-tabulate world-size (^ _ 0)))

;; 現在のインデックスとトーラス的に隣合うインデックスを取得
(define (neighbor-indices index cols lines)
  (define (make-point x y)
    (cons x y))
  (define (point-x p)
    (car p))
  (define (point-y p)
    (cdr p))
  (define (add-point p1 p2)
    (make-point
     (+ (point-x p1)(point-x p2))
     (+ (point-y p1)(point-y p2))))
  (define (index->point index cols)
    (receive (y x)(quotient&remainder index cols)
      (make-point x y)))
  (define (point->index x y cols)
    (+ x (* y cols)))
  (define (replace-edge-point modulus intersection)
    (remainder (+ modulus intersection) intersection))
  (define (neighbor-locations index cols lines)
    (define relative-locations
      '((-1 . 1)(0 . 1)(1 . 1)(-1 . 0)
        (1 . 0)(-1 . -1)(0 . -1)(1 . -1)))
    (let1 base-point (index->point index cols)
        (map (^r (let1 rp (add-point base-point r)
                   (make-point (replace-edge-point (point-x rp) cols)
                               (replace-edge-point (point-y rp) lines))))
             relative-locations)))
  (map (^n (point->index (point-x n)(point-y n) cols))
       (neighbor-locations index  cols lines)))

(define-constant +LIFE+ 10)
(define-constant +BIRTH+ 3)
(define-constant +NEIGHBOR-ALLIVE-2+ 12)
(define-constant +NEIGHBOR-ALLIVE-3+ 13)

(define (alive? cell)
  (any (pa$ = cell)
       (list +BIRTH+ +NEIGHBOR-ALLIVE-2+ +NEIGHBOR-ALLIVE-3+)))

(define (print-world world cols)
  (for-each print (map list->string
                       (slices (map (^(cell)
                                      (if (alive? cell)
                                          #\0
                                          #\_))
                                    world)
                               cols)))
  (flush))

(define (usage cmd)
  (print "usage: " cmd " [option] ... [file]")
  (print " options:")
  (print "   h|help print this help")
  (print  "  c|columns" 100)
  (print  "  l|lines" 30)
  (print  "  s|sleep" 10)
  (print "Exit the game by pressing the C-c")
  (exit))

(define (main args)
  (let-args (cdr args)
      ((help "h|help" => (cut usage (car args)))
       (cols "c|columns=i" (x->integer (get-tput-val 'cols)))
       (lines "l|lines=i" (x->integer (get-tput-val 'lines)))
       (sleep "s|sleep=i" 10)
       (enter2step? "e|enter-step")
       (else (opt . _)
             (print "Unknown option : " opt)
             (usage (car args)))
       . rest)
    (let* ((world-size (* cols lines))
           (world (create-random-life-world world-size))
           (alives (filter-alive world)))
      (print world)
      (with-full-screen
       (^ _ (let rec ((alives alives))
              (return-to-top)
              (let1 world (create-empty-world world-size)
                (dolist (alive alives)
                  (for-each (^(neighbor)
                              (inc! (list-ref world neighbor)))
                            (neighbor-indices alive cols lines))
                  (set! (~ world alive)(+ (~ world alive) +LIFE+)))
                (print-world world cols)
                (if enter2step?
                    (read-line)
                    (sleep-milliseconds sleep))
                (rec (filter-alive world alive?)))))))))
