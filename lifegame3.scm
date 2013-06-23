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

(define (file->world path)
  (call-with-input-file path
    (^p (let* ((line-list (port->list read-line p))
               (world (map (compose x->integer string)
                           (append-map string->list line-list)))
               (lines (length line-list)))
          (values world (quotient (length world) lines) lines)))))

(define (usage cmd)
  (print "usage: " cmd " [option] ... [file]")
  (print " options:")
  (print "   h|help         print this help")
  (print  "  c|columns      world width (default:80)")
  (print  "  l|lines        world height (default:25)")
  (print  "  s|sleep        Milliseconds waiting for the next phase (default:100)")
  (print "   f|fullscreen   full screen display")
  (print "   e|enter-step   enter key to the next phase")
  (print "Exit the game by pressing the C-c")
  (exit))

(define (main args)
  (let-args (cdr args)
      ((help "h|help" => (cut usage (car args)))
       (cols "c|columns=i" 80)
       (lines "l|lines=i" 25)
       (sleep "s|sleep=i" 100)
       (fullscreen? "f|fullscreen")
       (enter2step? "e|enter-step")
       (else (opt . _)
             (print "Unknown option : " opt)
             (usage (car args)))
       . rest)
    (receive (world cols lines)
        (if (null? rest)
            (let ((cols (if fullscreen? (x->integer (get-tput-val 'cols)) cols))
                  (lines (if fullscreen? (x->integer (get-tput-val 'lines)) lines)))
              (values (create-random-life-world (* cols lines)) cols lines))
            (file->world (car rest)))
      (let ((alives (filter-alive world)))
        (with-full-screen
         (^ _ (let rec ((alives alives))
                (return-to-top)
                (let1 world (create-empty-world (* cols lines))
                  (dolist (alive alives)
                    (for-each (^(neighbor)
                                (inc! (list-ref world neighbor)))
                              (neighbor-indices alive cols lines))
                    (set! (~ world alive)(+ (~ world alive) +LIFE+)))
                  (print-world world cols)
                  (if enter2step?
                      (read-line)
                      (sleep-milliseconds sleep))
                  (rec (filter-alive world alive?))))))))))
