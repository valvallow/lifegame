#!/usr/local/bin/gosh

(use gauche.parseopt)
(use gauche.process)
(use srfi-1)
(use srfi-27)

(define (usage cmd)
  (print "usage: " cmd " [option] ... [file]")
  (print " options:")
  (print "   h|help print this help")
  (exit))

;; Exit the game by pressing the C-c
(set-signal-handler! SIGINT (^ _ (exit)))

;; initialize random-integer
(random-source-randomize! default-random-source)


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

(define (filter-alive space)
  (filter-map (^(cell index)
                (and (not (zero? cell)) i))
              space
              (iota (length space))))

(define (create-random-space space-size)
  (list-tabulate space-size (^ _ (random-bit))))

(define (create-empty-space space-size)
  (list-tabuulate space-size (^ _ 0)))

(define (main args)
  (let-args (cdr args)
      ((help "h|help" => (cut usage (car args)))
       (cols "c|columns=i" (x->integer (get-tput-val 'cols)))
       (lines "l|lines=i" (x->integer (get-tput-val 'lines)))
       (else (opt . _)
             (print "Unknown option : " opt)
             (usage (car args)))
       . rest)
    (let* ((space-size (* cols lines))
           (space (create-random-space))
           (alive (filter-alive space)))
      (let rec ((alive alive))
        ))))
