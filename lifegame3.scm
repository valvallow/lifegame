#!/usr/local/bin/gosh

(use gauche.parseopt)

(define (usage cmd)
  (print "usage: " cmd " [option] ...")
  (print " options:")
  (print "   h|help print this help")
  (exit))


(define (main args)
  (let-args (cdr args)
      ((help "h|help" => (cut usage (car args)))
       (else (opt . _)
             (print "Unknown option : " opt)
             (usage (car args)))
       . rest)
    (set-signal-handler! SIGINT (^ _ (exit)))))
