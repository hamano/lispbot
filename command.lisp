
(require :cl-irc)

(defun hello-command (msg)
  (progn (format t "COMMAND: hello~%")
         (irc:privmsg (irc:connection msg)
                      (car (irc:arguments msg))
                      (format nil "Hello, ~A." (irc:source msg))
                      )))

(defun help-command (msg)
  (let ((conn (irc:connection msg)))
  (progn (format t "COMMAND: help~%")
         (irc:privmsg conn (car (irc:arguments msg)) "usage:")
         (irc:privmsg conn (car (irc:arguments msg)) "  (help)")
         (irc:privmsg conn (car (irc:arguments msg)) "  (hello)")
         (irc:privmsg conn (car (irc:arguments msg)) "  (reload)")
         (irc:privmsg conn (car (irc:arguments msg)) "  (join \"#channel\")")
         )))

(defun debug-command (msg)
  (let ((conn (irc:connection msg))
        (from (car (irc:arguments msg))))
  (progn (format t "COMMAND: debug~%")
         (irc:privmsg conn from "debug:")
         (irc:privmsg conn (car (irc:arguments msg))
                      (format nil "  conn*: ~A" conn))
         (irc:privmsg conn (car (irc:arguments msg))
                      (format nil "  msg: ~A" msg))
         )))

(defun join-command (msg expr)
  (let ((conn (irc:connection msg)))
  (progn (format t "COMMAND: join~%")
         (if (= (length expr) 2)
             (irc:join conn (second expr))))))

(defun reload-command (msg)
  (let ((conn (irc:connection msg)))
  (progn (format t "COMMAND: reload~%")
         (load "command")
         (load "config")
         (irc:privmsg conn (car (irc:arguments msg)) "reloaded.")
         )))


(defparameter safe-symbol-list
  (append
   '(nil t)
   '(+ - * / 1+ 1- conjugate gcd lcm)
   '(abs acos acosh asin asinh atan atanh cis cos cosh sin sinh tan tanh)
   '(exp isqrt log phase pi signum sqrt)
   '(= /= < <= > >= eq eql evenp oddp plusp minusp zerop max min)
   '(if cond)
   '(and or not)
   '(atom listp symbolp numberp null)
   '(cons car cdr list append member reverse nth nthcdr length)
   '(first second third last)
   '(let do)
   '(quote)
   '(cadr)
   '(1+ +1)
   '(reduce)
   ))

(defun safe-symbol? (lst)
  (cond
   ((null lst) t)
   ((typep (car lst) 'symbol)
    (and (member (car lst) safe-symbol-list) (safe-symbol? (cdr lst))))
   ((listp (car lst))
    (and (safe-symbol? (car lst)) (safe-symbol? (cdr lst))))
   (t (safe-symbol? (cdr lst)))))

(defun eval-command (msg expr)
  (let ((conn (irc:connection msg)))
    (progn (format t "COMMAND: eval")
           (irc:privmsg conn (car (irc:arguments msg))
                        (format nil "~A" (ignore-errors (eval expr)))))))