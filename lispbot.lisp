#!/usr/bin/sbcl --load
(require :cl-irc)
(require :iconv)
(load "config")
(load "command")

(defun jis2utf8 (str)
  (sb-ext:octets-to-string
   (iconv:iconv "ISO-2022-JP" "UTF-8"
                (sb-ext:string-to-octets str :external-format :ascii))
   :external-format :utf-8))

(defun utf82jis (str)
  (sb-ext:octets-to-string
   (iconv:iconv "UTF-8" "ISO-2022-JP"
                (sb-ext:string-to-octets str :external-format :utf-8))
   :external-format :ascii))

(defvar *connection*
  (irc:connect
   :nickname *irc-nick*
   :server *irc-host*
   :logging-stream nil))

(mapcar #'(lambda (channel) (irc:join *connection* channel)) *irc-channels*)

(defun join-hook (msg)
  (let ((conn (irc:connection msg))
        (channel (car (irc:arguments msg)))
        (from (irc:source msg)))
    (irc:op conn channel from)))

(defun print-hook (msg)
  (format t "~A <~A> ~A~%"
          (car (irc:arguments msg))
          (irc:source msg)
          (jis2utf8 (car (cdr (irc:arguments msg))))))

(defun command-hook (msg)
  (let ((text (jis2utf8 (second (irc:arguments msg)))))
    (if (or (char= (aref text 0) #\() (char= (aref text 0) #\'))
        (let ((expr (multiple-value-bind (x y)
                        (ignore-errors (read-from-string text))
                      (if x x y))))
          (format t "COMMAND HOOK:~%")
          (if (listp expr)
              (cond
               ((not (car expr)) (format t "warning: text is nil."))
               ((eq 'hello (car expr)) (hello-command msg))
               ((eq 'help (car expr)) (help-command msg))
               ((eq 'debug (car expr)) (debug-command msg))
               ((eq 'join (car expr)) (join-command msg expr))
               ((eq 'reload (car expr)) (reload-command msg))
               ((safe-symbol? expr) (eval-command msg expr))
               (t (irc:privmsg *connection*
                               (car (irc:arguments msg))
                               (utf82jis "シンボルが定義されていません。"))))
            (irc:privmsg *connection*
                         (car (irc:arguments msg))
                         (format nil "~A~%" expr))
          )))))

(irc:add-hook *connection* 'irc:irc-join-message #'join-hook)
;(irc:remove-hooks *connection* 'irc:irc-privmsg-message)
(irc:add-hook *connection* 'irc:irc-privmsg-message 'print-hook)
(irc:add-hook *connection* 'irc:irc-privmsg-message 'command-hook)

(irc:read-message-loop *connection*)
