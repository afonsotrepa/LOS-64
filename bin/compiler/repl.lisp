(include "bin/compiler/eval.lisp")
;(block test (write-string "321")  (return-from test 123) )
;(progn (write-string "123") (write-string "abc"))
;((lambda () 123))
;(defun test () (return-from test (null t)))
;(test)
;(set 'test 1488)
;(let ((abc 123)) (setl 'test 321) test)
;(write-string (read-line))
;(write-string (set-char (make-string 100 #\a) 10 #\B))
;(let ((test 321)) (setl 'test 123))
;(write-char (char "test" 0))
;(write-string (car (read-token-from-string "\"qwe\"(test 123)" 0)))
;(car (set-car (cons 14 88) 123))
;(caddr (list-reverse '(1 2 3)))
;(parse-word "123")
;(write-string (car (read-token-from-string "qweqweqweqwe" 0)))
;(caar (car (parse-list '("(" "(" "321" ")" "123" "1488" ")" "test"))))
;(caar (read-from-string "(123)"))
;(car (read-from-string "123"))
;(defmacro when (test &body conseqs)
  ;`(if ,test (progn ,@conseqs)) 
;(when t (write-string "TEST~%") 123)
;(dolist (x '(#\1 #\2 #\3)) 
  ;(print-char x) 
;(print (read-from-string "'(14 88)"))
;(print (eval '(car (quote (1 2 3)))))
;(print (car (read-from-string (read-line))))
;(print (read-from-string "(car (cons 1 2))"))
;;read-line seems to have problems at moderate typing speeds
;(loop (write-char (read-char)))
;(do 
  ;((i 0 (add i 1)) 
   ;(e 123)) 
  ;((eq 10 i) 321) 
  ;(print i) 
  ;(print e))
;(set 'i (cons 14 88))
;(asm "mov rdi, rax
;      sub rdi, 0b001
;      mov rsi, [alist_ptr]
;      extern gc_in_root
;      call gc_in_root")

;;On hold:
;(defun apply (func args)
  ;(write-string "TEST")
  ;args
  ;(asm "mov rsi, rax
        ;push rsi")
  ;func
  ;(asm "pop rsi
        ;mov rdi, rax")
  ;(asm "extern APPLY
        ;call APPLY"))

;;REPL:
(defun repl ()
  (let ((exprs NIL) (len 0) (i 0))
    (loop 
      (write-char #\>) 
      (setq exprs (read))
      (setq len (list-length exprs))
      (setq i 0)
      (dolist (expr exprs)
        (setq i (add 1 i))
        (if (eq len i) 
          (print (eval expr))
          (eval expr))))))
(repl)

;(loop (cons 1 2)
;      (write-char #\M))


;;BUG:
;;Need to put the args passed to primitives somewhere that lisp_alloc can check to avoid reusing that area of memory
;(do ((i 0 (add i 1)))
;  ((eq i 10000) i)
;  (cons 14 88)
;  (write-char #\Z))
