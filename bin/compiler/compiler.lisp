;;;Lisp cross-compiler written in CL for LispOS (for x86-64)
;;;r15 holds the lexical enviroment

(defmacro emit (string &rest rest)
  `(format out (concatenate 'string ,string "~%") ,@rest))

(defmacro emit-ret (&optional (return-value NIL return-value-supplied-p))
  (if return-value-supplied-p
    `(progn (emit "mov rax, ~A" ,return-value) (emit "ret"))
    `(emit "ret")))

(defun immediatep (e)
  (if (or (numberp e) (typep e 'character) (null e) (eq t e))
    t
    NIL))

(defconstant +primitives+
             '(add sub cons car cdr eq write-char make-string write-string
                   set-char string-eq quote if make-symbol set symbol-value
                   lambda symbol-lexical-value let symbol-name mul apply
                   cond atom symbolp error asm tagbody go setl read-char
                   add8 char string-length set-car set-cdr greater lesser
                   numberp log stringp rem log div expt charp functionp
                   ))

(defun primcallp (e)
  (if (not (listp e)) 
    NIL
    (if (position (car e) +primitives+)
      t
      NIL)))

(defvar *lexicalp* NIL) ;easier than passing it through all the emits
(defconstant +registers+ '(rdi rsi rdx rcx r8 r9))

(defconstant +fixnum-shift+ 2)
(defconstant +fixnum-tag+ #b00)
(defconstant +cons-shift+ 3)
(defconstant +cons-tag+ #b001)
(defconstant +char-shift+ 8)
(defconstant +char-tag+ #b00001111)
(defconstant +NIL+ #b00101111)
(defconstant +T+ #b100) ;temporary value for T (=1)
(defconstant +string-shift+ 3)
(defconstant +string-tag+ #b011)
(defconstant +symbol-shift+ 3)
(defconstant +symbol-tag+ #b101)
(defconstant +compiled-function-shift+ 3)
(defconstant +compiled-function-tag+ #b010) ;not set in stone, might change it!
(defun immediate-rep (e)
  (cond ((numberp e) (logior +fixnum-tag+ (ash e +fixnum-shift+)))
        ((typep e 'character)
         (logior +char-tag+ (ash (char-code e) +char-shift+)))
        ((null e) +NIL+)
        ((eq t e) +T+)
        (t NIL)))

(defmacro emit-immediate (e &optional &key (i 0 i-supplied-p) (safep NIL))
  `(progn
     ;;push the +registers+ if needed (safep == t)
     (when (and (>= ,i 0) ,safep (< ,i (length +registers+)))
       (emit "push ~a" (nth ,i +registers+)))
     (cond 
       ((immediatep ,e)
        (if ,i-supplied-p
          (if (>= ,i (length +registers+))
            (emit "push ~a" (immediate-rep ,e)) 
            (emit "mov ~a, ~a" (nth ,i +registers+) (immediate-rep ,e)))
          (emit "mov rax, ~a" (immediate-rep ,e))))
       (t NIL))))

(defun emit-string (string out)
  ;;Pass the string through format, to help with inputting special chars like
  (setf string (format NIL string))
  (loop 
    :with tag := (gensym)
    :with end := (gensym)
    :initially
    (emit "jmp ..@~a" end)
    (emit "align 8")
    (emit "..@~a:" tag)
    (emit "dq ~a" (immediate-rep (length string)))
    :for char :across string :do
    (emit "db ~a" (char-code char))
    :finally
    (emit "..@~a:" end)
    (emit "mov rax, ..@~a" tag)
    (emit "or rax, ~a" +string-tag+)
    ))

(defmacro emit-symbol (name &optional &key (value NIL))
  `(emit-call 'make-symbol (list ,name ,value) out))

(defmacro emit-variable (symbol)
  `(if (null *lexicalp*)
     (emit-call 'symbol-value (list `(quote ,,symbol)) out)
     (emit-call 'symbol-lexical-value (list `(quote ,,symbol)) out)))

(defmacro emit-list (list)
  `(loop :initially
         (emit ";;list")
         (emit "push rdi")
         (emit "push rsi")

         (emit "mov rax, ~a" +NIL+) ;end of the list
         :for e :in (reverse ,list)
         :do
         (emit "mov rsi, rax")
         (emit "push rsi")
         (emit-quote e out :safep NIL)
         (emit "mov rdi, rax")
         (emit "pop rsi")

         (emit "extern CONS")
         (emit "call CONS")
         :finally
         (emit "pop rsi")
         (emit "pop rdi")
         (emit ";;end of list")))

;;Compile the lambda form (instead of the runtime interpreter)
(defun emit-lambda (args exprs out)
  (let ((label (gensym)) (*lexicalp* t))
    (emit "jmp ~a.end" label) ;jump to after the function definition
    (emit "align 8") ;8bytes aligned to get the last 3 bits free for the tag
    ;;"reserve" space for the list holding the args
    (loop :for i :from 1 :to 8 :do (emit "nop"))
    (emit "~a:" label)
    (emit "ENTER")
    (loop :for expr :in exprs :do
          (emit-expr expr out :safep t))
    (emit "LEAVE")
    (emit "ret") ;the functions return point
    (emit ".end: mov rax, ~a" label) ;rax = address of the function
    (emit "or rax, ~a" +compiled-function-tag+) ;put the tag
    (emit "push rax")
    ;;make the list holding the args (symbols) and put it before the label
    (emit-list args)
    (emit "mov [~a-8], rax" label)
    (emit "pop rax")))

(defun emit-include (file-in out &optional &key (safep NIL))
  (let ((code NIL))
    (with-open-file (in file-in :direction :input)
      (setf code  (loop :for tmp = (read in NIL)
                        :until (eq tmp NIL)
                        :collect tmp)))
    (loop :for expr :in code :do (emit-expr expr out :safep safep))))

;;replaced by a macro
;(defun emit-defun (name args body out &optional &key (safep NIL))
  ;(emit-expr `(set ,`(quote ,name)
                   ;,(loop :for expr :in body
                         ;:with block := `(block ,name) :do
                         ;(setf block (append block (list expr)))
                         ;:finally (return `(lambda ,args ,block))))
             ;out :safep safep))

;;soon to be replaced by a macro
(defun emit-cond (clauses out &optional &key (safep NIL))
  (loop :for clause :in clauses
        :for next := (gensym)
        :with end := (gensym) :do
        (emit-expr (car clause) out :safep safep)
        (emit "cmp rax, ~a" +NIL+)
        (emit "je ..@~a" next)
        (loop :for expr :in (cdr clause) :do
              (emit-expr expr out :safep safep))
        (emit "jmp ..@~a" end)
        (emit "..@~a:" next)
        :finally
        (emit "..@~a:" end)))

;;Evaluates the arguments in the wrong order! (minor bug)
(defun emit-apply (function args out &optional &key (safep NIL))
  (loop
    :initially
    (when safep (emit "SAVE"))
    ;;make the list holding the args
    (emit "mov rax, ~a" +NIL+)
    :for arg :in (reverse args) :do
    (emit "push rax")
    (emit-expr arg out :safep NIL)
    (emit "mov rdi, rax")
    (emit "pop rsi")
    (emit "call CONS")
    :finally 
    ;;pass the args to apply and call it
    (emit "push rax")
    (emit-expr function out :safep NIL)
    (emit "mov rdi, rax")
    (emit "pop rsi")
    (emit "extern APPLY")
    (emit "call APPLY")
    (when safep (emit "LOAD"))))

(defun emit-let (bindings body out)
  (let ((*lexicalp* t))
   (loop :initially
         (emit "push r15") ;save r15
         (emit "mov rax, r15") ;add to the existing lexical enviroment
         :for binding :in (reverse bindings) :do
         (emit "push rax")

         (emit-expr (cadr binding) out :safep NIL)
         (emit "push rax")
         (emit-symbol (symbol-name (car binding)))
         (emit "pop rsi")
         (emit "mov rdi, rax")
         (emit "extern CONS")
         (emit "call CONS")

         (emit "mov rdi, rax")
         (emit "pop rsi") ;rsi = saved value of rax
         (emit "call CONS")
         :finally
         (emit "mov r15, rax") ;r15 = list holding the bindings
         (loop :for expr :in body :do
               (emit-expr expr out :safep NIL))
         (emit "pop r15"))))

(defun emit-quote (e out &optional &key (safep NIL))
  (cond ((or (null e) (eq t e)) (emit-immediate e :safep safep))
        ((symbolp e)
         (emit-symbol (symbol-name e)))
        ((listp e)
         (emit-list e))
        (t (emit-expr e out :safep safep))))

(defmacro emit-pass-arg (arg-to-pass &optional &key (safep NIL))
  `(if (>= i (length +registers+))
     (emit "push ~a" ,arg-to-pass)
     (if ,safep
       (progn
         (emit "push ~a" (nth i +registers+))
         (emit "mov ~a, ~a" (nth i +registers+) ,arg-to-pass))
       (emit "mov ~a, ~a" (nth i +registers+) ,arg-to-pass))))

(defmacro emit-clean-args (args)
  `(loop :for i
         :downfrom (1- (if (> (length +registers+) (length ,args))
                         (length ,args)
                         (length +registers+)))
         :to 0
         :do (emit "pop ~a" (nth i +registers+))))

(defmacro emit-arg-eval (arg i &optional &key (safep NIL))
  `(progn
     (emit-expr ,arg out :i ,i :safep ,safep)
     ;;symbols are immediate too, but are generated differently
     (when (or (not (immediatep ,arg))
               (and (not (eq 'NIL ,arg)) (not (eq 't ,arg)) (symbolp ,arg)))
       (emit-pass-arg "rax" :safep ,safep))))


(defun emit-call (func args out &optional &key (clearp t) (safep t))
  (when safep (emit "SAVE"))
  ;replace the "-" with "_", because nasm doesn't accept labels with "-"
  (setf func (make-symbol (substitute #\_ #\- (if (symbolp func)
                                                (symbol-name func)
                                                func))))
  ;;Pass the args to the stack (if needed)
  (when (> (length args) (length +registers+))
    (loop :for arg :in (reverse (subseq args (length +registers+)(length args)))
          :for i :from (length +registers+) :to (length args)
          :do (emit-arg-eval arg i :safep NIL)))
  ;;Pass the args to the registers (if needed)
  (when args
    (loop :for arg 
          :in (subseq args 0 (if (> (length +registers+) (length args))
                               (length args)
                               (length +registers+)))
          :for i :from 0 :to (length args)
          :do 
          (emit-arg-eval arg i :safep NIL)
          (when (not (or (eq i (1- (length args)))
                         (eq i (1- (length +registers+))))) 
            (emit "push ~a" (nth i +registers+))))
    (loop :for register :in (reverse (subseq +registers+ 0 
                                             (1- (if (> (length +registers+) 
                                                        (length args))
                                                   (length args)
                                                   (length +registers+)))))
          :do (emit "pop ~a" register)))
  ;;Declare the function to be called as external and call it
  (emit "extern ~A" func)
  (emit "call ~A" func)
  ;;Remove the pushed args
  (when (and clearp (>= (length args) (length +registers+)))
    (emit "add rsp, ~a" (* 8 (- (length args) (length +registers+)))))
  ;;Pop the saved registers from the stack
  ;(emit-clean-args args)
  (when safep (emit "LOAD")))

;;If is defined here, and not together with the other primitives, because it should only evaluate the test and the correct choice(conseq OR altern, never both)
(defun emit-if (test conseq altern out)
  (let ((false (gensym)) (true (gensym)) (exit (gensym)))
    (emit-expr test out :safep t)
    (emit "cmp rax, ~a" +NIL+)
    (emit "je ..@~a" false)
    (emit "..@~a:" true)
    (emit-expr conseq out :safep t)
    (emit "jmp ..@~a" exit)
    (emit "..@~a:" false)
    (emit-expr altern out :safep t)
    (emit "..@~a:" exit)))


(defvar *tags* NIL)
(defun emit-go (tag out)
  (when (null (cadr (assoc tag *tags*)))
    (error "~a, passed to go, isn't a defined tag name." tag))
  (emit "jmp ..@~a" (cadr (assoc tag *tags*))))

(defun emit-tagbody (statements out &optional &key (safep NIL))
  (loop :for statement :in statements
        :when (symbolp statement) :do
        (push `(,statement ,(gensym)) *tags*))
  (loop :initially 
        (emit ";;start of tagbody")
        (when safep 
          (emit "push rdi")
          (emit "push rsi"))
        :for statement :in statements
        :if (symbolp statement) :do 
        (emit "..@~a:" (cadr (assoc statement *tags*)))
        :else :do
        (emit-expr statement out)
        :finally 
        (when safep 
          (emit "pop rsi")
          (emit "pop rdi"))
        (emit ";;end of tagbody"))
  (loop :for statement :in statements
        :for label := (gensym)
        :when (symbolp statement) :do
        (pop *tags*)))

(defvar *blocks* NIL)
(defun emit-block (name forms out &optional &key (safep NIL))
  (when (not (symbolp name))
    (error "~a is an invalid block name." name))
  (let ((label (gensym)))
    (push `(,name ,label) *blocks*)
    (when safep (emit "SAVE"))
    (loop :for form :in forms
          :do (emit-expr form out))
    (emit "..@~a:" label)
    (when safep (emit "LOAD"))
    (pop *blocks*)))

(defun emit-return-from (name value out)
  (emit-expr value out)
  (when (null (assoc name *blocks*))
    (error "~a, passed to return-from, isn't a defined block name." name))
  (emit "jmp ..@~a" (cadr (assoc name *blocks*))))

(defun emit-progn (forms out &optional &key (safep NIL))
  (loop :for form :in forms
        :do (emit-expr form out :safep safep)))

(defvar *macros* NIL)
(defun emit-defmacro (name args body)
  (let ((rest-parameter 
          (if (and (position '&rest args) (position '&body args))
            (error "Expecting &rest or &body in emit-defmacro, not both")
            (if (position '&rest args)
              '&rest
              '&body))))
   (if (and (position rest-parameter args)
           (>= (1+ (position rest-parameter args)) (length args)))
    (error "Only expecting one argument afer &rest or &body in emit-defmacro")
    (push `(,name ,args ,body) *macros*))))

(defun emit-macrocall (name args out &optional &key (safep NIL))
  (let* ((macro (assoc name *macros*))
         (macro-args (cadr macro))
         (macro-body (caddr macro))
         (rest-parameter 
           (if (and (position '&rest args) (position '&body args))
             (error "Expecting &rest or &body in emit-defmacro, not both")
             (if (position '&rest args)
               '&rest
               '&body))))
    ;;put the args after &rest into a list
    (when (position rest-parameter macro-args)
      (setf args 
            (append (subseq args 0 (position rest-parameter macro-args))
                    `(,(loop :for arg 
                             :in (subseq args (position rest-parameter macro-args))
                             :collect arg)))))
    ;;quote the args so they won't get evaluated at compile time
    (loop :for arg :in args
          :do (setf (nth (position arg args) args) `(quote ,arg)))
    (emit-expr (eval 
                 `((lambda 
                     ,(remove rest-parameter macro-args) ,@macro-body) 
                       ,@args))
               out :safep safep)))

(defun emit-expr (e out &optional &key  (i 0 i-supplied-p) (safep NIL))
  (cond 
    ((immediatep e)
     (if i-supplied-p
       (emit-immediate e :i i :safep safep)
       (emit-immediate e :safep safep)))
    ((stringp e)
     (emit-string e out))
    ((symbolp e)
     (emit-variable e))
    ;;atoms should come before here or "(car e)" will give a type error
    ((eq (car e) 'if)
     (when (> (length e) 4) (error "IF statement too long"))
     (emit-if (cadr e) (caddr e) (cadddr e) out))
    ((eq (car e) 'quote)
     (emit-quote (cadr e) out))  
    ((eq (car e) 'lambda)
     (emit-lambda (cadr e) (cddr e) out))  
    ((eq (car e) 'let)
     (emit-let (cadr e) (cddr e) out))  
    ((eq (car e) 'cond)
     (emit-cond (cdr e) out :safep safep))
    ;((eq (car e) 'defun)
     ;(emit-defun (cadr e) (caddr e) (cdddr e) out :safep safep))
    ((eq (car e) 'include)
     (emit-include (cadr e) out :safep safep))
    ((eq (car e) 'asm)
     (emit (cadr e)))
    ((eq (car e) 'tagbody)
     (emit-tagbody (cdr e) out :safep safep))
    ((eq (car e) 'go)
     (emit-go (cadr e) out))
    ((eq (car e) 'block)
     (emit-block (cadr e) (cddr e) out :safep safep))
    ((eq (car e) 'return-from)
     (emit-return-from (cadr e) (caddr e) out))
    ((eq (car e) 'progn)
     (emit-progn (cdr e) out :safep safep))
    ((eq (car e) 'defmacro)
     (emit-defmacro (cadr e) (caddr e) (cdddr e)))
    ((primcallp e)
     (emit-call (car e) (cdr e) out :safep safep))
    (t (if (assoc (car e) *macros*)
         (emit-macrocall (car e) (cdr e) out :safep safep)
         (emit-apply (car e) (cdr e) out :safep safep)))))

(defun compiler (file-in file-out)
  (let ((code NIL))
    (with-open-file (in file-in :direction :input)
      (setf code  (loop :for tmp = (read in NIL)
                        :until (eq tmp NIL)
                        :collect tmp)))
    (print code)
    (with-open-file (out file-out :direction :output :if-exists :supersede)
      (emit "[Bits 64]")
      (emit "%include \"macros.s\"")
      ;;.data section
      (emit "section .data")
      (emit "repl_start: db \"REPL Initialize\",0x0a, 0")
      ;;.text section
      (emit "section .text")
      (emit "global repl_initialize")
      (emit "repl_initialize:")
      ;;initialize the heap
      ;(emit "extern lisp_heap_ptr")
      ;(emit "extern lisp_heap")
      ;(emit "mov rax, [lisp_heap]")
      ;(emit "mov [lisp_heap_ptr], rax")
      ;;initialize the alist
      (emit "extern alist_ptr")
      (emit "mov qword [alist_ptr], ~a" +NIL+) ;initialize the alist
      (emit "ENTER")
      ;;initialize the lexical enviroment 
      (emit "mov r15, ~a" +NIL+) 
      ;;print("REPL Initialize\n")
      (emit "mov rdi, repl_start")
      (emit "extern print")
      (emit "call print")

      (emit ";;Compiled code starts here")
      (loop :for expr :in code :do (emit-expr expr out :safep NIL))
      (emit ";;Compiled code ends here")
      
      ;;return rax (the value of the last evaluated expr)
      (emit "LEAVE")
      (emit-ret))))

(compiler (nth 1 sb-ext:*posix-argv*) "~/LOS-64/bin/lisp.s")
