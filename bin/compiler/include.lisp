;;Initialize NIL and T (immediate-rep does it for compile-time only)
(set (make-symbol "NIL") NIL)
(set (make-symbol "t") t)
      
;;;Macros
(defmacro when (test &body conseqs)
  `(if ,test (progn ,@conseqs)))
(defmacro unless (test &body conseqs)
  `(when (not ,test) ,@conseqs))


(defmacro defun (name args &body body)
  `(set (quote ,name)
        (lambda ,args (block ,name ,@body))))

(defmacro return (value)
  `(return-from NIL ,value))

;(defmacro cond (&body clauses))

;;quick and dirty implementation of do
(defmacro do (varlist endlist &body body)
  (let ((start (gensym)) (end (gensym)))
    `(block 
       NIL
       (let (,@(loop :for var-form :in varlist
                    :collect (loop :for i :from 0 to 1
                                   :collect (car var-form)
                                   :do (setf var-form (cdr var-form)))))
         (tagbody
           (go ,end)
           ,start
           ,@body
           ;;step-forms
           ,@(loop :for var-form :in varlist
                   :when (caddr var-form)
                   :collect `(setq ,(car var-form) ,(caddr var-form)))
           ,end
           (unless ,(car endlist) (go ,start))
           (return (progn ,@(cdr endlist))))))))

;;This implementation of dolist doesn't make use of the do macro
(defmacro dolist (var-form &body body)
  (let ((list-symbol (gensym)))
    (let ((loop-symbol (gensym)))
      `(block 
         NIL
         ;;(car var-form) (the var) is declared in the let to make it lexical and, therefore, preserve it's dynamic or (former) lexical value
         (let ((,list-symbol ,(cadr var-form)) (,(car var-form) NIL))
           (tagbody 
             ,loop-symbol
             (when (null ,list-symbol)
               (return NIL))
             (setl ',(car var-form) (car ,list-symbol))
             ,@body
             (setl ',list-symbol (cdr ,list-symbol))
             (go ,loop-symbol)))))))

;;Basic loop macro:
(defmacro loop (&body body)
  `(block 
     NIL
     (tagbody 
       loop
       ,@body
       (go loop))))

(defmacro setq (symbol value)
  `(setl ',symbol ,value))

;(defmacro or (&rest forms)
  ;(let ((true (gensym)))
    ;`(let ((,true ,(car forms)))
       ;(if ,true
         ;,true
         ;,(if (null (cdr forms))
            ;NIL
            ;`(or ,@(cdr forms)))))))
;;more efficient but lower-level definition:
(defmacro or (&rest forms)
  `(if ,(car forms)
     (asm "nop") ;rax holds the value from the test already
     ,(if (null (cdr forms))
        NIl
        `(or ,@(cdr forms)))))


;;;Functions
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun caar (x) (car (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun cddar (x) (cdr (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun caaar (x) (car (car (car x))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddar (x) (car (cdr (cdr (cdr (car x))))))
(defun cadadar (x) (car (cdr (car (cdr (car x))))))
(defun caddddar (x) (car (cdr (cdr (cdr (cdr (car x)))))))


(defun null (x)
  (eq x NIL))

(defun and (x y)
  (cond (x (cond (y t) (t '())))
        (t '())))

(defun not (x)
  (cond (x '())
        (t t)))

;(defun consp (object)
  ;(not (atom object)))
;;defined as a macro for memory efficency (important in the gc)
(defmacro consp (obj)
  `(if (atom ,obj)
     NIL
     t))

(defun append (x y)
  (cond ((null x) y)
        (t (cons (car x) (append (cdr x) y)))))


(defun list (x y)
  (cons x (cons y '())))


(defun pair (x y)
  (cond ((and (null x) (null y)) '())
        ((and (consp x) (consp y))
         (cons (list (car x) (car y))
               (pair (cdr x) (cdr y))))))

(defun assoc (x y)
  (cond ((eq (car (car y)) x) (car (cdr (car y))))
        ('t (assoc x (cdr y)))))

(defun list-length (lst)
  (let ((n 0))
    (tagbody start
             (if (null lst)
               (return-from list-length n)
               (progn
                 (setl 'n (add n 1))
                 (setl 'lst (cdr lst))
                 (go start))))))

(defun list-reverse (lst)
  (list-reverse-aux lst nil))

(defun list-reverse-aux (lst aux)
  (if (null lst)
      aux
    (list-reverse-aux (cdr lst) (cons (car lst) aux))))

(defmacro push (obj lst)
  `(setq ,lst (cons ,obj ,lst)))
(defmacro pop (lst)
  `(let ((cr (car ,lst)))
     (setq ,lst (cdr ,lst))
     cr))

(defun list-position (obj lst)
  (let ((i 0))
   (dolist (item lst)
     (if (eq item obj) 
       (return-from list-position i)
       (setq i (add i 1))))
   NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Parser:

(defun read-line ()
  (let ((line NIL) (str "") (len 0))
    (loop 
      (setq line (cons (write-char (read-char)) line))
      (if (eq (car line) #\backspace)
        (setq line (cddr line))
        (when (eq (car line) #\newline)
          (tagbody 
            (setq line (cdr line)) ;pop #\newline
            (when (null line)
              (return-from read-line str)) ;return ""
            (setq len (list-length line))
            (setq str (make-string len (car line)))
            (setq len (sub len 1))
            loop
            (set-char str len (car line))
            (setq len (sub len 1))
            (setq line (cdr line))
            (if (null line)
              (return-from read-line str)
              (go loop))))))))

;;Returns a cons of the first token (string) and the index (fixnum) of it's last char
(defun read-token-from-string (str i)
  (let ((lst NIL) (token "") (chr #\0) (len (string-length str)) (cntr 0))
    (tagbody 
      loop
      ;;if at the end of str:
      (when (eq i len)
        (go make))
      (setl 'chr (char str i))
      ;;if end of a word:
      (when (eq chr #\space)
        (if (null lst)
          (progn 
            (setl 'i (add i 1)) 
            (go loop))
          (go make)))
      ;;if start/end of list or quote/backquote:
      (when (or (or (or (eq chr #\() (eq chr #\))) (eq chr #\')) (eq chr #\`))
        (if (null lst)
          (return-from read-token-from-string (cons (make-string 1 chr) (add 1 i)))
          (go make)))
      ;;if comma:
      (when (eq chr #\,)
        (error "Comma outside of backquoted expression in str passed to read-token-from-string"))
      ;;if start of a string:
      (when (eq chr #\")
        (when (not (null lst))
          (go make))
        (tagbody 
          (setl 'lst (cons chr lst))
          (setl 'i (add i 1))
          
          string-loop
          (setl 'chr (char str i))
          (if (and (not (eq chr #\")) (eq i len))
            (error "Missing \" in str passed to read-token-from-string"))
          (if (eq chr #\")
            (progn
              (setl 'i (add i 1))
              (setl 'lst (cons chr lst))
              (go make)))
          (if (eq chr #\\ )
            (if (eq len (add i 1))
              (error "Missing \" in str passed to read-token-from-string")
              (progn
                (setl 'i (add i 1))
                (setl 'lst (cons chr lst))
                (setl 'i (add i 1))
                (setl 'lst (cons chr lst))
                (go string-loop))))
          (setl 'i (add i 1))
          (setl 'lst (cons chr lst))
          (go string-loop)))
      ;;else:
      (setl 'i (add i 1))
      (setl 'lst (cons chr lst))
      (go loop)
      ;;make the string from the chars in lst:
      make
      (if (null lst) (return-from read-token-from-string (cons "" i)))
      (setl 'token (make-string (list-length lst) (car lst)))
      (setl 'cntr (sub (list-length lst) 1))
      makeloop
      (setl 'lst (cdr lst))
      (if (null lst) 
        (return-from read-token-from-string (cons token i)))   
      (setl 'cntr (sub cntr 1))
      (set-char token cntr (car lst))
      (go makeloop))))

(defun parse-number (str)
  (let ((i 0 ) (chr #\0) (n 0) (len (string-length str)))
   (tagbody
    number-loop
    (setl 'chr (char str i))
    (setl 'n (add (sub (progn chr (asm "shr rax, 6")) #x30) ;#x30 == #\0 (zero)
                  (mul n 10))) ;10 because we're using decimal
    (setl 'i (add i 1))
    (if (eq len i) 
      (return-from parse-number n)
      (go number-loop)))))

;;helper functions for parse-word:
(defun lower-case-p (char)
  (if (and (greater (progn char (asm "shr rax, 6")) #x60)
           (lesser (progn char (asm "shr rax, 6")) #x7B))
    t
    NIL))
(defun char-lower-to-upper-case (chr)
  (if (lower-case-p chr)
    (progn chr (asm "sub rax, 0x20<<8"))
    chr))
(defun string-lower-to-upper-case (str)
  (let ((len (string-length str)) (i 0))
   (tagbody
     loop
     (set-char str i (char-lower-to-upper-case (char str i)))
     (setl 'i (add 1 i))
     (if (not (eq i len)) 
       (go loop)
       (return-from string-lower-to-upper-case
                    str)))))
;;"word" here stands for a string containing a number or a symbol
;;parse-word converts all symbols to uppercase for better compatability with CL
(defun parse-word (word)
  (let ((word-len (string-length word)) (i 0) (chr (char word 0)) (n 0) (symbol NIL))
    (tagbody loop
            (if (and (greater (progn chr (asm "shr rax, 6")) #x29)
                     (lesser (progn chr (asm "shr rax, 6")) #x3A))
              (progn
                (setl 'i (add i 1))
                (if (eq word-len i) (go number))
                (setl 'chr (char word i))
                (go loop))
              (go symbol))
            number
            (return-from parse-word (parse-number word))
            symbol
            ;return a symbol with a name equal to the word
            (make-symbol (string-lower-to-upper-case word) NIL))))

;;returns a cons of the parsed list and the updated token-list
(defun parse-list (token-list)
  (let ((tmp-list NIL) (tmp-cons NIL))
    (tagbody
      (setl 'token-list (cdr token-list)) ;the first token is "(", so we ignore it

      list-loop
      ;;if missing ")":
      (if (null token-list)
        (error "Missing ) in parsed list"))
      ;;if end of list:
      (if (string-eq (car token-list) ")")
        (go list-end))
      ;;list: (list inside of a list)
      (if (string-eq (car token-list) "(")
        (progn
          (setl 'tmp-cons (parse-list token-list))
          (setl 'tmp-list (cons (car tmp-cons) tmp-list))
          (setl 'token-list (cdr tmp-cons))
          (go list-loop)))
      ;;word:
      (setl 'tmp-list (cons (parse-word (car token-list)) tmp-list))
      (setl 'token-list (cdr token-list))
      (go list-loop)
      
      list-end
      (setl 'tmp-list (list-reverse tmp-list))
      (setl 'token-list (cdr token-list))
      (return-from parse-list (cons tmp-list token-list)))))

;;returns a cons of the parsed expression and the updated token-list
(defun parse-quote (token-list)
  (setl 'token-list (cdr token-list)) ;the first token is always "'"
  (let ((expr NIL) (tmp-cons NIL))
    (tagbody
     ;;list:
     (if (string-eq (car token-list) "(")
       (progn
         (setl 'tmp-cons (parse-list token-list))
         (setl 'expr (car tmp-cons))
         (setl 'token-list (cdr tmp-cons))
         (go end)))
    ;;quote:
    (if (string-eq (car token-list) "'")
      (progn
        (setl 'tmp-cons (parse-quote token-list))
        (setl 'expr (car tmp-cons))
        (setl 'token-list (cdr tmp-cons))
        (go end)))  
    ;;quasiquote
    (if (string-eq (car token-list) "`")
      (error "Quasiquotes haven't been implemented in the parser yet!"))
    ;;word:
    (setl 'expr (parse-word (car token-list)))
    (setl 'token-list (cdr token-list))
    end
    (return-from parse-quote (cons (list 'quote expr) token-list)))))

;;returns a list with the parsed expressions
(defun read-from-string (str)
  (let ((token-list NIL) (token-index (cons "" 0)) (final-list NIL) (tmp-cons NIL))
    (tagbody 
      ;;build up a list of all the tokens (strings)
      token-list-loop 
      (setl 'token-index (read-token-from-string str (cdr token-index)))
      (if (string-eq (car token-index) "")
        (go parse) ;go to the next step
        (progn
          (setl 'token-list (cons (car token-index) token-list))
          (go token-list-loop)))
      ;;parse the tokens in the token list and put them in final-list
      parse
      (setl 'token-list (list-reverse token-list))
      parse-loop
      (when (null token-list)
        (go parse-end))
      ;;list:
      (when (string-eq (car token-list) "(")
        (setl 'tmp-cons (parse-list token-list))
        (setl 'final-list (cons (car tmp-cons) final-list))
        (setl 'token-list (cdr tmp-cons))
        (go parse-loop))
      ;;quote:
      (when (string-eq (car token-list) "'")
        (setl 'tmp-cons (parse-quote token-list))
        (setl 'final-list (cons (car tmp-cons) final-list))
        (setl 'token-list (cdr tmp-cons))
        (go parse-loop))
      ;;quasiquote/backquote:
      (when (string-eq (car token-list) "`")
        (error "Quasiquotes haven't been implemented in the parser yet!"))
      ;;word:
      (setl 'final-list (cons (parse-word (car token-list)) final-list))
      (setl 'token-list (cdr token-list))
      (go parse-loop)
      parse-end
      (return-from read-from-string (list-reverse final-list)))))

(defun read ()
  (read-from-string (read-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;print-nler:
;;;write-char and write-string are primitives defined in primitives.s

;;returns the last digit as a char
(defun number-to-char (n)
  (rem n 10)
  (asm "add rax, 0x30<<2")
  (asm "shl rax, 6")
  (asm "or rax, 1111b"))

(defun number-to-string (n)
  (let ((str "") (neg (lesser n 0)) (lst NIL) (i 1))
    (when neg (setl 'n (sub 0 n)))
    ;;put the digits in lst (in the order)
    (tagbody
      loop
      (setl 'lst (cons (rem n 10) lst))
      (setl 'n (div n 10))
      (when (not (eq n 0))
        (go loop)))
    (when neg (setl 'lst (cons 1 lst)))
    ;;pop them from lst to str
    (setl 'str (make-string (list-length lst) (number-to-char (car lst))))
    (setl 'lst (cdr lst))
    (dolist (digit lst)
      (set-char str i (number-to-char (car lst)))
      (setl 'lst (cdr lst))
      (setl 'i (add i 1)))
    (when neg (set-char str 0 #\-))
    str))

;;listp differs from the CL version
(defun listp (object)
  (if (consp object)
    (if (null (cdr object))
      t
      (listp (cdr object)))
    NIL))

;;nl is used to append a newline after print-nl
(defun print-nl (object nl)
  (cond ((stringp object)
         (write-char #\")
         (write-string object)
         (write-char #\"))
        ((charp object)
         (print-nl "TEST2" t)
         (write-string "#\\")
         (write-char object))
        ((numberp object)
         (write-string (number-to-string object)))
        ((symbolp object)
         (write-string (symbol-name object)))
        ((listp object)
         (write-string "(")
         (dolist (item object)
           (print-nl item NIL)
           (write-char #\space))
         (write-char #\backspace)
         (write-string ")"))
        ((null object)
         (write-string "NIL"))
        ((consp object)
         (write-string "(")
         (print-nl (car object) NIL)
         (write-string " . ")
         (print-nl (cdr object) NIL)
         (write-string ")"))
        ((functionp object)
         ;;temporary "representation" for functions
         (write-string "FUNCTION"))
        (t (error "Unknown object type passed to print-nl")))
  (when nl
    (write-char #\newline))
  object)

(defun print (obj)
  (print-nl obj t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Garbage-collector:
;;Iterate through the memory in the heap and look them up in the root-set (lexical enviroment and alist)
;;seems to wrongly delete a list passed to apply?
;;;CURRENTLY UNUSED! lisp_alloc does all the "garbage collection" itself
;(defun collect-garbage ()
  ;(let ((heap-min (asm "extern heap_min ~%mov rax, heap_min"))
        ;(heap-ptr (asm "extern heap_ptr ~%mov rax, [heap_ptr]")))
    ;(print-nl (mul 4 heap-min) t)
    ;(print-nl (mul 4 heap-ptr) t)
    ;;;All the types are 8 bytes aligned and take up at least one qword
    ;;;This loop can only call primitives to prevent a heap overflow 
    ;;;(function calls use memory) 
    ;(do ((ptr heap-min (add ptr 2)) ;fixnum 2 == 2<<2 == 8 (in asm)
         ;(tmp NIL))
      ;((eq ptr heap-ptr) (sub ptr heap-min))
      ;(write-char #\O)
      ;(setq tmp
            ;(or (gc-in-root-p ptr (asm "mov rax, r15"))
                ;(gc-in-root-p ptr (asm "mov rax, [alist_ptr]"))))
      ;(write-char #\T)
      ;;;cons and strings are the only types that may be stored in the heap atm
      ;(cond ((consp tmp)
             ;(write-char #\M)
             ;(setq ptr (add ptr 2))) ;cons take up 16 bytes
            ;((stringp tmp) ;div by 4 == shr by 2
             ;(write-char #\L)
             ;(setq ptr (add ptr (div (string-length tmp) 4))))
            ;(t ;;clear the qword
             ;(write-char #\Z)
             ;ptr ;put the ptr in rax
             ;(asm "mov qword [rax], 0"))))))
