;************************************************************
; CS 201a

;************************************************************
; Name: Feridun Mert Celebi
; Email address: feridun.celebi@yale.edu
;************************************************************

; Computer science topics: computer representation of integers, 
; translating and simulating TC-201 instructions.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 7)

;************************************************************
; ** problem 1 ** (10 points) - Done
; Write two procedures
 
; (bits->usint lst) 
; (usint->bits n)

; (bits->usint lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.
; Note the special case that () may represent 0.

; (usint->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in binary.
; Note that for 0 the answer is (0) but for
; all other numbers the answer starts with 1.

; Examples:
; (bits->usint '()) => 0
; (bits->usint '(0)) => 0
; (bits->usint '(1 0)) => 2
; (bits->usint '(0 0 0 1 1 0)) => 6

; (usint->bits 0) => (0)
; (usint->bits 6) => (1 1 0)
; (usint->bits 14) => (1 1 1 0)
; (usint->bits 7) => (1 1 1)
;************************************************************

(define bits->usint
  (lambda (lst)
    (let ((len (length lst)))
    (cond
      ((null? lst) 0)
      (else
       (+ (* (car lst) (expt 2 (- len 1))) (bits->usint (cdr lst))))))))

; digits-in-base takes a nonnegative integer n
; and a positive integer base (at least 2)
; and returns a list of the digits of n 
; when written in the given base.
(define digits-in-base
    (lambda (n base)
      (if (> base n)
          (list n)
          (append  (digits-in-base (quotient n base) base) 
                 (list (remainder n base) )))))

(define usint->bits
  (lambda (n)
    (digits-in-base n 2)))

;************************************************************
; ** problem 2 ** (5 points) - Done
; Write two procedures:

; (sublist lst i j)
; (left-fill item lst n)

; (sublist lst i j) 
; takes a list lst and nonnegative integers i and j
; and returns the list of elements indexed i through j.
; You may assume i and j are at least 0 and less than the
; length of the list, and i is less than or equal to j.
; As in list-ref, list elements are indexed from 0.

; (left-fill item lst n)
; takes an arbitrary Scheme value item,
; a list lst and a nonnegative integer n.
; If n is greater than the length of lst, it returns
; a list equal to lst with enough copies of item added at the
; beginning to make the length of the list n.
; If n is less than or equal to the length of lst,
; it is returned as is.

; Examples:
; (sublist '(a b c d e) 1 3) => (b c d)
; (sublist '(a b c d e) 4 4) => (e)
; (sublist '(a b c) 0 0) => (a)
; (sublist '(a b c) 0 2) => (a b c)

; (left-fill 0 '(1 0 0) 12) => (0 0 0 0 0 0 0 0 0 1 0 0)
; (left-fill 0 '(1 0 0) 4) => (0 1 0 0)
; (left-fill 0  '(1 0 0) 2) => (1 0 0)
; (left-fill 'x '(a b c) 6) => (x x x a b c)
;************************************************************

(define sublist
  (lambda (lst i j)
    (let ((dif (- j i)))
      (cond
        ((equal? dif -1) '())
        ((not (equal? i 0)) (sublist (cdr lst) (- i 1) (- j 1)))
        (else
         (cons (car lst) (sublist (cdr lst) i (- j 1))))))))

(define left-fill
  (lambda (item lst n)
    (let ((len (length lst)))
    (cond
      ((<= n len) lst)
      (else
       (append (list item) (left-fill item lst (- n 1))))))))

;************************************************************
; The TC-201 uses 16-bit sign-magnitude representation of integers.
; The next problem asks you to implement this arithmetic.
; Note that in sign-magnitude representation of integers with 16 bits
; bits, we can represent numbers from -32767 to +32767, 
; and there are two representations of 0.
;************************************************************
; ** problem 3 ** (10 points) - Done
; Write the following four procedures:

; (bits->tcint lst) 
; (tcint->bits x)
; (add-bits lst1 lst2)
; (sub-bits lst1 lst2)

; (bits->tcint lst) takes a list lst of 16 bits
; and returns an integer (positive, zero, or negative) 
; whose value is represented by the 16 bits (in sign-magnitude.)

; (tcint->bits x) takes an integer x (positive, zero, or negative)
; and returns a list of *two items*:
; the symbol ok or the symbol overflow
; and a list of 16 bits.
; If x can be correctly represented in sign-magnitude
; arithmetic in 16 bits, then the symbol is ok and
; the 16 bits give the correct representation.
; If x cannot be correctly represented, then the
; symbol is overflow and the 16 bits are all zeros.

; (add-bits lst1 lst2) input is two lists of 16 bits
; the returned value is a list of two items:
; the symbol ok or the symbol overflow
; and a list of 16 bits.
; If the sum of the numbers represented by lst1 and
; lst2 can be correctly represented in 16 bits in the TC-201,
; then the symbol is ok and the 16 bits is the representation
; of the sum.
; If the sum cannot be correctly represented, then
; the symbol is overflow and the 16 bits are all zeros.

; (sub-bits lst1 lst2) input is two lists of 16 bits
; the returned value is a list of two items:
; the symbol ok or the symbol overflow
; and a list of 16 bits.
; If the value of the difference of the two numbers 
; (first minus second) represented by lst1 and lst2 can
; be correctly represented, then the symbol is ok
; and the 16 bits represents the difference.
; If the difference cannot be correctly represented, then
; the symbol is overflow and the 16 bits are all zeros.

; For tcint->bits, add-bits and sub-bits,
; the representation returned for 0 should be a list of 16 zeros.

; Feel free to use Scheme arithmetic procedures to compute the
; sum or difference!

; Here are some constants

(define twotothe16 65536)
(define twotothe15 32768)
(define twotothe12  4096)

; Here are some 12 and 16 bit quantities to use for tests
; 12-bit 0 and 1

(define zero12  '(0 0 0 0  0 0 0 0  0 0 0 0))
(define one12   '(0 0 0 0  0 0 0 0  0 0 0 1))

; 16-bit 0, -0, 1, -1, 2, -2, 32767, and -32767

(define zero16   '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
(define mzero16  '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
(define one16    '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1))
(define mone16   '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1))
(define two16    '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0))
(define mtwo16   '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0))
(define large16  '(0 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1))
(define mlarge16 '(1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1))

; Examples:

; (tcint->bits 1) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (tcint->bits 0) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits -1) => (ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (tcint->bits 14) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0))
; (tcint->bits 32767) => (ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (tcint->bits -32767) => (ok (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (tcint->bits 32768) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits -32768) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits 65535) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (bits->tcint large16) => 32767
; (bits->tcint mone16) =>  -1
; (bits->tcint one16) => 1
; (bits->tcint mzero16) => 0
; (bits->tcint zero16) => 0
; (bits->tcint '(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0)) => -14
; (bits->tcint mlarge16) => -32767

; (add-bits one16 one16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (add-bits mzero16 mzero16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits large16 zero16) => (ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (add-bits large16 one16) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits one16 mone16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits mone16 mone16) => (ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (add-bits mlarge16 mone16) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits mlarge16 one16) => (ok (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))

; (sub-bits mlarge16 mlarge16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits one16 one16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits one16 mzero16) => (ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (sub-bits large16 one16) => (ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
; (sub-bits one16 large16) => (ok (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
; (sub-bits mone16 one16) => (ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (sub-bits mlarge16 large16) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits large16 mlarge16) => (overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;************************************************************

(define bits->tcint
  (lambda (lst)
    (cond
     ((equal? (car lst) 1) (* -1 (bits->usint (cdr lst))))
     (else
      (bits->usint (cdr lst))))))

(define tcint->bits
  (lambda (x)
    (cond
      ((or (< 32767 x) (> -32767 x)) (cons 'overflow (list (left-fill 0 '() 16))))
      ((>= x 0) (cons 'ok (list (cons 0 (left-fill 0 (usint->bits x) 15)))))
      ((< x 0) (cons 'ok (list (cons 1 (left-fill 0 (usint->bits (* -1 x)) 15)))))
      (else
       (cons 'ok (list (cons 0 (left-fill 0 (usint->bits x) 15))))))))

(define add-bits
  (lambda (lst1 lst2)
    (let ((l1 (bits->tcint lst1)) (l2 (bits->tcint lst2)))
      (cond
        ((or (< 32767 (+ l1 l2)) (> -32767 (+ l1 l2))) (cons 'overflow (list (left-fill 0 '() 16))))
        ((>= (+ l1 l2) 0) (cons 'ok (list (cons 0 (left-fill 0 (usint->bits (+ l1 l2)) 15)))))
        ((< (+ l1 l2) 0) (cons 'ok (list (cons 1 (left-fill 0 (usint->bits (* -1 (+ l1 l2))) 15)))))
        (else
         (cons 'ok (list (cons 0 (left-fill 0 (usint->bits (+ l1 l2)) 15)))))))))

(define sub-bits
  (lambda (lst1 lst2)
    (let ((l1 (bits->tcint lst1)) (l2 (bits->tcint lst2)))
      (cond
        ((or (< 32767 (- l1 l2)) (> -32767 (- l1 l2))) (cons 'overflow (list (left-fill 0 '() 16))))
        ((>= (- l1 l2) 0) (cons 'ok (list (cons 0 (left-fill 0 (usint->bits (- l1 l2)) 15)))))
        ((< (- l1 l2) 0) (cons 'ok (list (cons 1 (left-fill 0 (usint->bits (* -1 (- l1 l2))) 15)))))
        (else
         (cons 'ok (list (cons 0 (left-fill 0 (usint->bits (- l1 l2)) 15)))))))))
        
;************************************************************
; ** problem 4 ** (5 points) - Done
; Write two procedures

; (left-lookup val table)
; (right-lookup val table)

; that operate on tables, where
; a table is defined to be a list of (val1 val2) entries.

; Here are selectors for tables, please use them:

(define first-entry car)
(define rest-of-entries cdr)

; And for entries:

(define left-value car)
(define right-value cadr)

; (left-lookup val table) finds the first entry whose
; left-value is equal to val, and returns the
; right-value of the entry.  Otherwise, it returns #f.

; (right-lookup val table) finds the first entry whose
; right-value is equal to val, and returns the 
; left-value of the entry.  Otherwise, it returns #f.

; Here is a possibly useful table giving the equivalence
; of symbolic opcodes of the TC-201 and their 4 bit translations.

(define op-table
  '((halt (0 0 0 0)) (load (0 0 0 1)) (store (0 0 1 0))
    (add (0 0 1 1)) (sub (0 1 0 0)) (input (0 1 0 1))
    (output (0 1 1 0)) (jump (0 1 1 1)) (skipzero (1 0 0 0))
    (skippos (1 0 0 1)) (skiperr (1 0 1 0)) (loadi (1 0 1 1))
    (storei (1 1 0 0)) (halt (1 1 0 1)) (halt (1 1 1 0))
    (halt (1 1 1 1))))

; Examples

; (left-lookup 'b '((a 3) (b 2) (c 1))) => 2
; (left-lookup 'd '((a 3) (b 2) (c 1))) => #f
; (left-lookup 'halt op-table) => (0 0 0 0)
; (left-lookup 'skippos op-table) => (1 0 0 1)
; (left-lookup 'read op-table) => #f

; (right-lookup '(0 1 0 1) op-table) => input
; (right-lookup 3 '((a 3) (b 3) (c 3))) => a
; (right-lookup 5 '((a 3) (b 2) (c 1))) => #f
; (right-lookup '(0 0 0 0) op-table) => halt
; (right-lookup '(1 1 1 1) op-table) => halt
;************************************************************

(define left-lookup
  (lambda (val table)
    (cond
      ((null? table) #f)
      ((equal? (left-value (first-entry table)) val) (right-value (first-entry table)))
      (else
       (left-lookup val (rest-of-entries table))))))

(define right-lookup
  (lambda (val table)
    (cond
      ((null? table) #f)
      ((equal? (right-value (first-entry table)) val) (left-value (first-entry table)))
      (else
       (right-lookup val (rest-of-entries table))))))

;************************************************************
; An assembly language program for the TC-201 is represented
; as a list of items, each of which represents 

; an instruction 
; or
; a data statement.

; Each item is a list, which may optionally have a label as its
; first element.  A label is a symbol ending in a colon (:).

; For an instruction with an address 
; (load, store, add, sub, jump, loadi, storei), 
; the first (or next) symbol is the opcode, and the
; final entry is a symbol or a nonnegative number
; representing the contents of the address field.

; For an instruction with no address 
; (halt, input, output, skipzero, skippos, skiperror), 
; the first (or next) symbol is the opcode,
; and there are no further entries.

; For a data statement, the first (or next) symbol is the symbol
; data, followed by an integer (positive, zero, or negative)
; or a symbol, representing the value to be placed 
; in the corresponding memory location.
; In the case of a symbol, the symbol followed by a colon (:)
; should be a label in the program, and the number is
; the address of the memory register with that label.
; You may assume that the integer is in the correct range
; to be represented correctly in a TC-201 memory register.

; Examples of programs:

; Initializes count to 1 and halts.

(define prog1
  '((start: load one)
    (store count)
    (halt)
    (one: data 1)
    (count: data 0)))

; reads in a zero-terminated sequence of numbers
; and prints out their sum and then halts.

(define prog2
  '((start: input)
    (skipzero)
    (jump continue)
    (jump finish)
    (continue: add sum)
    (store sum)
    (jump start)
    (finish: load sum)
    (output)
    (halt)
    (sum: data 0)))

;************************************************************
; ** problem 5 ** (10 points)
; Define a program

; rem-prog

; for the TC-201 in the above format to
; input two integers
; compute and output the integer remainder of
; the first integer divided by the second integer
; and then halt.

; You may assume that the first integer is non-negative
; and the second integer is positive, and both are
; in the range to be correctly represented as 16-bit
; TC-201 integers.
; Your algorithm does not have to be particularly "efficient" -- 
; interpreting division as repeated subtraction is fine.

; NOTE: Include an explanation of how your program works.

; Examples:
;    inputs  ->  output
;     0  13        0
;    43  20        3
;    17   3        2
;   121  12        1
;  1066  50       16
;************************************************************

; I used a similar algorithm to the TC-201 program that we wrote in our class, which read two numbers provided the product.
; In this program, we read in a number and store it in x and read in another number and store it in y. X is also used as an accumlator of the output
; As long as x is positive, we subtract y from x and repeat until x becomes 0 or negative. When x because 0 or negative
; the program uses a jump to another label called end, which eventually displays the outputs and halts.
; If x is 0 at the beginning, the skipzero makes it jump to the end label. The second skipzero in the end label, prevents x from incremented by y, by
; skipping it.

(define rem-prog
'((start: input)
  (store x)
  (input)
  (store y)
  (load x)
  (skipzero)
  (loop-main: skippos)
  (jump print-end)
  (jump subtraction)
  (print-end: load x)
  (skipzero)
  (add y)
  (output)
  (halt)
  (subtraction: load x)
  (sub y)
  (store x)
  (jump loop-main)
  (x: data 0)
  (y: data 0)))

;************************************************************
; ** problem 6 ** (10 points) - Done
; Write a procedure

; (symbol-table prog)

; that takes a TC-201 program prog 
; and returns a table containing all the labels
; defined in the program and their corresponding addresses
; when the program is loaded into memory.
; We assume that the program is loaded into memory *starting at address 0*.

; The order of the labels in the table should be the
; order in which they are first defined in the program.
; You may assume that no label will be defined more than once.

; Note that a symbol is a label if it is first in the list
; for an instruction or data statement,
; AND its name ends with colon (:).  
; It may be helpful to look up the procedures: 
; symbol->string, string-ref, string-length, 
; and the representation of the character constant #\:

; Here is a procedure you may use to determine whether
; a symbol ends with the colon (:)

(define label?
  (lambda (sym)
    (let ((str (symbol->string sym)))
      (let ((char (string-ref str (- (string-length str) 1))))
  (equal? char #\:)))))


; Examples for symbol-table:

; (symbol-table '((here: load 14) (store 15) (there: halt))) =>
;         ((here: 0) (there: 2))
; (symbol-table prog1) =>
;         ((start: 0) (one: 3) (count: 4))
; (symbol-table prog2) => 
;         ((start: 0) (continue: 4) (finish: 7) (sum: 10))
;************************************************************

(define symbol-table
  (lambda (prog)
    (symbol-table-helper prog 0)))

; This basically does the job of the symbol-table. However, it gets n, as well as the program itself as input, for numbering purposes.
(define symbol-table-helper
  (lambda (prog n)
    (cond
     ((null? prog) '())
     ((label? (caar prog)) (cons (list (caar prog) n) (symbol-table-helper (cdr prog) (+ n 1))))
     (else
      (symbol-table-helper (cdr prog) (+ n 1))))))
	     
;************************************************************
; ** problem 7 ** (10 points) - Done
; Write a procedure 

; (assemble prog)

; that takes a symbolic TC-201 program prog 
; in assembly language and returns the list of
; 16-bit values (represented as lists of bits)
; that represent that program 
; when loaded starting at address 0.
; There should be one 16-bit value for each
; instruction or data statement in the program.

; Examples (formatted for readability):

; (assemble '((halt))) =>
; ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (assemble '((data -1))) =>
; ((1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

; (assemble '((load 22) (store 23))) =>
; ((0 0 0 1 0 0 0 0 0 0 0 1 0 1 1 0) 
;  (0 0 1 0 0 0 0 0 0 0 0 1 0 1 1 1))

; (assemble prog1) =>
; ((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (assemble prog2) =>
;  ((0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 1 1 1 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1)
;   (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;************************************************************

; (define assemble
;   (lambda (prog)
;     (assemble-helper1 prog prog)))
; 
; (define assemble-helper1
;   (lambda (prog old)
;     (cond
;       ((null? prog) '())
;       ((equal? (length (car prog)) 1) (cons (append (left-lookup (caar prog) op-table) (left-fill 0 '() 12)) (assemble-helper1 (cdr prog) old)))
;       ((and (equal? (length (car prog)) 2) (label? (caar prog)) (assemble-helper1 (cons (cdar prog) (cdr prog)) old)))
;       ((and (equal? (length (car prog)) 2) (equal? 'data (caar prog))) (cons (cadr (tcint->bits (cadar prog))) (assemble-helper1 (cdr prog) old)))
;       ((and (equal? (length (car prog)) 2) (number? (cadar prog))) (cons (append (left-lookup (caar prog) op-table)(left-fill 0 (usint->bits (cadar prog)) 12)) (assemble-helper1 (cdr prog) old)))
;       ((and (equal? (length (car prog)) 2) (not (boolean? (left-lookup (cdar prog) op-table))) (cons (append (left-lookup (caar prog) op-table) (left-fill 0 (left-lookup (cdar prog)) 12)) (assemble-helper1 (cdr prog) old))))
;       ((and (equal? (length (car prog)) 2) (boolean? (right-lookup (cdar prog) op-table)) (cons (append (left-lookup (caar prog) op-table)(left-fill 0 (assemble-helper2 (car prog) old) 12)) (assemble-helper1 (cdr prog) old))))
;       ((equal? (length (car prog)) 3) (assemble-helper1 (cons (cdar prog) (cdr prog)) old)))))
; 
; (define assemble-helper2
;   (lambda (lst prog)
;     (let ((address (get-address 0 prog (cadr lst))))
;     (left-fill 0 (usint->bits address) 12))))
; 
; (define get-address
;   (lambda (n prog sym)
;     (let ((str (string-append (symbol->string sym) ":")))
;     (cond
;       ((null? prog) #f)
;       ((equal? (symbol->string (caar prog)) str) n)
;       (else
;        (get-address (+ n 1) (cdr prog) sym))))))
;  


(define assemble
  (lambda (prog)
    (assemble-helper1 prog prog)))

; This procedure basically does the job of assemble. It takes the program twice, one for usage in general purposes
; one for use in the procedure assemble-helper2. This procedure returns the 16-bit values.
(define assemble-helper1
  (lambda (prog backup)
    (let ((all-0 (left-fill 0 '() 12)))
    (cond
      ((null? prog) '())
      ((equal? (length (first-entry prog)) 1) (cons (append (left-lookup (left-value (first-entry prog)) op-table) all-0) 
                                                    (assemble-helper1 (rest-of-entries prog) backup)))
      ((and (equal? (length (first-entry prog)) 2) (label? (left-value (first-entry prog))) 
            (assemble-helper1 (cons (cdar prog) (rest-of-entries prog)) backup)))
      ((and (equal? (length (first-entry prog)) 2) (equal? 'data (caar prog))) (cons (cadr (tcint->bits (cadar prog))) 
                                                                                     (assemble-helper1 (rest-of-entries prog) backup)))
      ((and (equal? (length (first-entry prog)) 2) (number? (cadar prog))) (cons (append (left-lookup (first-entry (left-value prog)) op-table)(left-fill 0 (usint->bits (right-value (first-entry prog))) 12)) 
                                                                                 (assemble-helper1 (rest-of-entries prog) backup)))
      ((and (equal? (length (first-entry prog)) 2) (not (boolean? (left-lookup (cdar prog) op-table))) (cons (append (left-lookup (first-entry (left-value prog)) op-table) (left-fill 0 (left-lookup (cdar prog)) 12)) 
                                                                                                             (assemble-helper1 (rest-of-entries prog) backup))))
      ((and (equal? (length (first-entry prog)) 2) (boolean? (right-lookup (cdar prog) op-table)) (cons (append (left-lookup (first-entry (left-value prog)) op-table)(left-fill 0 (assemble-helper2 (first-entry prog) backup) 12)) 
                                                                                                        (assemble-helper1 (rest-of-entries prog) backup))))
      ((equal? (length (first-entry prog)) 3) 
       (assemble-helper1 (cons (cdar prog) (rest-of-entries prog)) backup))))))

; This procedure is for the the case where the right-value in the first-entry of the program is a label in the other parts of the program.
; It uses the get-address procedure to find the address of the label and returns the address of the label in 12 bits.
(define assemble-helper2
  (lambda (lst prog)
    (let ((address (get-address 0 prog (cadr lst))))
    (left-fill 0 (usint->bits address) 12))))

; This procedure takes n, the program and the symbol to be looked for. It adds a : to the symbol and 
; searches in the program. When there is a match, the procedure returns n, the address of the symbol.
(define get-address
  (lambda (n prog sym)
    (let ((str (string-append (symbol->string sym) ":")))
    (cond
      ((equal? (symbol->string (caar prog)) str) n)
      (else
       (get-address (+ n 1) (cdr prog) sym))))))
;************************************************************
; Next we develop a simulator for the TC-201

; A configuration of the TC-201 is a list
; giving the values of:

; the accumulator (acc)
; the program counter (pc)
; the run flag (run-flag)
; the arithmetic error bit (aeb)
; the contents of the 4096 memory registers

; as a table of (val1 val2) entries.
; For the accumulator, the left value is the symbol acc
; and the right value is a list giving the 16 bits of its contents.
; For the program counter, the left value is the symbol pc
; and the right value is a list giving the 12 bits of its contents.
; For the run flag, the left value is the symbol run-flag and
; the right value is either 0 (not running) or 1 (running).
; For the arithmetic error bit, the left value is the symbol aeb
; and the right value is either 0 (no error) or 1 (error).

; For each memory location listed, the left value is an integer
; (between 0 and 4095 inclusive) giving the address of the
; memory location, and the right value is a list giving the 16 bits
; of its contents.
; Memory locations not listed in the configuration are assumed
; to contain 16 0's.

; For example, here is the configuration that results from
; assembling and loading program prog1 starting
; at memory location 0.

; (Recall the definition of prog1 from above.)

; (define prog1
;   '((start: load one)
;     (store count)
;     (halt)
;     (one: data 1)
;     (count: data 0)))

(define config1
  '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (pc (0 0 0 0 0 0 0 0 0 0 0 0))
    (run-flag 1)
    (aeb 0)
    (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
    (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

; Here is another representation of the same
; configuration

(define config1a
  '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
    (run-flag 1)
    (aeb 0)
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (pc (0 0 0 0 0 0 0 0 0 0 0 0))))

(define config2a
  '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
    (run-flag 1)
    (aeb 0)
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (pc (0 0 0 0 0 0 0 0 0 0 0 0))))

(define numbers
  '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (4 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))))

;************************************************************
; ** problem 8 ** (10 points) - Done
; Write two procedures

; (memory-read address config)
; (normalize config)

; (memory-read address config)
; given an integer address between 0 and 4095 inclusive 
; and a TC-201 configuration, returns
; the list of 16 bits represeting the contents of
; the addressed register in memory.

; (normalize config)
; takes a configuration of the TC-201 and returns
; an equivalent configuration of the TC-201 in
; a *normalized* form, with
; the CPU registers and flags in the order:
; acc, pc, run-flag, aeb
; and the memory registers (in order of increasing addresses)
; whose contents are not all 0's.

; Examples
; (memory-read 0 config1) => (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
; (memory-read 2 config1a) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; (memory-read 7 config1) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; (normalize config1a) =>
; ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (pc (0 0 0 0 0 0 0 0 0 0 0 0))
;  (run-flag 1)
;  (aeb 0)
;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
;************************************************************

(define memory-read
  (lambda (address config)
    (cond
      ((null? config) (left-fill 0 '() 16))
      ((equal? (caar config) address) (cadar config))
      (else
       (memory-read address (cdr config))))))

; This remove procedure receives two arguments: one element and one list.
; This procedure scans the list and tries to find the element. If it can find
; the element it removes from the list.
(define remove
  (lambda (element lst)
    (cond
      ((equal? '() lst) '())
      ((equal? (car lst) element) (remove element (cdr lst)))
      (else (cons (car lst) (remove element (cdr lst)))))))

; Next three procedures, bubble-sort, bubble-helper and swap are all part of my sorting algorithm.
; However inefficient, I decided to use bubble-sort to sort the address of the configuration. 
; While first two procedures simulate two nested for-loops, the swap method swaps the two values if the first
; one is greater than the second one.
(define bubble-sort
  (lambda (lst i)
    (let ((len (length lst)))
    (cond
      ((equal? i len) lst)
      (else
       (bubble-sort (bubble-helper lst 0 1) (+ 1 i)))))))

(define bubble-helper
  (lambda (lst i1 i2)
    (cond
      ((equal? i2 (length lst)) lst)
      ((> (car (sublist lst i1 i1)) (car (sublist lst i2 i2))) (bubble-helper (swap lst i1 i2) (+ i1 1) (+ i2 1)))
      (else
       (bubble-helper lst (+ i1 1) (+ i2 1))))))

(define swap
  (lambda (lst i1 i2)
    (let ((temp (car (sublist lst i1 i1))))
      (cond
        ((equal? 0 i1) (cons (car (sublist lst i2 i2)) (cons temp (sublist lst (+ i2 1) (- (length lst) 1)))))
        ((equal? i2 (- (length lst) 1)) (cons (car (sublist lst 0 (- i1 1))) (list (car (sublist lst i2 i2)) temp)))
        (else
         (append (sublist lst 0 (- i1 1)) (cons (car (sublist lst i2 i2)) (cons temp (sublist lst (+ i2 1) (- (length lst) 1))))))))))

(define normalize
  (lambda (config)
    (normalize-helper (append '(acc pc run-flag aeb) (bubble-sort (number config) 0)) config config config)))

; This basically does the job of the normalize, it takes a list of all the elements(in order) that a configuration should have
; and sorts the unorganized configuration accordingly.
(define normalize-helper
  (lambda (lst-of config old-config removed)
    (cond
      ((null? config) '())
      ((null? lst-of) '())
      ((equal? (car lst-of) (caar config)) (cons (car config) (normalize-helper (cdr lst-of) old-config old-config (remove (car config) old-config))))
      (else
       (normalize-helper lst-of (cdr config) old-config removed)))))

; This procedure takes the configuration as its input. Then it returns every left-value in the bit-pattern, as long as bit-pattern does not have all 0's.
(define number
  (lambda (config)
    (cond
      ((null? config) '())
      ((and (not (equal? (cadar config) (left-fill 0 '() 16))) (number? (caar config))) (cons (caar config) (number (cdr config))))
      (else
       (number (cdr config))))))

;************************************************************
; Here is a procedure to print out a configuration
; for you to examine

(define print-config
  (lambda (config)
    (display "*******************")(newline)
    (print-lst config)))

(define print-lst
  (lambda (lst)
    (map (lambda (item) 
           (display " ") 
           (display item) 
           (newline))
       lst)
    (newline)))

;************************************************************
; ** problem 9 ** (10 points) - Done
; Write two procedures

; (init-config bit-patterns)
; (halted? config)

; (init-config bit-patterns)
; takes a list of 16-bit patterns
; (each one a list of 16 bits)
; and returns the *normalized*
; configuration of the TC-201 
; with those 16-bit patterns loaded into memory
; starting at address 0 and continuing
; with consecutive addresses,
; the accumulator set to 16 0's,
; the program counter set to 12 0's
; the run-flag set to 1, 
; and the arithmetic error bit set to 0.

; (halted? config)
; returns #t if in configuration config the TC-201
; is halted (i.e., the run-flag is 0) and #f otherwise.

; Here are the bit-patterns for load 3, store 4:

(define bit-patterns0
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

; These are the bit-patterns for load 8, store 9,
; halt, data 1, data -6.

(define bit-patterns1
  '((0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0)
    (0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1) 
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
    (1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)))

; Examples
; (init-config bit-patterns0) =>
;((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (pc (0 0 0 0 0 0 0 0 0 0 0 0))
; (run-flag 1)
; (aeb 0)
; (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
; (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

; (init-config bit-patterns1) => 
;((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (pc (0 0 0 0 0 0 0 0 0 0 0 0))
; (run-flag 1)
; (aeb 0)
; (0 (0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0))
; (1 (0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1))
; (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (4 (1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)))

;************************************************************

(define init-config
  (lambda (bit-pattern)
    (normalize (init-config-helper '(acc pc run-flag aeb) bit-pattern))))

; This basically does the job of the init-config. Puts, acc, pc, run-flag, aeb and bit-patterns together.
(define init-config-helper
  (lambda (lst bit-pattern)
    (cond
      ((null? lst) (get-bit-pattern bit-pattern 0))
      ((equal? (car lst) 'acc) (cons (list (car lst) (left-fill 0 '() 16)) (init-config-helper (cdr lst) bit-pattern)))
      ((equal? (car lst) 'pc) (cons (list (car lst) (left-fill 0 '() 12)) (init-config-helper (cdr lst) bit-pattern)))
      ((equal? (car lst) 'run-flag) (cons (list (car lst) 1) (init-config-helper (cdr lst) bit-pattern)))
      ((equal? (car lst) 'aeb) (cons (list (car lst) 0) (init-config-helper (cdr lst) bit-pattern))))))

; gets the bit-pattern and n, returns all the bit-patterns in order and in a numbered way.
(define get-bit-pattern
  (lambda (bit-pattern n)
    (cond
     ((null? bit-pattern) '())
     (else
      (cons (list n (car bit-pattern)) (get-bit-pattern (cdr bit-pattern) (+ n 1)))))))

(define halted?
  (lambda (config)
    (if (equal? (right-value (first-entry (rest-of-entries (rest-of-entries config)))) 0)
        #t
        #f)))

;************************************************************
; ** problem 10 ** (20 points)
; Write a procedure

; (next-config config)

; that returns the *normalized* next configuration of the TC-201
; after executing one instruction.
; If the run-flag is 0, the *normalized* input configuration 
; should be returned.
; If the run-flag is 1, the *normalized* configuration that
; results from executing the instruction at the address
; in the program counter should be returned.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei.

; You should intepret an undefined opcode (1101 through 1111)
; as a halt instruction.

; Test configurations for next-config are
; available in the separate file hw6-configs.scm.

; Here's a generic simulator to run a program
; and return the list of successive configurations
; once your next-config program is working.

; It runs until the machine reaches a halted configuration
; or the specified number of instructions have been executed,
; whichever occurs first.

(define simulate
  (lambda (config steps)
    (cond
     ((or (halted? config) (< steps 1)) (list config))
     (else (cons config
		 (simulate (next-config config) (- steps 1)))))))

; Once your assemble, init-config and next-config
; procedures are working, you'll be able to run
; programs using simulate, for example:
;; > (for-each print-config (simulate (init-config (assemble prog1)) 3))
;; *******************
;;  (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;;  (pc (0 0 0 0 0 0 0 0 0 0 0 0))
;;  (run-flag 1)
;;  (aeb 0)
;;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;; *******************
;;  (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;;  (pc (0 0 0 0 0 0 0 0 0 0 0 1))
;;  (run-flag 1)
;;  (aeb 0)
;;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;; *******************
;;  (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;;  (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;;  (run-flag 1)
;;  (aeb 0)
;;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;;  (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;; *******************
;;  (acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;;  (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;;  (run-flag 0)
;;  (aeb 0)
;;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;;  (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

(define config-output
  '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
    (pc (0 0 0 0 0 0 0 0 1 0 1 1))
    (run-flag 1) 
    (aeb 0)
    (10 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
    (11 (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
;************************************************************

(define next-config
  (lambda (config)
    (let ((pc (memory-read (bits->usint (cadadr config)) config)))
    (cond
      ((equal? (right-lookup (sublist pc 0 3) op-table) 'output) (normalize (next-config-helper config pc)))
      (else
       (normalize (next-config-helper config pc)))))))

; This basically does the job of the next-config. If the config is halted, it returns the normalized config.
; Otherwise, it compares the op-codes and makes calls to the corresponding procedures.
(define next-config-helper
  (lambda (config pc)
    (let ((op-code (sublist pc 0 3)) (address (sublist pc 4 15)))
    (cond
      ((halted? config) config)
      ((or (and (equal? address (left-fill 0 '() 12)) (equal? (right-lookup op-code op-table) 'halt)) (boolean? (right-lookup op-code op-table))) (halt config))
      ((equal? 'add (right-lookup op-code op-table)) (add (increase-pc config) (bits->usint address)))
      ((equal? 'sub (right-lookup op-code op-table)) (sub (increase-pc config) (bits->usint address)))
      ((equal? 'load (right-lookup op-code op-table)) (load-n (increase-pc config) (bits->usint address)))
      ((equal? 'store (right-lookup op-code op-table)) (store-n (increase-pc config) (bits->usint address) (increase-pc config)))
      ((equal? 'loadi (right-lookup op-code op-table)) (loadi (increase-pc config) (bits->usint address)))
      ((equal? 'storei (right-lookup op-code op-table)) (storei (increase-pc config) (bits->usint address) (increase-pc config)))
      ((equal? 'input (right-lookup op-code op-table)) (input (increase-pc config)))
      ((equal? 'output (right-lookup op-code op-table)) (output (increase-pc config)))
      ((equal? 'jump (right-lookup op-code op-table)) (jump config address))
      ((equal? 'skipzero (right-lookup op-code op-table)) (skipzero (increase-pc config)))
      ((equal? 'skippos (right-lookup op-code op-table)) (skippos (increase-pc config)))
      ((equal? 'skiperr (right-lookup op-code op-table)) (skiperr (increase-pc config)))))))

; As we try to get the next config, the pc also increases by 1, everytime we make a move. This procedure takes config and returns config, with an incremented pc.
(define increase-pc
  (lambda (config)
    (cons (car config) (cons (cons 'pc (list (left-fill 0 (usint->bits (+ (bits->usint (cadadr config)) 1)) 12))) (cddr config)))))

; stops program execution
(define halt
  (lambda (config)
    (append (sublist config 0 1) (cons (cons 'run-flag '(0)) (cdddr config)))))

; adds contents of addressed word to ACC
(define add
  (lambda (config address)
    (let ((sum (add-bits (cadar config) (memory-read address config))))
      (cond
      ((equal? (car sum) 'ok) (cons (cons 'acc (cdr sum)) (append (append (sublist (cdr config) 0 1) (list (append '(aeb) '(0)))) (cddddr config))))
      (else
       (cons (cons 'acc (cdr sum)) (append (append (sublist (cdr config) 0 1) (list (append '(aeb) '(1)))) (cddddr config))))))))

; subtracts contents of addressed word from ACC
(define sub
  (lambda (config address)
    (let ((sum (sub-bits  (cadar config) (memory-read address config))))
      (cond
      ((equal? (car sum) 'ok) (cons (cons 'acc (cdr sum)) (append (append (sublist (cdr config) 0 1) (list (append '(aeb) '(0)))) (cddddr config))))
      (else
       (cons (cons 'acc (cdr sum)) (append (append (sublist (cdr config) 0 1) (list (append '(aeb) '(1)))) (cddddr config))))))))

; copies the instruction address to the PC
(define jump
  (lambda (config address)
    (cons (car config) (cons (list 'pc address) (cddr config)))))

; skips next instruction if ACC is +0 or -0
(define skipzero
  (lambda (config)
    (let ((acc (bits->tcint (right-value (first-entry config)))))
      (cond
        ((equal? acc 0) (increase-pc config))
        (else config)))))

; skips next instruction if ACC is positive
(define skippos
  (lambda (config)
    (let ((acc (bits->tcint (right-value (first-entry config)))))
      (cond
        ((> acc 0) (increase-pc config))
        (else config)))))

; skips next instruction if AEB is 1 and sets it to 0
(define skiperr
  (lambda (config)
    (let ((aeb (right-value (first-entry (rest-of-entries (rest-of-entries (rest-of-entries config)))))))
      (cond
        ((equal? aeb 1) (increase-pc (append (sublist config 0 2) (cons (append '(aeb) '(0)) (cddddr config)))))
        (else config)))))

; writes number from ACC out to user
(define output
  (lambda (config)
    (let ((acc (bits->tcint (right-value (first-entry config)))))
      (display acc)
      config)))

; copies number from user into ACC
(define input
  (lambda (config)
    (begin
      (display "input = ")
      (let ((inp (read)))
        (cons (cons 'acc (list (left-fill 0 (usint->bits inp) 16))) (cdr config))))))

; copies contents of the addressed word into ACC
(define load-n
  (lambda (config n)
    (let ((put-acc (memory-read n config)))
    (cons (cons 'acc (list put-acc)) (cdr config)))))

; copies contents of word indirectly addressed to ACC
(define loadi
  (lambda (config n)
    (let ((put-acc (memory-read (bits->usint (memory-read n config)) config)))
      (cons (cons 'acc (list put-acc)) (cdr config)))))

; copies contents of ACC into addressed word
(define store-n
  (lambda (config n backup)
      (cond
        ((null? config) '())
        ((boolean? (memory-read-special n backup))(append (sublist backup 0 (- (length backup) 1)) (list (cons n (list (memory-read 'acc backup))))))
        ((equal? (left-value (first-entry config)) n) (cons (cons (left-value (first-entry config)) (list (memory-read 'acc backup))) (store-n (cdr config) n backup)))
        (else
         (cons (car config) (store-n (cdr config) n backup))))))

; copies contents of ACC to word indirectly addressed
(define storei
  (lambda (config n backup)
      (cond
        ((null? config) '())
        ((boolean? (memory-read-special (bits->usint (memory-read n backup)) backup))(append (sublist backup 0 (- (length backup) 1)) (list (cons n (list (memory-read 'acc backup))))))
        ((equal? (left-value (first-entry config)) (bits->usint (memory-read n config))) (cons (cons (left-value (first-entry config)) (list (memory-read 'acc backup))) (storei (cdr config) n backup)))
        (else
         (cons (car config) (storei (cdr config) n backup))))))

; A special form of memory-read, which returns false, when there is no match.
(define memory-read-special
  (lambda (address config)
    (cond
      ((null? config) #f)
      ((equal? (caar config) address) (cadar config))
      (else
       (memory-read-special address (cdr config))))))

;********************** end of hw6.scm **********************
