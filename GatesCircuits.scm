;**********************************************************
; CS 201a

;**********************************************************
; Name: Feridun Mert Celebi
; Email address: feridun.celebi@yale.edu
;**********************************************************

; Computer science topics: gates and circuits

;**********************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 4)

; Here are procedures that allow the testing of
; outputs without regard to order.  
; Note that they return 1 for true and 0 for false.
; The first three are parameterized by an equality test
; that returns 1 or 0.

(define b-member
  (lambda (b-eq-test item lst)
    (cond
     ((null? lst) 0)
     ((= 1 (b-eq-test item (car lst))) 1)
     (else (b-member b-eq-test item (cdr lst))))))

(define b-unique
  (lambda (b-eq-test lst)
    (cond
     ((null? lst) 1)
     ((= 1 (b-member b-eq-test (car lst) (cdr lst))) 0)
     (else (b-unique b-eq-test (cdr lst))))))

(define b-subset
  (lambda (b-eq-test lst1 lst2)
    (cond
     ((null? lst1) 1)
     ((= 1 (b-member b-eq-test (car lst1) lst2) )
      (b-subset b-eq-test (cdr lst1) lst2))
     (else 0))))

(define b-permutation
  (lambda (b-eq-test lst1 lst2)
    (* (b-unique b-eq-test lst1) 
       (b-unique b-eq-test lst2)
       (b-subset b-eq-test lst1 lst2) 
       (b-subset b-eq-test lst2 lst1))))

(define b-p
  (lambda (lst1 lst2)
    (b-permutation (lambda (x y) (if (equal? x y) 1 0)) lst1 lst2)))

(define b-pp
  (lambda (lst1 lst2)
    (b-permutation (lambda (x y) (if (= 1 (b-p x y)) 1 0)) lst1 lst2)))


;**********************************************************
; Here are procedures to compute the gate functions
; with 0 = false and 1 = true.

(define b-not (lambda (x) (if (equal? x 1) 0 1)))
(define b-and (lambda (x y) (if (equal? x 0) 0 y)))
(define b-or (lambda (x y) (if (equal? x 1) 1 y)))
(define b-xor (lambda (x y) (if (equal? x 0) y (b-not y))))
(define b-nand (lambda (x y) (b-not (b-and x y))))
(define b-nor (lambda (x y) (b-not (b-or x y))))

;**********************************************************
; A gate is a list of three elements:
; 1) a list giving the names of the input wires
; 2) the name of the output wire
; 3) a symbol indicating the type of gate, one of:
;   not, and, or, xor, nand, nor
; corresponding to the functions above.

; Input and output wire names are Scheme symbols.

; Some examples of gates

(define gate1 (list '(x y) 'z 'and))
(define gate2 (list '(w1 w2) 'w3 'or))
(define gate3 (list '(s) 't 'not))
(define gate4 (list '(x y) 'q 'nand))

; Selectors for gates -- please use these to refer to parts of a gate.

(define gate-inputs (lambda (gate) (car gate)))
(define gate-output (lambda (gate) (cadr gate)))
(define gate-fn (lambda (gate) (caddr gate)))

; Constructor for gates -- please use it to construct gates.
; (Yes, it is just a synonym for list.)

(define gate (lambda (inputs output fn) (list inputs output fn)))

; Examples of selectors and constructor:

; (gate-inputs gate2) => (w1 w2)
; (gate-output gate4) => q
; (gate-fn gate3) => not
; (gate '(x y) 'z 'and) => ((x y) z and)

;**********************************************************
; A circuit is represented as a list of 3 items:
; (1) a list of input wire names,
; (2) a list of output wire names,
; (3) a list of gates.

; Input and output wire names are Scheme symbols.
; In a well-formed circuit, every wire must be either 
; an input of the circuit or an output of exactly one 
; gate in the circuit.

; Here are selectors for a circuit -- please use them
; to access parts of a circuit.

(define ckt-inputs (lambda (ckt) (car ckt)))
(define ckt-outputs (lambda (ckt) (cadr ckt)))
(define ckt-gates (lambda (ckt) (caddr ckt)))

; Here is a constructor for circuits -- please use it
; to construct circuits.  (Yes, another synonym for list.)

(define ckt (lambda (inputs outputs gates) (list inputs outputs gates)))

;**********************************************************
; Examples of circuits

; Here is a circuit to compute z = 1 if x = y and z = 0 otherwise:
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define eq-ckt1
  (ckt
   '(x y)
   '(z)
   (list
    (gate '(x) 'w1 'not)
    (gate '(y) 'w2 'not)
    (gate '(x y) 'w3 'and)
    (gate '(w1 w2) 'w4 'and)
    (gate '(w3 w4) 'z 'or))))

; This is interpreted as follows:
; the inputs of the circuit are wires x and y
; the outputs of the circuit are just wire z
; there are five gates specified as follows:
; wire w1 is the output of a NOT gate with input x
; wire w2 is the output of a NOT gate with input y
; wire w3 is the output of an AND gate with inputs x and y
; wire w4 is the output of an AND gate with inputs w1 and w2
; wire z is the output of an OR gate with inputs w3 and w4

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.

(define eq-ckt2
  (ckt
   '(x y)
   '(v)
   (list
    (gate '(x y) 'w 'xor)
    (gate '(w) 'v 'not))))

; Here is a two-bit selector:
; z1 = x1 * s' + y1 * s
; z0 = x0 * s' + y0 * s
; This is also a combinational circuit.

(define sel-ckt
  (ckt
   '(x1 x0 y1 y0 s)
    '(z1 z0)
    (list
     (gate '(s) 't 'not)
     (gate '(x1 t) 'u1 'and)
     (gate '(x0 t) 'u0 'and)
     (gate '(y1 s) 'v1 'and)
     (gate '(y0 s) 'v0 'and)
     (gate '(u1 v1) 'z1 'or)
     (gate '(u0 v0) 'z0 'or))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a cycle of wires and gates.

(define latch-ckt
  (ckt
   '(x y)
   '(q u)
   (list
    (gate '(x u) 'q 'nand)
    (gate '(y q) 'u 'nand))))

; This is also a sequential circuit, with
; an OR gate one of whose inputs is its output.

(define seq-or-ckt
  (ckt
   '(x)
   '(y)
   (list
    (gate '(x y) 'y 'or))))

; This is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has no inputs, but
; does have an output.

(define clock-ckt
  (ckt
   '()
   '(z)
   (list
    (gate '(z) 'z 'not))))

;**********************************************************
; ** problem 1 ** (10 points) - Done
; Write two procedures.

; (ckt-wires ckt) to return the list of all the wire names that appear
;      in the circuit ckt, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, with duplicates removed.
; (find-gate wire ckt) to return the gate in the circuit ckt with the given
;      output wire, or #f if there is no such gate.

; You may assume that ckt is a well-formed circuit; in particular,
; a wire is the output of *at most one* gate.

; Examples:

; (b-p (ckt-wires eq-ckt1) '(x y z w1 w2 w3 w4)) => 1
; (b-p (ckt-wires sel-ckt) '(x1 x0 y1 y0 s z1 z0 t u1 v1 u0 v0)) => 1
; (find-gate 'w3 eq-ckt1) => ((x y) w3 and)
; (find-gate 'v eq-ckt2) => ((w) v not)
; (find-gate 'y sel-ckt) => #f
;**********************************************************

; This remove procedure receives two arguments: one element and one list.
; This procedure scans the list and tries to find the element. If it can find
; the element it removes from the list.
(define remove
  (lambda (element lst)
    (cond
      ((equal? '() lst) '())
      ((equal? (car lst) element) (remove element (cdr lst)))
      (else (cons (car lst) (remove element (cdr lst)))))))

; It takes a list of Scheme values and returns
; a list of the top-level elements of lst with
; duplicates removed.
(define remove-duplicates
  (lambda (lst)
    (if (null? lst)
     '()
     (cons (car lst) (remove-duplicates(remove (car lst) (cdr lst)))))))

; This gets all the circuit wires and all the gates.
; Then combines all the circuit wires with the wires that are in the gates.
(define get-wires
  (lambda (ckt-w gates)
    (cond
      ((null? gates) '())
      (else
       (append (append ckt-w (append (gate-inputs (car gates)) (gate-inputs (car gates)))) (get-wires ckt-w (cdr gates)))))))

(define ckt-wires
  (lambda (ckt)
    (let ((input-w (ckt-inputs ckt)) (output-w (ckt-outputs ckt)) (gates (ckt-gates ckt)))
      (remove-duplicates (get-wires (append input-w output-w) gates)))))

(define find-gate
  (lambda (wire crc)
    (let ((gates (ckt-gates crc)))
    (cond
      ((null? gates) #f)
      ((equal? (gate-output (car gates)) wire) (car gates))
      (else
       (find-gate wire (ckt (ckt-inputs crc) (ckt-outputs crc) (cdr (ckt-gates crc)))))))))
        

;**********************************************************
; ** problem 2 ** (14 points) - Done
; Define circuits for a half-adder and a full-adder
; in the representation described above.

; Your half-adder should be called 
; ha-ckt 
; and should have input wires: x and y and 
; output wires: z and co, 
; where  z is the mod 2 sum of x and y, 
; and co is 1 if both x and y are 1.

; Your full-adder should be called 
; fa-ckt 
; and should have input wires: x, y, and ci and 
; output wires: z and co,
; where the value of z is the mod 2 sum of x, y, and ci,
; and the value of co is 1 if and only if at least two
; of x, y, and ci are 1.

; The order and names of the circuit input and output 
; wires should be as specified above, but 
; the number and names of internal wires (wires that
; are neither circuit inputs nor circuit outputs)
; are up to you.

; You'll be able to simulate your circuits once your simulator is
; working.
;**********************************************************

(define ha-ckt
  (ckt
   '(x y)
   '(z co)
   (list
    (gate '(x y) 'z 'xor)
    (gate '(x y) 'co 'and))))

(define fa-ckt
  (ckt
   '(x y ci)
   '(z co)
   (list
    (gate '(x y) 'w1 'xor)
    (gate '(w1 ci) 'z 'xor)
    (gate '(x y) 'w2 'and)
    (gate '(y ci) 'w3 'and)
    (gate '(x ci) 'w4 'and)
    (gate '(w2 w3) 'w5 'or)
    (gate '(w4 w5) 'co 'or))))

;**********************************************************
; A configuration of a circuit is a table giving a value (0 or 1)
; for each wire in the circuit.  Examples:

(define eq1-config1 '((x 0) (y 0) (z 0) (w1 0) (w2 0) (w3 0) (w4 0)))
(define eq1-config2 '((x 0) (y 0) (z 1) (w1 1) (w2 1) (w3 0) (w4 1)))
(define sel-config1 '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0)
  	      (z0 0) (t 0)  (u1 0) (v1 0) (u0 0) (v0 0)))
(define sel-config2 '((x1 1) (x0 1) (y1 0) (y0 0) (s 0) (z1 0)
		      (z0 0) (t 1)  (u1 0) (v1 0) (u0 0) (v0 0)))
(define latch-config1 '((x 0) (y 0) (q 0) (u 0)))
(define latch-config2 '((x 0) (y 1) (q 1) (u 0)))

;**********************************************************
; ** problem 3 ** (10 points) - Done
; Write a procedure (next-value wire ckt config)
; that returns the value on the given wire
; of the given circuit ckt, *after one gate delay*
; starting with the given configuration of the circuit.

; You may assume that the given wire is one
; of the wires of the given circuit, and that
; the configuration is a configuration of the
; given circuit.  If the wire is an input wire
; of the circuit, then its next value is the
; same as its value in config.  Otherwise, the
; wire is the output of a gate of the circuit
; and the gate function is applied to the values
; of the input wires in config to determine the
; next value.

; Note that this doesn't compute the "eventual" value
; of the wire, just the "next" value of the wire,
; after *one gate delay*.

; You may want to write an auxiliary procedure
; to look up the value of a wire in a configuration.

; Examples:
; (next-value 'w1 eq-ckt1 eq1-config1) => 1
; (next-value 'w4 eq-ckt1 eq1-config1) => 0
; (next-value 'z eq-ckt1 eq1-config2) => 1
; (next-value 'x0 sel-ckt sel-config1) => 1
; (next-value 'v1 sel-ckt sel-config1) => 1
; (next-value 'u0 sel-ckt sel-config2) => 1
;**********************************************************

; For (symbol-in? sym exp) the argument sym is a
; Scheme symbol, the argument exp is a "permitted
; expression", and the value returned should be #t
; if sym occurs in exp, and #f otherwise.
(define symbol-in?
  (lambda (sym exp)
    (cond
      ((null? exp) #f)
      ((and (not (list? exp)) (equal? sym exp)) #t)
      ((and (not (list? exp)) (not (equal? sym exp))) #f)
      (else (or (symbol-in? sym (car exp)) (symbol-in? sym (cdr exp)))))))

; This is an auxillary procedure for the eval-in-env procedure.
; It basically gets exp and env as the input.
; It returns the value equivalent to the symbol by searching the environment.
(define equival
  (lambda (exp env)
    (cond
      ((null? env) env)
      ((list? exp) exp)
      ((symbol-in? exp (car env)) (cadar env))
      (else
       (equival exp (cdr env))))))

; This is the helper procedure for the next value.
; It takes the name of the wire, the gates and the configuration.
; Then checks throught the gates, finds the gate with the matching wire name
; and evaluates the value of the inputs of the gates according to the corresponding
; function.
(define next-value-helper
  (lambda (wire gates config)
    (cond
      ((null? gates) (equival wire config))
      ((not (equal? wire (gate-output (car gates)))) (next-value-helper wire (cdr gates) config))
      ((equal? (gate-fn (car gates)) 'not) (b-not (equival (car (gate-inputs (car gates))) config)))
      ((equal? (gate-fn (car gates)) 'and) (b-and (equival (car (gate-inputs (car gates))) config)
                                                  (equival (cadr (gate-inputs (car gates))) config)))
      ((equal? (gate-fn (car gates)) 'or) (b-or (equival (car (gate-inputs (car gates))) config)
                                                  (equival (cadr (gate-inputs (car gates))) config)))
      ((equal? (gate-fn (car gates)) 'xor) (b-xor (equival (car (gate-inputs (car gates))) config)
                                                  (equival (cadr (gate-inputs (car gates))) config)))
      ((equal? (gate-fn (car gates)) 'nand) (b-nand (equival (car (gate-inputs (car gates))) config)
                                                  (equival (cadr (gate-inputs (car gates))) config)))
      ((equal? (gate-fn (car gates)) 'nor) (b-nor (equival (car (gate-inputs (car gates))) config)
                                                  (equival (cadr (gate-inputs (car gates))) config))))))

; (define next-value-helper
;   (lambda (wire gates config ckt)
;     (cond
;       ((null? gates) '())
;       ((or (symbol-in? wire (ckt-inputs ckt)) (symbol-in? wire (ckt-outputs ckt))) (equival wire config))
;       ((not (equal? wire (gate-output (car gates)))) (next-value-helper wire (cdr gates) config ckt))
;       ((equal? (gate-fn (car gates)) 'not) (b-not (equival (car (gate-inputs (car gates))) config)))
;       ((equal? (gate-fn (car gates)) 'and) (b-and (equival (car (gate-inputs (car gates))) config)
;                                                   (equival (cadr (gate-inputs (car gates))) config)))
;       ((equal? (gate-fn (car gates)) 'or) (b-or (equival (car (gate-inputs (car gates))) config)
;                                                   (equival (cadr (gate-inputs (car gates))) config)))
;       ((equal? (gate-fn (car gates)) 'xor) (b-xor (equival (car (gate-inputs (car gates))) config)
;                                                   (equival (cadr (gate-inputs (car gates))) config)))
;       ((equal? (gate-fn (car gates)) 'nand) (b-nand (equival (car (gate-inputs (car gates))) config)
;                                                   (equival (cadr (gate-inputs (car gates))) config)))
;       ((equal? (gate-fn (car gates)) 'nor) (b-nor (equival (car (gate-inputs (car gates))) config)
;                                                   (equival (cadr (gate-inputs (car gates))) config))))))


(define next-value
  (lambda (wire ckt config)
    (let ((gates (ckt-gates ckt)))
    (next-value-helper wire gates config))))
       
;**********************************************************
; ** problem 4 ** (10 points) - Done
; Write a procedure (next-config ckt config)
; that takes a circuit and a current configuration config
; and returns the "next" configuration of the circuit,
; after *one gate delay* has elapsed.

; In the "next" configuration of the circuit:
; the value of each wire is the result
; of applying the next-value procedure to the wire,
; circuit and the configuration config.

; This means that the input values do not
; change and other values propagate through the
; circuit *one gate delay* at a time.
; This is a very simplified model of
; the time-varying behavior of wires and gates!

; Examples:
; (b-p (next-config eq-ckt1 eq1-config1) '((x 0) (y 0) (z 0) (w1 1) (w2 1) (w3 0) (w4 0))) => 1
; (b-p (next-config eq-ckt1 eq1-config2) '((x 0) (y 0) (z 1) (w1 1) (w2 1) (w3 0) (w4 1))) => 1
; (b-p (next-config sel-ckt sel-config1) '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) (t 0) (u1 0) (v1 1) (u0 0) (v0 0))) => 1
; (b-p (next-config latch-ckt latch-config2) '((x 0) (y 1) (q 1) (u 0))) => 1
; (b-p (next-config latch-ckt latch-config1) '((x 0) (y 0) (q 1) (u 1))) => 1

; (b-p (next-config sel-ckt (next-config sel-ckt sel-config1)) '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 1) (z0 0) (t 0) (u1 0) (v1 1) (u0 0) (v0 0)))  => 1
;**********************************************************

; (define next-config
;   (lambda (ckt config)
;     (next-config-helper (ckt-inputs ckt) (ckt-gates ckt) config)))
; 
; (define next-config-helper
;   (lambda (ckt-inp gates config)
;       (cond
;         ((null? gates) config)
;         ((and (equal? (gate-fn (car gates)) 'not) (symbol-in? (car (gate-inputs (car gates))) ckt-inp))
;          (next-config-helper ckt-inp (cdr gates) (change-config config (next-value-helper (gate-output (car gates)) gates config) (gate-output (car gates)))))
;         ((and (symbol-in? (car (gate-inputs (car gates))) ckt-inp) 
;               (symbol-in? (cadr (gate-inputs (car gates))) ckt-inp))
;          (next-config-helper ckt-inp (cdr gates) (change-config config (next-value-helper (gate-output (car gates)) gates config) (gate-output (car gates)))))
;         (else
;          (next-config-helper ckt-inp (cdr gates) config)))))
; 
; (define change-config
;   (lambda (config value wire-name)
;     (cond
;       ((null? config) '())
;       ((symbol-in? wire-name (car config)) (cons (append (list wire-name) (list value)) (cdr config)))
;       (else
;        (cons (car config) (change-config (cdr config) value wire-name))))))


; (define delete-inp
;   (lambda (inp-wire wires)
;     (cond
;       ((null? inp-wire) wires)
;       (else
;        (delete-inp (cdr inp-wire) (remove (car inp-wire) wires))))))
; 
; (define delete-out
;   (lambda (out-wire wires)
;     (cond
;       ((null? out-wire) wires)
;       (else
;        (delete-out (cdr out-wire) (remove (car out-wire) wires))))))
; 
; (define next-config
;   (lambda (ckt config)
;     (let ((out-wires (delete-inp (ckt-inputs ckt) (ckt-wires ckt))))
;       (next-c-helper (delete-out (ckt-outputs ckt) out-wires) (ckt-gates ckt) config))))
; 
; (define next-c-helper
;   (lambda (out gates config)
;     (cond
;       ((null? gates) config)
;       ((and (equal? (gate-fn (car gates)) 'not) (not (symbol-in? (car (gate-inputs (car gates))) out)))
;        (next-c-helper out (cdr gates) (change-config config (next-value-helper (gate-output (car gates)) gates config) (gate-output (car gates)))))
;       ((and (not (symbol-in? (car (gate-inputs (car gates))) out)) (not (symbol-in? (cadr (gate-inputs (car gates))) out)))
;        (next-c-helper out (cdr gates) (change-config config (next-value-helper (gate-output (car gates)) gates config) (gate-output (car gates)))))
;       (else
;        (next-c-helper out (cdr gates) config)))))


; This is the helper procedure for the next-config procedure.
; It takes the all the wires in the circuit, the circuit, the configuration and another configuration for storage purposes.
; It basically applies next-value procedure to every wire, except the input wires of the circuit and then updates the
; the configuration by using the change-config procedure.
(define next-config-helper
  (lambda (wires ckt config old-config)
    (cond
      ((null? wires) config)
      ((null? (next-value (car wires) ckt old-config)) (next-config-helper (cdr wires) ckt config old-config))
      (else
       (next-config-helper (cdr wires) ckt (change-config config (next-value (car wires) ckt old-config) (car wires)) old-config)))))

; This basically gets the original configuration, the value to change and the wire name,
; then it updates the configuration by first finding the wire and updating the value corresponding to it.
(define change-config
  (lambda (config value wire-name)
    (cond
      ((null? config) '())
      ((symbol-in? wire-name (car config)) (cons (append (list wire-name) (list value)) (cdr config)))
      (else
       (cons (car config) (change-config (cdr config) value wire-name))))))

(define next-config
  (lambda (ckt config)
    (let ((wires (ckt-wires ckt)))
      (next-config-helper wires ckt config config))))

;**********************************************************
; ** problem 5 ** (10 points) - Done
; Write three procedures

; (stable? ckt config)
; (init-config ckt input-values)
; (output-values ckt config)

; (stable? ckt config)
; returns #t if the next configuration
; of circuit ckt from the configuration config
; is the same as config.

; (init-config ckt input-values)
; returns the configuration of the circuit ckt
; with the circuit input wires assigned the given input values, 
; (paired up in the order of the list of input wires)
; and all other wires  assigned the value 0.

; (output-values ckt config)
; returns a table giving the values of each
; of the output wires of circuit ckt in the
; configuration config; the order is that of
; the list of output wires of the circuit.

; Examples:

; (stable? eq-ckt1 '((x 0) (y 0) (z 1) (w1 1) (w2 1) (w3 0) (w4 1))) => #t
; (stable? eq-ckt1 '((x 0) (y 0) (z 0) (w1 1) (w2 0) (w3 1) (w4 0))) => #f
; (stable? latch-ckt '((x 0) (y 1) (q 0) (u 0))) => #f
; (stable? latch-ckt '((x 1) (y 1) (q 1) (u 0))) => #t
; (stable? clock-ckt '((z 0))) => #f

; (b-p (init-config eq-ckt1 '(1 1)) '((x 1) (y 1) (z 0) (w1 0) (w2 0) (w3 0) (w4 0))) => 1
; (b-p (init-config eq-ckt1 '(0 1)) '((x 0) (y 1) (z 0) (w1 0) (w2 0) (w3 0) (w4 0))) => 1
; (b-p (init-config eq-ckt2 '(1 0)) '((x 1) (y 0) (v 0) (w 0))) => 1
; (b-p (init-config clock-ckt '()) '((z 0))) => 1
; (b-p (init-config sel-ckt '(1 0 0 1 1)) '((x1 1) (x0 0) (y1 0) (y0 1) (s 1) (z1 0) (z0 0) (t 0) (u1 0) (v1 0) (u0 0) (v0 0))) => 1

; (output-values eq-ckt1 eq1-config2) => ((z 1))
; (output-values sel-ckt sel-config2) => ((z1 0) (z0 0))
; (output-values latch-ckt latch-config1) => ((q 0) (u 0))
; (output-values latch-ckt latch-config2) => ((q 1) (u 0))
;**********************************************************

(define stable?
  (lambda (ckt config)
    (let ((next (next-config ckt config)))
      (cond
        ((equal? next config) #t)
        (else
         #f)))))

; This is the helper procedure for the init-config.
; It basically takes all the wires and the inputs
; and then combines them. When the inputs become empty,
; it just adds 0.
(define init-helper
  (lambda (wires inp)
    (cond
      ((null? wires) '())
      ((null? inp) (cons (list (car wires) '0) (init-helper (cdr wires) inp)))
      (else
       (cons (list (car wires) (car inp)) (init-helper (cdr wires) (cdr inp)))))))

(define init-config
  (lambda (ckt input-values)
    (let ((wires (ckt-wires ckt)))
      (init-helper wires input-values))))

; This is the helper method for output values. It gets the output wires of the circuit
; and the configuration. And then returns them all as a table.
(define out-helper
  (lambda (out config)
    (cond
      ((null? out) '())
      (else
       (cons (get-val (car out) config) (out-helper (cdr out) config))))))

; This procedure gets the output wire of the circuit and the configuration,
; then finds the wire in the configuration and returns that wire-value pair.
(define get-val
  (lambda (out config)
    (cond
      ((null? config) '())
      ((symbol-in? out (car config)) (car config))
      (else
       (get-val out (cdr config))))))

(define output-values
  (lambda (ckt config)
    (let ((out (ckt-outputs ckt)))
      (out-helper out config))))

;**********************************************************
; ** problem 6 ** (10 points) - Done
; Write a procedure (simulate ckt config n)
; which simulates the given circuit from the given
; configuration by repeatedly calling next-config
; until either
; the configuration reached is stable, or
; next-config has been called n times, whichever occurs first.

; Examples:

; (simulate clock-ckt '((z 0)) 5) => 
; (((z 0)) ((z 1)) ((z 0)) ((z 1)) ((z 0)) ((z 1)))

; (b-pp (simulate eq-ckt1 eq1-config1 5) '(((x 0) (y 0) (z 0) (w1 0) (w2 0) (w3 0) (w4 0)) ((x 0) (y 0) (z 0) (w1 1) (w2 1) (w3 0) (w4 0)) ((x 0) (y 0) (z 0) (w1 1) (w2 1) (w3 0) (w4 1))  ((x 0) (y 0) (z 1) (w1 1) (w2 1) (w3 0) (w4 1)))) => 1

; (b-pp (simulate sel-ckt sel-config1 5) '(((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) (t 0) (u1 0) (v1 0) (u0 0) (v0 0)) ((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) (t 0) (u1 0) (v1 1) (u0 0) (v0 0)) ((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 1) (z0 0) (t 0) (u1 0) (v1 1) (u0 0) (v0 0)))) => 1

; (b-pp (simulate latch-ckt latch-config2 3) '(((x 0) (y 1) (q 1) (u 0)))) => 1

; (b-pp (simulate eq-ckt2 (init-config eq-ckt2 '(0 1)) 5) '(((x 0) (y 1) (v 0) (w 0)) ((x 0) (y 1) (v 1) (w 1)) ((x 0) (y 1) (v 0) (w 1)))) => 1

;**********************************************************

(define simulate
  (lambda (ckt config n)
    (cond
      ((or (stable? ckt config) (equal? n 0)) (list config))
      (else
       (cons config (simulate ckt (next-config ckt config) (- n 1)))))))

;**********************************************************
; ** problem 7 ** (10 points) - Done
; Write a procedure

; (final-outputs ckt input-values)

; that takes a circuit ckt and a list input-values
; of values for its input wires.

; If the circuit would eventually reach a stable configuration 
; from the initial configuration given by (init-config ckt input-values)
; then (final-outputs ckt input-values) returns the
; values on the output wires in the stable configuration
; (in the format of (output-values ckt config).)

; Otherwise, (final-outputs ckt input-values) returns
; the symbol none.

; Examples:
; (final-outputs clock-ckt '()) => none
; (final-outputs eq-ckt1 '(1 1)) => ((z 1))
; (final-outputs sel-ckt '(0 1 1 0 0)) => ((z1 0) (z0 1))
; (final-outputs latch-ckt '(1 0)) => ((q 0) (u 1))
;**********************************************************

; This is the helper procedure for the final-outputs procedure.
; It gets the circuit, the configuration (after init-config has been applied) and the number of trials.
; Then it basically performs the abovementioned description of the procedure.
(define final-out-helper
  (lambda (ckt config n)
    (cond
      ((equal? n 0) 'none)
      ((stable? ckt config) (output-values ckt config))
      (else
       (final-out-helper ckt (next-config ckt config) (- n 1))))))

(define final-outputs
  (lambda (ckt input-values)
    (let ((init (init-config ckt input-values)))
      (final-out-helper ckt init (expt 2 (length (ckt-gates ckt)))))))
        
;**********************************************************
; ** problem 8 ** (15 points) - Done
; Define a 4-bit ripple-carry adder circuit as described in lecture
; using the circuit representation developed above.

; Please name it: adder-ckt.

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

; For example:
; (final-outputs adder-ckt '(1 0 0 1 1 1 0 1)) =>
; ((z4 1) (z3 0) (z2 1) (z1 1) (z0 0))

; You may construct the circuit entirely by hand.
; Or you may use procedures to construct the circuit.
;**********************************************************

(define adder-ckt
  (ckt
   '(x3 x2 x1 x0 y3 y2 y1 y0)
   '(z4 z3 z2 z1 z0)
   (list
    (gate '(x0 y0) 'z0 'xor)
    (gate '(x0 y0) 'c1 'and)
    
    (gate '(x1 y1) 'w1 'xor)
    (gate '(w1 c1) 'z1 'xor)
    (gate '(x1 y1) 'w2 'and)
    (gate '(y1 c1) 'w3 'and)
    (gate '(x1 c1) 'w4 'and)
    (gate '(w2 w3) 'w5 'or)
    (gate '(w4 w5) 'c2 'or)
    
    (gate '(x2 y2) 'w6 'xor)
    (gate '(w6 c2) 'z2 'xor)
    (gate '(x2 y2) 'w7 'and)
    (gate '(y2 c2) 'w8 'and)
    (gate '(x2 c2) 'w9 'and)
    (gate '(w7 w8) 'w10 'or)
    (gate '(w9 w10) 'c3 'or)
    
    (gate '(x3 y3) 'w11 'xor)
    (gate '(w11 c3) 'z3 'xor)
    (gate '(x3 y3) 'w12 'and)
    (gate '(y3 c3) 'w13 'and)
    (gate '(x3 c3) 'w14 'and)
    (gate '(w12 w13) 'w15 'or)
    (gate '(w14 w15) 'z4 'or))))

;**********************************************************
; ** problem 9 ** (10 points) - Done
; Write a procedure (ckt->exp var ckt)
; whose inputs are a well-formed *combinational* circuit ckt 
; and a wire name var which is a circuit input or gate output
; of ckt.  The procedure returns an expression over the circuit inputs 
; giving the value of var in terms of the circuit inputs.
; The format of the expression is an extension of the
; format used for Boolean expressions in the previous assignment,
; with the symbols xor, nand, and nor representing functions
; XOR, NAND, and NOR, in addition to - for NOT, + for OR and * for AND.

; Hint:  a recursive method works nicely, 
; working "backwards" from outputs to inputs.
; Expressions *logically equivalent* to those below
; are also acceptable answers.

; Examples:
; (ckt->exp 'z eq-ckt1) => (+ (* x y) (* (- x) (- y)))
; (ckt->exp 'x eq-ckt1) => x
; (ckt->exp 'w3 eq-ckt1) => (* x y)
; (ckt->exp 'w4 eq-ckt1) => (* (- x) (- y))
; (ckt->exp 'v eq-ckt2) => (- (xor x y))
; (ckt->exp 'w eq-ckt2) => (xor x y)
; (ckt->exp 'z0 sel-ckt) => (+ (* x0 (- s)) (* y0 s)))
;**********************************************************

; It gets the var, the gates the ckt and the gates for saving purposes.
; It first finds the var in the gate outputs and then traces it back
; by using recursion. It uses normal recursion and cons every element together.
(define helper
  (lambda (var gates ckt old)
    (cond
      ((null? gates) var)
      ((symbol-in? var (ckt-inputs ckt)) var)
      ((not (equal? var (gate-output (car gates)))) (helper var (cdr gates) ckt old))
      ((equal? (gate-fn (car gates)) 'not) (list '- (helper (car (gate-inputs (car gates))) old ckt old)))
      (else
       (cons (get-oper (car gates)) (append (list (helper (car (gate-inputs (car gates))) old ckt old)) (list (helper (cadr (gate-inputs (car gates))) old ckt old))))))))

; This procedure gets the gate as an input and returns the name of the boolean function.
(define get-oper
  (lambda (gate)
    (cond
      ((equal? (gate-fn gate) 'and) '*)
      ((equal? (gate-fn gate) 'or) '+)
      ((equal? (gate-fn gate) 'xor) 'xor)
      ((equal? (gate-fn gate) 'nand) 'nand)
      ((equal? (gate-fn gate) 'nor) 'nor))))

(define ckt->exp
  (lambda (var ckt)
    (let ((gates (ckt-gates ckt)))
      (helper var gates ckt gates))))
;**************  end of hw # 5  ************************************
