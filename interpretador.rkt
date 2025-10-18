#lang eopl
(require racket/string)

; ---------------------------------------
; Taller 3 - Intérprete
; Autores:
; Jhon Antony Murillo Olave – 2240927
; Jerson Alexis Ortiz Velasco – 2410014
; Curso: Fundamentos de Interpretación y Compilación
; URL del repositorio: https://github.com/Jerson1005/Taller3-FLP.git
; ---------------------------------------


;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      <un-programa (exp)>
;;
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;
;;                  ::= "\"" <identifier> "\""
;;                      <texto-lit (txt)>
;;
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;
;;                  ::= <primitiva-unaria> (<expresion>)
;;                      <primapp-un-exp (prim-unaria exp)>
;;
;;                  ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;
;;                  ::= declarar (<identificador> = <expresion> (;)) { <expresion> }
;;                      <variableLocal-exp (ids exps cuerpo)>
;;
;;                  ::= procedimiento (<identificador>*",") haga <expresion> finProc
;;                      <procedimiento-exp (ids cuerpo)>
;;
;;                  ::= evaluar <expresion> (<expresion> ",")* finEval
;;                      <app-exp(exp exps)>
;;
;;                  ::= recursivo { <identificador> ({ <identificador> }* ",") = <expresion> }* en <expresion>
;;                     <letrec-exp (proc-names idss bodies bodyletrec)>
;;
;;  <primitiva-binaria> :=  + (primitiva-suma)
;;                      :=  ~ (primitiva-resta)
;;                      :=  / (primitiva-div)
;;                      :=  * (primitiva-multi)
;;                      :=  concat (primitiva-concat)
;;
;;  <primitiva-unaria>  :=  longitud (primitiva-longitud)
;;                      :=  add1 (primitiva-add1)
;;                      :=  sub1 (primitiva-sub1)

;******************************************************************************************

;Especificación Léxica
(define scanner-spec-simple-interpreter
  '((espacio
     (whitespace) skip)
    
    (comentario
     ("%" (arbno (not #\newline))) skip)

    (identifier
     ("@" (arbno (or letter digit "_" "?"))) symbol)

    (string
     ("\"" (arbno (not #\")) "\"") string)


    (number
     ("-" digit (arbno digit) "." (arbno digit)) number)
    (number
     (digit (arbno digit) "." (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit)) number)))


;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (string) texto-lit)
    (expresion (identifier) var-exp)

    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)

    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI")
                condicional-exp)

    (expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "{" (arbno expresion) "}")
                variableLocal-exp)

    (expresion ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expresion "finProc")
                procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)

    (expresion ("recursivo" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "en" expresion)
               letrec-exp)
  

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("concat") primitiva-concat)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)))


;Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))


;Procedimientos
(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))
    (exp expresion?)
    (env environment?)))

;; apply-procedure : procVal × (list-of values) -> value
(define (apply-procedure proc args)
  (cases procVal proc
    (cerradura (param-ids body saved-env)
      (let ((n (length param-ids)))
        (when (not (= n (length args)))
          (eopl:error 'apply-procedure
                      "Aridad distinta: espera ~s argumentos, recibió ~s"
                      n (length args)))
        ;; extender el ambiente con param-ids -> args y evaluar el cuerpo ahí
        (eval-expression body (extend-env param-ids args saved-env))))))



;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e) 
     '(1 2 3 "hola" "FLP") 
     (empty-env))))

; buscar-variable: symbol × environment -> valor
; Busca una variable en el ambiente y devuelve su valor.
; Si no se encuentra, devuelve un mensaje de error.
(define apply-env
  (lambda (env sym)
    (deref (buscar-variable env sym))))
   
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable ~s no existe" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (buscar-variable env sym)))))))


;; unwrap-proc-body : expresion -> expresion
;; Propósito: si la expresión es un procedimiento (procedimiento-exp (ids body))
;;             devuelve el 'body' interno; en caso contrario devuelve la expresión.
(define (unwrap-proc-body e)
  (cases expresion e
    (procedimiento-exp (ids body) body)
    (else e)))

;; extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;; Propósito: construir un ambiente extendido para definiciones recursivas
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              ;; si el body es un procedimiento-exp, extrae su body interno
              (let ((actual-body (unwrap-proc-body body)))
                (vector-set! vec pos (cerradura ids actual-body env))))
            (iota len) idss bodies)
          env)))))


;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))


;;; process-string-lexeme : string|symbol -> string
;;; Propósito: recibe el lexema producido por el scanner para un literal de texto,
;;;            elimina comillas exteriores (si existen) y aplica desescape
;;;            de las secuencias comunes: \\n, \\t, \\\", \\\\.
(define (process-string-lexeme s)
  (let*
      ((raw (cond [(string? s) s]
                  [(symbol? s) (symbol->string s)]
                  [else (eopl:error 'process-string-lexeme "texto-lit: formato inválido ~s" s)]))
       (len (string-length raw))
       ;; quitar las comillas externas si el scanner devolvió "...."
       (trimmed (if (and (>= len 2)
                         (char=? (string-ref raw 0) #\")
                         (char=? (string-ref raw (- len 1)) #\"))
                    (substring raw 1 (- len 1))
                    raw))
       ;; aplicar desescape simple
       (step1 (string-replace trimmed "\\\\" "\\"))
       (step2 (string-replace step1 "\\\"" "\""))
       (step3 (string-replace step2 "\\n" (string #\newline)))
       (step4 (string-replace step3 "\\t" (string #\tab))))
    step4))


;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (var-exp (id) (apply-env env id))

      ;; texto-lit (txt) -> string
      (texto-lit (txt)
                 (process-string-lexeme txt))

      ;; PRIMITIVA BINARIA: forma concreta (exp1 prim-binaria exp2)
      (primapp-bin-exp (exp1 prim exp2)
        (let ((v1 (eval-expression exp1 env))
              (v2 (eval-expression exp2 env)))
          ;; apply-primitive espera el nodo prim y una lista de valores
          (apply-primitiva-binaria prim (list v1 v2))))

      ;; PRIMITIVA UNARIA: forma concreta prim ( exp )
      (primapp-un-exp (prim exp1)
        (let ((v (eval-expression exp1 env)))
          (apply-primitiva-unaria prim (list v))))

      ;; Condicional
      (condicional-exp (test-exp true-exp false-exp)
                       ;; if-exp (test then else)
                       (let ((v (eval-expression test-exp env)))
                         (if (valor-verdad? v)
                             (eval-expression true-exp env)
                             (eval-expression false-exp env))))

      ;; Declarar
      (variableLocal-exp (ids exps body)
        ;; evaluar todos los rhs en el ambiente actual (evaluación simultánea)
        (let ((vals (map (lambda (e) (eval-expression e env)) exps)))
          (if (not (= (length ids) (length vals)))
              (eopl:error 'variableLocal-exp "declarar: cantidad de ids y expresiones no coincide")
              (let ((new-env (extend-env ids vals env)))
                (eval-sequence body new-env)))))

      ;; Crear procedimiento: devuelve una cerradura (captura el ambiente actual)
      (procedimiento-exp (ids body)
        (cerradura ids body env))

      ;; Evaluar ... finEval  -> app-exp (proc-expr args)
      (app-exp (rator rands)
  (let ((proc (eval-expression rator env))
        (args (eval-rands rands env)))
    (if (procVal? proc)                   
        (apply-procedure proc args)
        (eopl:error 'eval-expression "Intento de aplicar no-procedimiento: ~s" proc))))

      ;; Recursión: permite definir procedimientos recursivos mutuamente
      (letrec-exp (proc-names idss bodies body-letrec)
                  (eval-expression body-letrec
                                   (extend-env-recursively proc-names idss bodies env)))
      )))


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define (eval-rands rands env)
  (if (null? rands)
      '()
      (cons (eval-expression (car rands) env)
            (eval-rands (cdr rands) env))))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


; apply-primitiva-binaria : primitiva-binaria × list -> valor
(define apply-primitiva-binaria
  (lambda (prim args)
    (let ((n (length args)))
      (cases primitiva-binaria prim
        (primitiva-suma ()
          (if (= n 2)
              (+ (car args) (cadr args))
              (eopl:error 'primitiva-suma "espera 2 argumentos")))

        (primitiva-resta ()
          (if (= n 2)
              (- (car args) (cadr args))
              (eopl:error 'primitiva-resta "espera 2 argumentos")))

        (primitiva-multi ()
          (if (= n 2)
              (* (car args) (cadr args))
              (eopl:error 'primitiva-multi "espera 2 argumentos")))

        (primitiva-div ()
          (if (= n 2)
              (if (zero? (cadr args))
                  (eopl:error 'primitiva-div "división por cero")
                  (/ (car args) (cadr args)))
              (eopl:error 'primitiva-div "espera 2 argumentos")))

        (primitiva-concat ()
          (if (= n 2)
              (let ((a (car args)) (b (cadr args)))
                (if (and (string? a) (string? b))
                    (string-append a b)
                    (eopl:error 'primitiva-concat "espera 2 strings")))
              (eopl:error 'primitiva-concat "espera 2 argumentos")))))))

; apply-primitive-unaria : primitiva-unaria × list -> valor
(define apply-primitiva-unaria
  (lambda (prim args)
    (let ((n (length args)))
      (cases primitiva-unaria prim
        (primitiva-add1 ()
          (if (= n 1)
              (+ (car args) 1)
              (eopl:error 'primitiva-add1 "espera 1 argumento")))

        (primitiva-sub1 ()
          (if (= n 1)
              (- (car args) 1)
              (eopl:error 'primitiva-sub1 "espera 1 argumento")))

        (primitiva-longitud ()
          (if (= n 1)
              (let ((a (car args)))
                (cond
                  ((string? a) (string-length a))
                  ((list? a) (length a))
                  (else (eopl:error 'primitiva-longitud "espera string o lista"))))
              (eopl:error 'primitiva-longitud "espera 1 argumento")))))))


; valor-verdad? : any -> Bool
; En números: 0 => #f, cualquier otro número => #t
; Si ya es booleano, lo devuelve tal cual
; Para otros tipos, lanza error
(define (valor-verdad? v)
  (cond
    [(number? v) (not (zero? v))]
    [(boolean? v) v]
    [else (eopl:error 'valor-verdad? "Se esperaba número o booleano, recibió: ~s" v)]))


; eval-sequence : (list-of expresion) × environment -> value
(define (eval-sequence seq env)
  (if (null? seq)
      #f
      (let loop ((s seq) (res #f))
        (if (null? s)
            res
            (loop (cdr s) (eval-expression (car s) env))))))


;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))
;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


; Ejercicios:

; a)
; 
; declarar (
;      @pi = 3.141592653589793;
;      @radio = 2.5
;    ) {
;      declarar (
;        @areaCirculo = procedimiento (@r) haga ( @pi * @r ) finProc
;      ) {
;        ( evaluar @areaCirculo(@radio) finEval * @radio )
;      }
;    }



;b)


; ;c)
; 
; recursivo
;      @sumar(@x,@y) = procedimiento (@x,@y) haga
;        Si @x entonces
;          add1( evaluar @sumar( sub1(@x) , @y ) finEval )
;        sino
;          @y
;        finSI
;      finProc
;    en
;      evaluar @sumar(4,5) finEval
; 



