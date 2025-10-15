#lang eopl
; ---------------------------------------
; Taller 3 - Intérprete
; Autores:
;Jhon Antony Murillo Olave – 2240927
;Jerson Alexis Ortiz Velasco – 2410014
; Curso: Fundamentos de Interpretación y Compilación
; URL del repositorio: https://github.com/Jerson1005/Taller3-FLP.git
; ---------------------------------------


; =======================================
; 1) Definición de la gramática y tipos
; =======================================
; Aquí se usará define-datatype para cada tipo de expresión
; Ejemplo: numero-lit, texto-lit, var-exp, primapp-bin-exp, etc.


; =======================================
; 2) Ambiente inicial y función buscar-variable
; =======================================
; - Definir estructura del ambiente (lista de pares)
; - Crear función (buscar-variable id ambiente)
; - Agregar variables iniciales @a, @b, @c, @d, @e


; =======================================
; 3) Implementación de valor-verdad?
; =======================================
; - Verifica si un número es 0 o diferente de 0


; =======================================
; 4) Condicionales
; =======================================
; - Agregar "Si ... entonces ... sino ... finSI"
; - Evaluar según valor-verdad?


; =======================================
; 5) Declaración de variables locales
; =======================================
; - Implementar "declarar (...) { cuerpo }"
; - Extender ambiente


; =======================================
; 6) Procedimientos (cerraduras)
; =======================================
; - Definir procVal
; - Implementar "procedimiento (...) haga ... finProc"


; =======================================
; 7) Evaluar procedimientos
; =======================================
; - Implementar "evaluar @proc (args) finEval"
; - Ligar parámetros con argumentos


; =======================================
; 8) Llamadas recursivas
; =======================================
; - Extender gramática para permitir recursión


; =======================================
; 9) Programas de prueba (a–f)
; =======================================
; - a) Área de círculo
; - b) Factorial
; - c) Suma recursiva
; - d) Restar y multiplicar con add1/sub1
; - e) Decoradores
; - f) Decorador con mensaje adicional

