#lang racket

; Abrir archivos
(define file-write-inv (open-output-file
           "C:\\Users\\marco\\Documents\\Tareas\\Implementación\\Evidencia_2\\data\\i4.txt"
            #:exists 'replace))

(define file-write-trans (open-output-file
           "C:\\Users\\marco\\Documents\\Tareas\\Implementación\\Evidencia_2\\data\\t4.txt"
            #:exists 'replace))


; Definir productos
(define productos
  ; Producto Cantidad Precio
  '((A 10 5)
    (B 8 9)
    (C 6 13)
    (D 6 15)
    (E 5 18)
    (F 5 25)
    (G 5 30)
    (H 5 44))
)

; Definir monedas
(define monedas
  ; Valor Cantidad Max
  '((1 30 40)
    (2 10 20)
    (5 2 20)
    (10 0 20)
    (20 0 10)
    (50 0 10)) 
)

; Definir transacciones
(define transacciones
  ; Id Producto Monedas
  '((1 G (50 50))
    (2 G (10 10 1 1 1 1 1 1 1 1 1 1))
    (3 G (10 10 1 1 1 1 1 1 1 1 1 1)))
)


; Guardar datos de inventario
(write productos file-write-inv)
(write monedas file-write-inv)

; Guardar transacciones
(write transacciones file-write-trans)

; Cerrar archivos
(close-output-port file-write-inv)
(close-output-port file-write-trans)
