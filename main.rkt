#lang racket

#| --- FUNCIÓN PRINCIPAL ---
Función principal que procesa las transacciones y regresa la ganancia total

Recibe                                                     Regresa
(ganancia acumulada, transacciones, archvio de lectura) -> (ganancia total) |#


(define (main ganancia transacciones archivo-lectura)

  ; Se leen los inventarios
  (define inv-productos (read archivo-lectura))
  (define inv-monedas (read archivo-lectura))

  ; Se cierra el archivo
  (close-input-port archivo-lectura)
  
  (cond
    
    ; Si se han procesado todas las transacciones, regresa la ganancia total
    [(null? transacciones) ganancia]

    ; Llama a main recursivamente
    [else (main
           
        ; Se procesa transaccion actual y se suma su ganancia con la ganancia acumulada
        (+ ganancia
           (procesa-transaccion (car transacciones) inv-productos inv-monedas (void) (void) (void)))

        ; Pasa  a la siguiente transaccion
        (cdr transacciones)

        ; Se vuelve a abrir el archivo
        (open-input-file inv-file-path)
           
       )
    ]
  )
)



#| --- PROCESA TRANSACCIÓN ---
Función que procesa una transacción una transacción y devuelve ganancia o imprime errores

Recibe                                                                                 Regresa
(transaccion, inv-productos, inv-monedas, resultado de precio, resultado de cambio, bool monedas agregadas) -> (ganancia de transaccion) |#


(define (procesa-transaccion transaccion inv-productos inv-monedas res-precio res-cambio m-agregadas)
  (cond

    ; --- VERIFICAR PRECIO Y PRODUCTO ---

    ; Si no hay res-precio es porque aún no se ha buscado el producto
    [(void? res-precio)

     ; Llamar a la función reecursivamente
     (procesa-transaccion transaccion inv-productos inv-monedas
                          ; Pasando como parametro de res-precio el resultado de buscar-producto
                          (buscar-producto (cadr transaccion)
                                           ; Se pasa un inventario para iterar sobre él
                                           inv-productos
                                           ; Se pasa otro inventario para restar la cantidad
                                           inv-productos)
                          res-cambio m-agregadas)]

    ; Validar si hubieron estados de salida en la busqueda de producto

    ; Si el res-precio es -1 es porque no se encontró el producto, marca error y regresa ganancia 0
    [(equal? res-precio -1)
     (imprimir 0 (car transaccion) "No se encontró producto" 'Null 0 0 (caddr transaccion)) 0]

    ; Si el res-precio es -2 es porque no hay inventario, marca error y regresa ganancia 0
    [(equal? res-precio -2)
     (imprimir 0 (car transaccion) "No hay inventario de producto" (cadr transaccion) 0 0 (caddr transaccion)) 0]


    ; --- VERIFICAR MONEDAS Y CALCULAR CAMBIO ---

    ; Si no hay res-cambio es porque aún no se han procesado las monedas y el cambio
    [(void? res-cambio)

     ; Llamar a la función recursivamente
     (procesa-transaccion transaccion inv-productos inv-monedas res-precio
                          ; Pasando como parametro de res-cambio el resultado de validar-monedas
                          (validar-monedas
                           ; Cantidad ingresada es la suma de las monedas ingresadas        
                           (apply + (caddr transaccion))
                           ; Monedas ingresadas
                           (caddr transaccion)
                           ; Pasar el precio de producto
                           (car res-precio) inv-monedas)
                          m-agregadas )
    ]

    ; Validar si hubieron estados de salida en la validación de monedas y calculo de cambio
    
    ; Si el res-cambio es -1 es porque no se alcanzó el precio del producto, regresa ganancia 0
    [(equal? res-cambio -1)
     (imprimir 0 (car transaccion) "Dinero no suficiente" (cadr transaccion) 0 0 (caddr transaccion)) 0]

    ; Si el res-cambio es -1 es porque se ingresó una moneda inválida, regresa ganancia 0
    [(equal? res-cambio -2)
     (imprimir 0 (car transaccion) "Se ingresó una moneda no válida" (cadr transaccion) 0 0 (caddr transaccion)) 0]

    ; Si el res-cambio es -3 es porque no hubo cambio suficiente, regresa ganancia 0
    [(equal? res-cambio -3)
     (imprimir 0 (car transaccion) "No es posible entregar cambio" (cadr transaccion) 0 0 (caddr transaccion)) 0]


    ; --- AGREGAR MONEDAS INGRESADAS A INVENTARIO ---

    ; Si no hay m-agregadas es porque aún no se han agregado las monedas
    [(void? m-agregadas)

     ; Llamar a la función recursivamente
     (procesa-transaccion transaccion inv-productos
                          ; Genera nuevo inventario usando el inventario resultado del cambio
                          (agregar-monedas (cadr res-cambio) (caddr transaccion))
                          ; Cambiar monedas agregadas a 1 para indicar operación realizada
                          res-precio res-cambio 1)
    ]

    ; Validar si hubieron estados de salida al agregar las monedas al inventario

    ; Si el inv-monedas es -1 es porque no hubo espacio suficiente, regresa ganancia 0
    [(equal? inv-monedas -1)
     (imprimir 0 (car transaccion) "No hay suficiente espacio en inventario" (cadr transaccion) 0 0 (caddr transaccion)) 0]


    ; --- ACTUALIZAR ARCHIVO ---

    ; Si la transacción fue exitosa
    [else
     
     ; Actualiza el archivo de inventarios
     (actualizar-archivo (open-output-file inv-file-path #:exists 'replace)
                         ; Nuevo inv-productos con la cantidad de producto disminuída resultado de buscar-producto
                         (cadr res-precio)
                         ; Inv-monedas
                         inv-monedas)

     ; Imprimir
     (imprimir 1 (car transaccion) "Venta exitosa" (cadr transaccion) (car res-precio) (apply + (caddr transaccion)) (car res-cambio))
     
     ; Regresa de ganancia el precio
     (car res-precio)]
    
  )
)



#| --- BUSCAR PRODUCTO ---
Función que busca el producto de la transacción

Recibe                       Regresa
(producto, inv-productos) -> ( '(precio, nuevo inv-productos) ó estado de salida) |#


(define (buscar-producto producto inv-productos todos-productos)
  (cond
    
    ; Si no se encontró regresa estado de salida -1
    [(null? inv-productos) -1]
    
    ; Si se encuentra el producto
    [(equal? producto (caar inv-productos))

     ; Checar que haya en inventario
     (cond
       
       ; Si no hay en inventario regresa estado de salida -2
       [(<= (cadar inv-productos) 0) -2]

       ; Si hay en inventario regresa lista con el precio
       [else (list (caddar inv-productos)
                   ; Y con el nuevo inv-productos
                   (modificar-inventario todos-productos '() producto -))]
       
     )
    ]
    
    ; Seguir buscando
    [else (buscar-producto producto (cdr inv-productos) todos-productos)]
    
  )
)



#| --- MODIFICAR INVENTARIO  ---
Función que regresa si es válida la modificación de un inventario dependiendo del operador

Recibe                                           Regresa
(inventario, inv-anterior, elemento, operador) -> (nuevo inventario) |#


(define (modificar-inventario inventario inv-anterior elemento operador)
  (cond

    ; Si se encuentran los datos del elemento buscado
    [(equal? elemento (caar inventario))
        
        ; Regresa una nueva lista que junta
        (append
         ; Los datos de los elementos anteriores
         inv-anterior
         ; Los datos del elemento buscado aplicando el operador a la cantidad en inventario
         (list (list (caar inventario) (operador (cadar inventario) 1) (caddar inventario) ))
         ; Los datos de los elementos que seguían
         (cdr inventario)
        )
    ]

    ; Llama recursivamente a la función pasando
    [else (modificar-inventario
           ; El resto de los elementos
           (cdr inventario)
           ; Añande los datos actuales a la lista de datos anteriores
           (append inv-anterior (list (car inventario)))
           elemento operador)]
    
  )
)



#| --- VALIDAR MONEDAS INGRESADAS ---
Función que valida las monedas ingresadas basandonos en un autómata y llama a calcula-cambio o regresa estado de salida

Recibe                                                           Regresa
(cantidad ingresada, monedas ingresadas, precio, inv-monedas) -> ('(cambio, nuevo-inv-monedas) ) |#


(define (validar-monedas cant-ingresada m-ingresadas precio inv-monedas)
  (cond

    #| Para saber si el estado final de las monedas (total insertado) es un estado aceptor simplemente
    se tuvo que realizar la suma de las monedas puesto que la misma función + recibe el estado actual (primer número)
    y la transición (segundo número) y regresa el estado destino (resultado de la suma)
    Esa suma se realizó al llamar a esta función |#

    ; Si el estado final no es estado aceptor (no es mayor que el precio) regresa estado de salida -1
    [(< cant-ingresada precio) -1]

    ; Iterar las monedas y verificar que sean sean válidas o regresar estado de salida -2
    [(> (apply + (map (lambda (moneda)
                (if (not (or (equal? moneda 1)
                             (equal? moneda 2)
                             (equal? moneda 5)
                             (equal? moneda 10)
                             (equal? moneda 20)
                             (equal? moneda 50))) 1 0)) m-ingresadas)) 0) -2]
    
    ; Si esta bien, llama a calcula-cambio y regresa el cambio y nuevo inventario o estados de salida
    [else (calcula-cambio
           ; La cantidad de cambio es la resta entre el dinero ingresado y el precio
           (- cant-ingresada precio) '() inv-monedas
           ; Pasar lista de valores de monedas en orden decreciente
           (reverse (map (lambda (datos-moneda) (car datos-moneda)) inv-monedas)))]
    
  )
)



#| --- CALCULA CAMBIO ---
Función que calcula el cambio de la transacción y genera el nuevo inventario de monedas

Recibe                                                        Regresa
(cantidad cambio, monedas de cambio, inv-monedas, valores) -> ( '(cambio, nuevo inv-monedas) ) |#


(define (calcula-cambio cant-cambio m-cambio inv-monedas valores)
  (cond

    ; Si la cantidad de cambio es 0, regresa una lista con las monedas de cambio y el nuevo inventario
    [(<= cant-cambio 0) (list m-cambio inv-monedas)]

    ; Si se han intentado todos los valores no hay cambio suficiente, regresa estado de salida -3
    [(null? valores) -3]

    [else  
     (cond

       ; Si el valor actual es más chico que la cantidad de cambio y su inventario se puede restar 1
       [(and (<= (car valores) cant-cambio) (> (modificar-inventario? inv-monedas (car valores) -) 0))
        
         ; Llama recursivamente a la función
         (calcula-cambio
          ; La nueva cantidad de cambio es la resta entre la cantidad actual y el valor actual
          (- cant-cambio (car valores))
          ; Se agrega la moneda actual a las monedas de cambio
          (append m-cambio (list (car valores)))
          ; Se genera nuevo inventario restando en 1 la cantidad
          (modificar-inventario inv-monedas '() (car valores) -)
          ; Se mantienen los valores para volver a checar el valor actual
          valores)]

       ; Si el valor no puede ser cambio intenta con el siguiente valor y sin cambiar los parámetros
       [else (calcula-cambio cant-cambio m-cambio inv-monedas (cdr valores))]
         
     )
    ]
  )
)



#| --- MODIFICAR-INVENTARIO? ---
Función que regresa si es válida la modificación del inventario de una moneda dependiendo del operador

Recibe                             Regresa
(inv-monedas, moneda, operador) -> (1 o estado de salida) |#


(define (modificar-inventario? inv-monedas moneda operador)
  (cond
    ; Si se encuentra la moneda
    [(equal? moneda (caar inv-monedas))
     (cond
       ; Si el operador es - y la moneda tiene de inventario 0, regresa estado de salida -1
       [(and (equal? operador -) (<= (cadar inv-monedas) 0)) -1]
       
       ; Si el operador es + y la cantidad es igual a la cantidad máxima, regresa estado de salida -2
       [(and (equal? operador +) (>= (cadar inv-monedas) (caddar inv-monedas))) -2]

       ; Si es válida la modificación, regresa 1
       [else 1]
     )
    ]

    ; Seguir buscando
    [else (modificar-inventario? (cdr inv-monedas) moneda operador)]
  )
)



#| --- AGREGAR MONEDAS ---
Función que regresa el nuevo inventario de monedas con las monedas agregadas o estado de salida

Recibe                             Regresa
(inv-monedas, monedas ingresadas) -> (nuevo inv-monedas o estado de salida) |#


(define (agregar-monedas inv-monedas monedas)
  (cond

    ; Si ya no hay monedas que agregar regresa el nuevo inventario
    [(null? monedas) inv-monedas]

    ; Si no se puede agregar moneda por inventario lleno, regresa estado de salida -1
    [(< (modificar-inventario? inv-monedas (car monedas) +) 0) -1]

    ; Llama recursivamente a la función
    [else (agregar-monedas
           ; Pasando el nuevo inventario con la cantidad de la moneda incrementada
           (modificar-inventario inv-monedas '() (car monedas) +)
           ; Analizar siguiente moneda
           (cdr monedas))
    ]
    
  )
)



#| --- ACTUALIZAR ARCHIVO ---
Función que actualiza el archivo con los nuevos inventarios

Recibe                                                Regresa
(archivo de escritura, inv-productos, inv-monedas) -> (void) |#


(define (actualizar-archivo write-inv-file inv-productos inv-monedas)
  
  ; Escribir inventarios
  (write inv-productos write-inv-file)
  (write inv-monedas write-inv-file)

  ; Cerrar archivo
  (close-output-port write-inv-file)
  
)



#| --- IMPRIMIR ---
Función que imprime el resultado de una transacción

Recibe                                                                                         Regresa
(estado de transacción, id transacción, mensaje, producto, precio, cant ingresada, monedas) -> (void) |#


(define (imprimir estado id mensaje producto precio ingresado monedas)

  ; Si el estado es 1 la transacción fue exitosa
  (if (equal? estado 1)
      (printf "~a. OK: ~a | Producto: ~a | Precio: ~a | Ingresado: ~a | Cambio: ~a\n"
              id mensaje producto precio ingresado monedas)

      ; Si el estado fue 0 hubo un error
      (printf "~a. ERROR: ~a | Producto: ~a | Se regresa: ~a\n"
              id mensaje producto monedas)
  )
)



#| --- REPORTES FINALES ---
Función que genera reportes finales sobre ganancia y alertas de inventarios

Recibe                                    Regresa
(ganancia, inv-productos, inv-monedas) -> (void) |#

(define (reportes-finales ganancia inv-productos inv-monedas)
  (printf "\n--- Reportes Finales ---\n")
  (printf "Ganancia: ~a\n" ganancia)
  (printf "Productos con poco inventario: ~a\n" (alerta-inventario inv-productos <= + 3))
  (printf "Monedas con poco inventario: ~a\n" (alerta-inventario inv-monedas <= + 2))
  (printf "Monedas con mucho inventario: ~a\n" (alerta-inventario inv-monedas > - 2))
)


#| --- ALERTA INVENTARIO ---
Función que genera lista de elementos en alerta

Recibe                                             Regresa
(tipo de inventario, inventario, <= ó >, + ó -, margen) -> (elementos en alerta) |#

(define (alerta-inventario inventario operador-1 operador-2 margen)
  (cond

    ; Si ya se recorrieron todos los elementos regresa lista vacía
    [(null? inventario) '()]

    ; Si el inventario es de productos
    [(operador-1 (cadar inventario) (operador-2 (if (equal? operador-1 <=) 0 (caddar inventario)) margen))
     ; Añade el elemento a la lista pasando el mismo margen
     (cons (caar inventario) (alerta-inventario (cdr inventario) operador-1 operador-2 margen))      
    ]

    ; Si el inventario es de monedas
    [else (alerta-inventario (cdr inventario) operador-1 operador-2 margen)]
    
  )
)



#| --- EJECUCIÓN --- |#

; Ingresa número de archivos
(displayln"\nNúmero de inventario y transacciones:")
(define n-archivos (read-line (current-input-port) 'any))

; Guardar transacciones
(define file-read-trans (open-input-file
               (string-append "C:\\Users\\marco\\Documents\\Tareas\\Implementación\\Evidencia_2\\data\\t" n-archivos ".txt")))
(define transacciones (read file-read-trans))
(close-input-port file-read-trans)

; Definir path del archivo inventario
(define inv-file-path (string-append "C:\\Users\\marco\\Documents\\Tareas\\Implementación\\Evidencia_2\\data\\i" n-archivos ".txt"))

; Ejecutar función main y obtener ganancia total
(printf "\n\n--- Transacciones ---\n")
(define ganancia (main 0 transacciones (open-input-file inv-file-path)))

; Abrir archivo de inventario y definir inventarios
(define file-read-inv (open-input-file inv-file-path))
(define inv-productos (read file-read-inv))
(define inv-monedas (read file-read-inv))
(close-input-port file-read-inv)

; Mostrar reportes finales
(reportes-finales ganancia inv-productos inv-monedas)
