Palacios Gómez Ernesto Rubén
Flores González Luis Brandon
Ley Flores Santiago

-------------------------------

¿Como se trataria el caso en que la lista tenga longitud impar?
En el caso cuando la lista es impar el elemento de enmedio se toma que esta relacionado con la lista vacia 
lo que hace que el ultimo elemento de la lista resultante sea la lista que solo contiene a ese elemento.

a) De todas las implementaciones para generar la lista de pares, ¿Cuál es el más eficiente?

La más eficiente es la generada por recursión de cola, ya que esta es más eficiente que la recursión
simple(se explica en el siguiente inciso) y CPS tenemos la materialización de la pila que remplaza la pila
de control actual por otra. En otras palabras, con recursión de cola no esta esperando a que se le regrese una
llamada recursiva.

b) Considerando el siguiente enunciado: «Los programas que utilizan recursión optimizados
con recursión de cola son más eficientes que aquellos que usan recursión simple»
(Abelson et al., 1996). Explicar y argumentar si se está de acuerdo o no con el enunciado.

Si, ya que la recursión simple implica la creación de un nuevo marco en la pila de llamadas y 
en la recursión de cola es posible realizar dichas llamadas recursivas reaprovechando el marco de la pila
anterior.

c)Dar un enunciado similar al anterior que compare las tre técnicas: recursión simple,
recursión de cola y continuation passing style.

La recursión de cola es la más eficiente, ya que en las otras dos esperas un resultado lo que genera otro registro
de activación.