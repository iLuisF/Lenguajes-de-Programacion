Palacios G�mez Ernesto Rub�n
Flores Gonz�lez Luis Brandon
Ley Flores Santiago

-------------------------------

�Como se trataria el caso en que la lista tenga longitud impar?
En el caso cuando la lista es impar el elemento de enmedio se toma que esta relacionado con la lista vacia 
lo que hace que el ultimo elemento de la lista resultante sea la lista que solo contiene a ese elemento.

a) De todas las implementaciones para generar la lista de pares, �Cu�l es el m�s eficiente?

La m�s eficiente es la generada por recursi�n de cola, ya que esta es m�s eficiente que la recursi�n
simple(se explica en el siguiente inciso) y CPS tenemos la materializaci�n de la pila que remplaza la pila
de control actual por otra. En otras palabras, con recursi�n de cola no esta esperando a que se le regrese una
llamada recursiva.

b) Considerando el siguiente enunciado: �Los programas que utilizan recursi�n optimizados
con recursi�n de cola son m�s eficientes que aquellos que usan recursi�n simple�
(Abelson et al., 1996). Explicar y argumentar si se est� de acuerdo o no con el enunciado.

Si, ya que la recursi�n simple implica la creaci�n de un nuevo marco en la pila de llamadas y 
en la recursi�n de cola es posible realizar dichas llamadas recursivas reaprovechando el marco de la pila
anterior.

c)Dar un enunciado similar al anterior que compare las tre t�cnicas: recursi�n simple,
recursi�n de cola y continuation passing style.

La recursi�n de cola es la m�s eficiente, ya que en las otras dos esperas un resultado lo que genera otro registro
de activaci�n.