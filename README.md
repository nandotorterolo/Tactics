# Tactics

Tactics se juega sobre un tablero de 8x8, casillas cuadradas, con dos dados de 6 caras y 30 piezas, 15 de color claro (“blancas”), y 15 de color oscuro (“negras”)
El objetivo del juego es dar captura al rey enemigo. A tal fin, los jugadores por turno lanzan los dados y de acuerdo a sus resultados
mueven una o más piezas de su color.

Las piezas de cada bando representan un ejército renacentista fantástico, disponiendo cada jugador, al inicio del juego, de las siguientes quince piezas:

- Dos unidades de escaramuzadores (HACHA)
- Dos unidades de rodeleros (ESPADA)
- Dos unidades de caballería (CABALLO)
- Dos unidades de piqueros (PICAS)
- Dos unidades de arqueros (ARCO)
- Dos unidades de arcabuceros (BUFO)
- Dos unidades de magos (MAGO)
- Una unidad, de color diferente para ser identificable en todo momento, representa al Rey y su escolta, la “guardia real”. En el caso de las blancas, es celeste claro, y en el de las negras, es verde oscuro.

![alt text](https://github.com/nandotorterolo/Tactics/blob/master/src/Referencias.png "Referencias")

Cada unidad posee características que le permiten capturar a ciertas unidades enemigas, asimismo, cada pieza tiene debilidad contra ciertas otras unidades enemigas.

Los jugadores colocan sus piezas de la manera que mejor consideren en las tres filas horizontales más cercanas a su persona. No pueden colocarse más de una pieza por casillero, y no pueden colocarse piezas contiguas a la línea central
La guardia real va en una de las cuatro casillas del medio marcadas con un cuadrado.

![alt text](https://github.com/nandotorterolo/Tactics/blob/master/src/ReyesIniciales.png "ReyesIniciales")

El bando atacante es el que realiza el primer movimiento. Los jugadores arrojan un dado cada uno, y el que obtenga el número más alto elige si va a atacar o a defender.


## Movimiento:
En su turno el jugador lanza los dados. La suma de puntos en los dados le indica cuántos puntos dispone para hacer su movimiento. Puede mover una o más piezas, hasta el límite de puntos disponibles.
Mover una pieza ortogonalmente una casilla (hacia arriba, abajo, o en la misma línea a los costados) cuesta 2 puntos.
Mover una pieza diagonalmente (adelante-izquierda, ad-der, atr-izq, atr-der) cuesta 3 puntos.
Una pieza sólo puede moverse a un casillero vacío, u ocupado por una de las piezas del rival a las que es capaz de capturar. En ese caso, la pieza del contrincante es retirada del tablero (ha sido capturada/eliminada/ventilada etc.)

Cada pieza sólo puede moverse una vez en cada turno. Asimismo, en cada movimiento solo puede desplazarse una casilla, con la siguiente excepción.

## Carga:
Si el jugador obtiene puntuaciones iguales en ambos dados, el número de puntos de movimiento disponible ese turno se duplica, y tiene la opción de mandar a una o más de sus unidades a cargar. La posibilidad realizar una carga significa que puede mover sus piezas no una sino dos casillas en línea recta a partir de su casilla de origen. El costo de movimiento de cada casilla sigue siendo el mismo; es decir, en la eventualidad de una carga, mover una pieza una casilla hacia delante cuesta 2 puntos, mientras que mover esa pieza cargando dos casillas hacia delante cuesta 2+2 = 4 puntos. (No pueden moverse piezas en líneas quebradas.)

## Carga inicial:
En su primer turno, tanto el jugador atacante como el defensor pueden cargar, independientemente del resultado de los dados. Es decir, hayan obtenido o no dos números iguales, disponen del doble de puntos de movimiento de lo que indican los dados, y pueden mover las piezas hasta un máximo de dos casillas en línea recta.

## Captura:
Cada clase de pieza puede capturar a tres clases de pieza enemiga, y ser capturada por otras tres clases. Piezas de clases iguales no pueden capturarse entre sí.
La excepción a esta regla de captura la constituyen las Guardias Reales de ambos bandos. La Guardia Real puede capturar cualquier pieza enemiga sin importar su clase, sin embargo, la guardia real puede (también) ser capturada por cualquier pieza enemiga.
Las piezas que cada unidad puede capturar y ser capturada por, son las siguientes:
[TABLA/DIAGRAMA]
Al realizar una captura, la pieza finaliza su movimiento.
Fin del juego: El juego termina cuando uno de los jugadores captura la Guardia Real enemiga, declarándose así vencedor.
