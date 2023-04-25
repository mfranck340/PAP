import scala.annotation.tailrec

class Tablero() {
  private val rand = new scala.util.Random(System.currentTimeMillis())

  //Función para crear un tablero (lista con tantas posiciones como indique celdas) con todas las fichas de aire
  def inicializarTablero(celdas:Int): List[Int] = {
    celdas match {
      case 0 => Nil                                                                       //En el caso base, devolvemos la lista vacía
      case _ => 0 :: inicializarTablero(celdas - 1)                                       //Realizamos la recursividad concatenando un 0 al inicio
    }
  }

  //Función para actualizar el tablero si se han eliminado fichas
  def actualizarTablero(tablero: List[Int], col: Int, dif: Int): List[Int] = {
    actualizarTableroAux(tablero, col, dif, comprobarTablero(tablero))                    //Llamamos a actualizarTableroAux() comprobando si se han eliminado fichas
  }

  //Función recursiva para actualizar el tablero si se han eliminado fichas
  @tailrec
  private def actualizarTableroAux(tablero: List[Int], col: Int, dif: Int, continuar: Boolean): List[Int] = {
    if (continuar) {                                                                      //Si hay bloques de aire, bajamos las fichas y generamos nuevas en lo alto
      mostrarTablero(tablero, col)                                                        //Mostramos el tablero
      val tabAux = generarFichas(bajarFichas(tablero, col, lengthCustom(tablero) - 1), dif, col)
      actualizarTableroAux(tabAux, col, dif, comprobarTablero(tabAux))                    //Hacemos recursividad comprobando si quedan bloques de aire en el nuevo tablero
    } else {
      tablero                                                                             //Si no hay bloques de aire, devolvemos el mismo tablero
    }
  }

  //Función que realiza cambios en el tablero dependiendo de las coordenadas introducidas
  def interactuarConTablero(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val (tabAux, vidas) = realizarMovimiento(tablero, cordX, cordY, col, dif)             //Realizamos el movimiento con las coordenadas
    (actualizarTablero(tabAux, col, dif), vidas)                                          //Devolvemos el tablero actualizado junto con las vidas que tiene el usuario tras realizar el movimiento
  }

  //Función que realiza el movimiento adecuado dependiendo del elemento que ha sido pulsado
  private def realizarMovimiento(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val elem = getElem(cordY * col + cordX, tablero)                                      //Obtenemos el elemento que se encuentra en la posición del tablero que se ha pulsado
    elem match {
      case _ if elem < 7 =>                                                               //Si el elemento es menor que 7, es una ficha normal
        sustituirFicha(eliminarFichas(tablero, cordY * col + cordX, col, lengthCustom(tablero) / col, elem), cordY * col + cordX, elem, dif)
      case 8 =>                                                                           //Si el elemento es 8, hacemos explotar la bomba
        (activarBomba(tablero, cordY * col + cordX, col, rand.nextInt(2)), false)
      case 9 =>                                                                           //Si el elemento es 9, hacemos estallar la TNT
        (activarTnt(tablero, cordY * col + cordX, col), false)
      case _ =>                                                                           //En cualquier otro caso, activamos el rompecabezas e insertamos un bloque de aire en su posición para no generar ningún bloque especial
        (insertar(0, cordY * col + cordX, activarRompe(tablero, elem % 10)), false)
    }
  }

  //Función para activar el rompecabezas
  private def activarRompe(tablero:List[Int], elem:Int): List[Int] = {
    activarRompeAux(tablero, elem, lengthCustom(tablero) - 1)                             //Llamamos a activarRompeAux() con la última posición del tablero
  }

  //Función recursiva para activar el rompecabezas (recorre todas las posiciones del tablero)
  @tailrec
  private def activarRompeAux(tablero:List[Int], elem:Int, pos:Int): List[Int] = {
    pos match {
      case -1 => tablero                                                                  //Si hemos recorrido el tablero entero, devolvemos el tablero actual
      case _ =>
        if (getElem(pos, tablero) == elem)                                                //Si el elemento en la posición actual coincide con el que buscamos, hacemos la llamada recursiva poniendo un bloque de aire
          activarRompeAux(insertar(0, pos, tablero), elem, pos - 1)
        else
          activarRompeAux(tablero, elem, pos - 1)                                         //Si no coincide, simplemente realizamos la llamada recursiva
    }
  }

  //Función para hacer estallar la TNT
  private def activarTnt(tablero:List[Int], posObj:Int, col:Int): List[Int] = {
    activarTntAux(tablero, posObj, col, lengthCustom(tablero) - 1)
  }

  //Función recursiva para hacer estallar la TNT (recorre todas las posiciones del tablero)
  @tailrec
  private def activarTntAux(tablero:List[Int], posObj:Int, col:Int, pos:Int): List[Int] = {
    pos match {
      case -1 => tablero                                                                  //Si hemos recorrido el tablero entero, devolvemos el tablero actual
      case _ =>
        if (math.sqrt(math.pow(posObj % col - pos % col, 2) + math.pow(posObj / col - pos / col, 2)) <= 4)
          activarTntAux(insertar(0, pos, tablero), posObj, col, pos - 1)            //Si la distancia entre la posición actual y la que ha tocado el usuario es de 4 o menor, hacemos la recursividad colocando un bloque de aire
        else
          activarTntAux(tablero, posObj, col, pos - 1)                                    //Si la distancia es mayor que 4, simplemente realizamos la llamada recursiva
    }
  }

  //Función para activar la bomba
  private def activarBomba(tablero:List[Int], pos:Int, col:Int, opcion:Int): List[Int] = {
    opcion match {
      case 0 => eliminarFila(tablero, (pos/col) * col, col)                               //Si la opción es 0, borramos la fila
      case _ => eliminarColumna(tablero, pos % col, col, lengthCustom(tablero) / col)     //Si no, borramos la columna
    }
  }

  //Función recursiva para eliminar la fila (al activar la bomba)
  @tailrec
  private def eliminarFila(tablero:List[Int], pos:Int, col:Int): List[Int] = {
    if (col == 0)
      tablero                                                                             //Si hemos recorrido todas las columnas, devolvemos el tablero
    else
      eliminarFila(insertar(0, pos, tablero), pos + 1, col - 1)                     //Realizamos la llamada recursiva colocando un bloque de aire en la posición indicada
  }

  //Función recursiva para eliminar la columna (al activar la bomba)
  @tailrec
  private def eliminarColumna(tablero:List[Int], pos:Int, col:Int, fil:Int): List[Int] = {
    if (fil == 0)
      tablero                                                                             //Si hemos recorrido todas las filas, devolvemos el tablero
    else
      eliminarColumna(insertar(0, pos, tablero), pos + col, col, fil - 1)           //Realizamos la llamada recursiva colocando un bloque de aire en la posición indicada
  }

  //Función que coloca los bloques especiales según el número de fichas que han sido borradas (o el elemento que se ha eliminado si solo se ha borrado 1)
  private def sustituirFicha(tablero: List[Int], pos: Int, elem: Int, dif: Int): (List[Int], Boolean) = {
    val fichas = contarFichasEliminadas(tablero)                                          //Contamos el número de fichas que se han eliminado
    fichas match {
      case 1 => (insertar(elem, pos, tablero), true)                                      //Si se ha eliminado una única ficha, volvemos a colocar el elemento y enviamos true para indicar que se debe restar una vida
      case 5 => (insertar(8, pos, tablero), false)                                  //Si se han eliminado 5 fichas, colocamos una bomba
      case 6 => (insertar(9, pos, tablero), false)                                  //Si se han eliminado 6 fichas, colocamos una TNT
      case _ =>                                                                           //Si se han eliminado 7 o más fichas, colocamos un rompecabezas
        if (fichas >= 7) (insertar(10 + rand.between(1, dif + 1), pos, tablero), false)
        else (tablero, false)                                                             //En cualquier otro caso, devolvemos el mismo tablero
    }
  }

  //Función recursiva que elimina las fichas comprobando si las fichas contiguas son iguales a la que se ha tocado
  private def eliminarFichas(tablero: List[Int], posIni: Int, col: Int, fil: Int, elem: Int): List[Int] = {
    //Función que llamamos para eliminar el elemento de la posición actual una vez que hemos comprobado todas las direcciones
    def marcarCasilla(): List[Int] = {
      insertar(0, posIni, tablero)                                                  //Colocamos un bloque de aire en la posición actual del tablero
    }

    //Función que comprueba si hay una ficha que coincide con la pulsada a la derecha
    def moverDerecha(): List[Int] = {
      if ((posIni + 1) % col != 0 && getElem(posIni + 1, tablero) == elem)                //Si el elemento a la derecha coincide con el que ha pulsado el usuario, marcamos la casilla y hacemos la llamada recursiva con la posición de la derecha
        eliminarFichas(marcarCasilla(), posIni + 1, col, fil, elem)
      else marcarCasilla()                                                                //Si no, simplemente marcamos la casilla
    }

    //Función que comprueba si hay una ficha que coincide con la pulsada a la izquierda
    def moverIzquierda(): List[Int] = {
      if (posIni % col != 0 && getElem(posIni - 1, tablero) == elem)                      //Si el elemento a la izquierda coincide con el que ha pulsado el usuario, hacemos la llamada recursiva con el tablero actualizado tras comprobar la posición a la derecha y con la posición de la izquierda
        eliminarFichas(moverDerecha(), posIni - 1, col, fil, elem)
      else moverDerecha()                                                                 //Si no, simplemente comprobamos la posición de la derecha
    }

    //Función que comprueba si hay una ficha que coincide con la pulsada arriba
    def moverArriba(): List[Int] = {
      if (posIni >= col && getElem(posIni - col, tablero) == elem)                        //Si el elemento de arriba coincide con el que ha pulsado el usuario, hacemos la llamada recursiva con el tablero actualizado tras comprobar la posición a la izquierda y con la posición de arriba
        eliminarFichas(moverIzquierda(), posIni - col, col, fil, elem)
      else moverIzquierda()                                                               //Si no, simplemente comprobamos la posición de la izquierda
    }

    //Comprueba si hay una ficha que coincide con la pulsada abajo
    if (posIni < col * (fil - 1) && getElem(posIni + col, tablero) == elem)               //Si el elemento de abajo coincide con el que ha pulsado el usuario, hacemos la llamada recursiva con el tablero actualizado tras comprobar la posición de arriba y con la posición de abajo
      eliminarFichas(moverArriba(), posIni + col, col, fil, elem)
    else moverArriba()                                                                    //Si no, simplemente comprobamos la posición de arriba
  }

  //Función recursiva para mostrar el tablero por pantalla
  def mostrarTablero(x:List[Int], n:Int): Unit = {
    x match {
      case Nil =>  print("\n" + "---" * (n - 1) + "----")
      case _ =>
        if (lengthCustom(x) % n == 0) {
          print("\n" + "---" * (n - 1) + "----" + "\n|")
        }
        x.head match {
          case 8 => print("B |")
          case 9 => print("T |")
          case _ =>
            if (x.head < 7) print(x.head + " |")
            else print(s"R${x.head % 10}|")
        }
        mostrarTablero(x.tail, n)
    }
  }

  //Función recursiva para obtener el número de elementos de una lista
  private def lengthCustom(x:List[Int]): Int = {
    x match {
      case Nil => 0                                                                       //Si la lista no tiene elementos, devolvemos 0
      case _ => 1 + lengthCustom(x.tail)                                                  //Sumamos 1 y hacemos la llamada recursiva quitando el primer elemento de la lista
    }
  }

  //Función recursiva que genera fichas aleatorias en las posiciones de aire que se encuentran en lo alto del tablero
  private def generarFichas(tablero:List[Int], dif:Int, col:Int): List[Int] = {
    col match {
      case 0 => tablero                                                                   //Una vez que hemos recorrido la primera fila entera (recorriendo el número de columnas), devolvemos el tablero actual
      case _ =>
        if (tablero.head == 0)                                                            //Si el elemento es un bloque de aire, generamos un número aleatorio que concatenamos al resultado obtenido tras realizar la llamada recursiva con el resto del tablero
          rand.between(1, dif + 1) :: generarFichas(tablero.tail, dif, col - 1)
        else                                                                              //Si no, simplemente hacemos la llamada recursiva con el resto del tablero y concatenamos el mismo elemento que había en la posición actual
          tablero.head :: generarFichas(tablero.tail, dif, col - 1)
    }
  }

  //Función recursiva para sustituir un elemento en la posición indicada de una lista por otro
  private def insertar(elem: Int, pos: Int, lista: List[Int]): List[Int] = {
    lista match {
      case Nil => elem :: Nil                                                             //Si la lista está vacía, simplemente añadimos el elemento
      case _ =>
        pos match {
          case 0 => elem :: lista.tail                                                    //Si la posición es 0, devolvemos el elemento concatenado a la cola de la lista
          case _ => lista.head :: insertar(elem, pos - 1, lista.tail)                     //Si no,hacemos la llamada recursiva restando 1 a la posición y con la cola de la lista
        }
    }
  }

  //Funcion recursiva para obtener un elemento situado en una posición específica de una lista
  @tailrec
  private def getElem(index: Int, matriz: List[Int]): Int = {
    index match {
      case 0 => matriz.head                                                               //Si la posición es 0, devolvemos el elemento en la cabeza
      case _ => getElem(index - 1, matriz.tail)                                           //Si no, hacemos la llamada recursiva restando 1 a la posición y con la cola de la matriz
    }
  }

  //Función recursiva para bajar las fichas del tablero cuando tienen bloques de aire en las posiciones inferiores
  @tailrec
  private def bajarFichas(tablero:List[Int], col:Int, pos:Int): List[Int] = {
    if (pos == col - 1)
      tablero                                                                             //Si hemos recorrido el tablero entero a excepción de la primera fila, devolvemos el tablero actual
    else {
      if (getElem(pos, tablero) == 0 && getElem(pos - col, tablero) != 0)                 //Si el elemento en la posición es un bloque de aire y encima de él hay una ficha, hacemos el intercambio de la ficha por el bloque de aire y hacemos la llamada recursiva con el nuevo tablero y la siguiente posición
        bajarFichas(insertar(0, pos - col, insertar(getElem(pos - col, tablero), pos, tablero)), col, pos - 1)
      else                                                                                //Si no, simplemente hacemos la llamada recursiva con la siguiente posición del tablero
        bajarFichas(tablero, col, pos - 1)
    }
  }

  //Función recursiva que comprueba si hay bloques de aire en el tablero
  @tailrec
  private def comprobarTablero(tablero:List[Int]): Boolean = {
    tablero match {
      case Nil => false                                                                   //Si hemos recorrido el tablero entero y no hemos encontrado ningún bloque de aire, devolvemos false
      case _ =>
        if (tablero.head == 0) true                                                       //Si la posición actual tiene un bloque de aire, devolvemos true
        else comprobarTablero(tablero.tail)                                               //Si no, hacemos la llamada recursiva con el resto del tablero
    }
  }

  //Función recursiva que devuelve el número de bloques de aire que hay en el tablero
  private def contarFichasEliminadas(tablero: List[Int]): Int = {
    tablero match {
      case Nil => 0                                                                       //Si hemos recorrido el tablero entero, devolvemos 0
      case _ =>
        if (tablero.head == 0) 1 + contarFichasEliminadas(tablero.tail)                   //Si en la posición actual hay un bloque de aire, sumamos 1 y hacemos la llamada recursiva con el resto del tablero
        else contarFichasEliminadas(tablero.tail)                                         //Si no, simplemente hacemos la llamada recursiva con el resto del tablero
    }
  }
}