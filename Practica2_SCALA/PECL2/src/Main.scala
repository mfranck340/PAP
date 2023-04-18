object Main {
  def main(args: Array[String]): Unit = {
    val ui = new UI()         //Creamos el objeto ui (interfaz de usuario)
    ui.centerOnScreen()       //Centramos la interfaz en la pantalla
    ui.visible = true         //Hacemos la interfaz visible
    ui.resizable = false      //Hacemos que no pueda redimensionarse la ventana de la interfaz
  }
}
