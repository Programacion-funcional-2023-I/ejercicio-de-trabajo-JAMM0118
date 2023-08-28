package introduccion

import javax.naming.OperationNotSupportedException

class EjercicioListas {
  /*
  * Punto 2 repetir lista
  * @param lista Lista a repetir
  * @param n Número de veces a repetir la lista
  * @return Lista repetida n veces
  * @example repetirLista(List(1, 2, 3), 2) = List(List(1,1),List(2,2),List(3,3))
  * @throws IllegalArgumentException si n es negativo
  */
  def repetirListas(lista: List[Int], n: Int): List[List[Int]] = {
    var listaRepetida : List[List[Int]] = List()
    var contador : Int = 0
    var listaAux : List[Int] = List()

    if(n < 0){
      throw new IllegalArgumentException("n debe ser mayor que 0")
    }
    else{
      while( contador < lista.length){
        for(i<- 0 until n){
          listaAux = listaAux :+ lista(contador)
        }
        listaRepetida = listaRepetida :+ listaAux
        listaAux = List.empty
        contador += 1

      }
    }
    return listaRepetida
    
  }
  /*
  * Punto 3: Filtrar listas
  * @param criterioIn Criterio de filtrado que puede ser "mayor", "menor", "mayoroigual", "igual", "diferente" o "menoroigual"
  * @param n Número a comparar
  * @param lista Lista a filtrar
  * @return Lista filtrada de acuerdo al criterio y n
  * @throws IllegalArgumentException si el criterio no es uno de los valores válidos
  */

  def filtrarListas(criterioIn: String, n: Int, lista: List[Int]) : List[Int] = {
    var criterio : String = criterioIn.toLowerCase()
    var listaFiltrada : List[Int] = List()
    var complemento = false

    for(i <- 0 until lista.length ){
      criterio match {
        case "mayor" => complemento = lista(i) > n
        case "menor" => complemento = lista(i) < n
        case "mayoroigual" => complemento = lista(i) >= n
        case "menoroigual" => complemento = lista(i) <= n
        case "igual" => complemento = lista(i) == n
        case "diferente" => complemento = lista(i) != n
        case _ => complemento = false
      }
      if (complemento){
          listaFiltrada = listaFiltrada :+ lista(i)
        }
      }
    
    if(listaFiltrada.isEmpty && complemento == false){
      throw new IllegalArgumentException("El criterio no es uno de los valores válidos")
    }
    else{
      return listaFiltrada
    }
    
  }
}

