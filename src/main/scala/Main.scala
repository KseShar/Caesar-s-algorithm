object Main {
  def main(args: Array[String]): Unit = {
    val xxx = new CryptoLogger()
    println(xxx.algCaesarEncrypt("az/AZ.ая/АЯ"))
    println(xxx.algCaesarDecrypt("dc/DC.гв/ГВ"))
    val xxx1 = new CryptoLogger(-3)
    println(xxx1.algCaesarEncrypt("az/AZ.ая/АЯ"))
    println(xxx1.algCaesarDecrypt("xw/XW.эь/ЭЬ"))
  }
}
class CryptoLogger(key: Int = 3){
  private val alphabetLat = "abcdefghijklmnopqrstuvwxyz"
  private val alphabetCyr = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"

  def algCaesarEncrypt (message: String): String ={
      message.map{
        case x if alphabetLat.contains(x.toLower) => charEncrypt(x, alphabetLat)
        case x if alphabetCyr.contains(x.toLower) => charEncrypt(x, alphabetCyr)
        case x => x
      }
  }
  def algCaesarDecrypt (message: String): String ={
      message.map{
        case x if alphabetLat.contains(x.toLower) => charDecrypt(x, alphabetLat)
        case x if alphabetCyr.contains(x.toLower) => charDecrypt(x, alphabetCyr)
        case x => x
     }
  }

  private def charEncrypt(c: Char, alphabet: String): Char = {
      val index = (alphabet.indexOf(c.toLower) + key) % alphabet.length
      val res = if (index < 0) alphabet.charAt(alphabet.length + index)
      else alphabet.charAt(index)

      if (c.isUpper) res.toUpper
      else res
  }
  private def charDecrypt(c: Char, alphabet: String): Char = {
      val index = (alphabet.indexOf(c.toLower) - key + alphabet.length) % alphabet.length
      val res = if (index < 0) alphabet.charAt(alphabet.length + index)
      else alphabet.charAt(index)

      if (c.isUpper) res.toUpper
      else res
  }
}