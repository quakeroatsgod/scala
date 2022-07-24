import org.jsoup._
object Main{
    def main(args:Array[String]): Unit={
        val doc = Jsoup.connect("http://en.wikipedia.org/").get()
        println(doc)
        
    }
}