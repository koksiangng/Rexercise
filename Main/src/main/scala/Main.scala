
import java.io.File
import scala.io.StdIn.readLine
import scala.util.Try

//The default path for testing src/main/scala/Files
//=> = Expression evaluated when parameter is accessed.
object Main extends App{

  val currentPath : String = args(0)

  //Get the files and get the filenames
  val files = Program.getFiles(currentPath).toOption.get
  val filenames = Program.getFilenames(currentPath).toOption.get

  println(files.length + " files read in the path " + currentPath)
  println()

  Program.iterate(filenames, Program.createData(files))
}

object Program{

  //Calculate score
  def calculateScore(hits : Array[Int], data : Array[Array[String]]) : Array[Double] = {
    val scores = new Array[Double](hits.length)

    //Basically percentage scoring.
    for(i <- hits.indices){
      scores(i) = hits(i).toDouble/data(i).length
    }
    scores
  }

  //Creates 2D array with data
  def createData(files : Array[File]) : Array[Array[String]] = {
    val data = Array.ofDim[String](files.length, 1)
    for (i <- files.indices){
      data(i) = Program.extract(files(i).toString)
    }
    data
  }

  //Returns txt files in the folder
  def getFiles(path : String) : Either[Unit, Array[File]] = {
    Try(new java.io.File(path).listFiles.filter(_.getName.endsWith(".txt"))).fold(
        l => Left(println("Current path not working, can't find files. Error code: " + l)),
        r => Right(r))
  }

  //Returns filenames
  def getFilenames(path : String) : Either[Unit, Array[String]] = {
    Try(new File(path).list).fold(
      l => Left(println("Current path not working, can't find filenames. Error code: " + l)),
      r => Right(r)
    )
  }

  //Extract words
  def extract(file : String) : Array[String] = {
    //Create empty array
    var c : Array[String] = Array()

    //Filter away whitespaces and items not a-z
    for(s <- scala.io.Source.fromFile(file).getLines.toArray){
      c = c ++ s.toLowerCase.split("[^a-z]| +").filter(_.nonEmpty)
    }

    //Return with distinct values in array
    c.distinct
  }

  //Keeps iterating the search
  def iterate(filenames : Array[String], data : Array[Array[String]]) : Unit = {
    val points = new Array[Int](data.length)
    print("search> ")
    val words = readLine().toLowerCase()

    //If quit, the system exits
    if(words == ":quit"){
      System.exit(0)
    }

    val wordsArr = words.split(" +").distinct

    //Check input to words data (Contains is basically the search)
    for(w <- wordsArr; i <- data.indices){
      if (data(i).contains(w)){
        points(i) += 1
      }
    }

    val scores = calculateScore(points, data)

    //Sorts highest score first.
    val scorebyfile = (scores zip filenames).sortBy(_._1)(Ordering[Double].reverse)

    //Print top 10 scores
    for(i <- 0 to 10){
      if(i < scorebyfile.length){
        println(scorebyfile(i)._2 + ": " + scorebyfile(i)._1 * 100 + " %")
      }
    }

    iterate(filenames, data)
  }

  //Print all values in an array
  def printall(list : Array[_]): Unit ={
    print("[")
    for (i <- list){
      print(s"$i, ")
    }
    print("]\n")
  }

}