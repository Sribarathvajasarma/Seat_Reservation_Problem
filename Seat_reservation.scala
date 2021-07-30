import Array._
import scala.io.StdIn.readLine
import scala.util.control.Breaks._




object Seat_reservation{

  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Char](7, 5);
    for (i <- 0 to 6) {
      for (j <- 0 to 4) {
        arr(i)(j) = ('A'.toInt + j).toChar;
      }
    }

    println("Initial seat arrangements");
    printarr(arr);
    Reserve(arr);
  }

  def Reserve(arr:Array[Array[Char]]): Unit = {
    print("Enter N when you have done")
    while (true) {
      print("enter valid seat no to check(like 1B) or N to end: ");
      var str = readLine()
      if (str(0) == 'N')
        break;
      else if(allOccupied(arr))
      {
        print("\nSorry all seats have been occupied\n");
      }
     else
     {
      check(arr, str)
      printarr(arr)
     }

  }
    println("\nThank you\n");

  }



  def allOccupied(arr:Array[Array[Char]]): Boolean ={
    var count = 0;
    for (i <- 0 to 6) {
      for (j <- 0 to 4) {
        if (arr(i)(j) == 'X') {
          count = count + 1
        }
      }
    }
    if (count == 35) {
      return true;
    }
    else
      {
        return false;
      }
  }


  def check(arr:Array[Array[Char]],str:String): Unit ={
    if (str(0) > '7' || str(0) < '1' || str(1) > 'E' || str(1) < 'A') {
      println("invalid seat request!\n");
    }
    else {
      var row = -1;
      var col = -1;
      var x = str(0) - 48;

      for (i <- 0 to 6) {
        if (i + 1 == x)
          row = i;
      }

      for (j <- 0 to 4) {
        if (arr(row)(j) == str(1))
          col = j
      }


      if (col == -1) {
        print("Seat is already occupied\n\n");

      }
      else {
        arr(row)(col) = 'X';
        println("Seat reserved successfully\n");
      }


    }

  }
  def printarr(arr:Array[Array[Char]]): Unit ={
    var i=0;
    var j=0;
    for (i <- 0 to 6) {
      print(i + 1 + " ")
      for (j <- 0 to 4) {
        print(arr(i)(j));
        print(" ")
      }
      print("\n")
    }
  }


}





