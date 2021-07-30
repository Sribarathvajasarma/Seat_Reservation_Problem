import scala.util.control.Breaks._

object Harvest_problem {

  var valarr = new Array[Int](50);

  def main(args: Array[String]): Unit = {
    print("Enter the month of harvest(eg: january): ");
    var month = scala.io.StdIn.readLine();
    print("Enter the start day of the month(eg: monday):")
    var day_name = scala.io.StdIn.readLine();
    var days = month_days_setter(month);

    val arr = Array.ofDim[Int](6, 7);
    for (i <- 0 to 5) {
      for (j <- 0 to 6) {
        {
          arr(i)(j) = -1;
        }

      }
    }
    var a = week_no_setter(day_name);
    arr_setter(arr, a, days)
    var weekarr = Array("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday");
    println();
    print("     ");
    for (i <- 0 to 6) {
      print(weekarr(i) + "   ")
    }
    println();

    printarr(arr);
    find(arr, days,a);


  }

  def week_no_setter(day_name: String): Int = {
    day_name match {
      case "monday" => return 0;
      case "tuesday" => return 1;
      case "wednesday" => return 2;
      case "thursday" => return 3;
      case "friday" => return 4;
      case "saturday" => return 5;
      case "sunday" => return 6;
      case _ => println("Invalid");
        return 0;

    }
  }

  def month_days_setter(month: String): Int = {
    month match {
      case "january" => return 31;
      case "february" => return 28;
      case "march" => return 31;
      case "april" => return 30;
      case "may" => return 31;
      case "june" => return 30;
      case "july" => return 31;
      case "augest" => return 31;
      case "september" => return 30;
      case "october" => return 31;
      case "november" => return 30;
      case "december" => return 31;
      case _ => println("not valid");
        return 0;
    }
  }



  def arr_setter(arr: Array[Array[Int]], a: Int, days: Int): Unit = {

    for (i <- 0 to (days - 1)) {
      print("Enter the harvest value of day " + (i + 1) + " : ");
      valarr(i) = scala.io.StdIn.readInt();

    }

    var q = 0;
    var j = 0;
    for (j <- a to 6) {
      arr(0)(j) = valarr(q);
      q = q + 1;
    }
    breakable {

      for (i <- 1 to 5) {
        for (j <- 0 to 6) {
          arr(i)(j) = valarr(q)
          q = q + 1;
          if (q == days) {
            break;
          }
        }

      }
    }
  }


  def printarr(arr: Array[Array[Int]]): Unit = {
    var t = 0
    for (i <- 0 to 5) {

      for (j <- 0 to 6) {
        if (arr(i)(j) == -1) {
          print("          ");
        }
        else {
          t = arr(i)(j);
          print(f"$t%10d")

        }


      }
      print("\n");


    }
  }

  def find_median(days:Int): Double =
  {
    var temp=0;
    var center=0;
    var median=0.0;
    for (i <- 0 to days - 1) {
      for (j <- 0 to days - i - 1) {
        if (valarr(j) > valarr(j + 1)) {
          temp = valarr(j);
          valarr(j) = valarr(j + 1);
          valarr(j + 1) = temp;
        }
      }
    }


    if (days % 2 == 0) {
      center = days / 2;

      median = (valarr(center).toFloat + valarr(center + 1).toFloat) / 2;

    }
    else {
      center = (days / 2) + 1;
      median = valarr(center);


    }
    return median;
  }

  def find(arr: Array[Array[Int]], days: Int, a:Int): Unit = {
    var sum = 0;
    var n = 0;
    var max = 0;
    var min = 10000;
    var max_date = 0;
    var min_date = 0;
    var max_day = 0;
    var min_day = 0;
    var average = 0.000;
    var p = 0;
    var q = 0;
    var w = 0;
    var r = 0;
    var median = 0.0;

    for (i <- 0 to 5) {

      for (j <- 0 to 6) {
        if (arr(i)(j) != -1) {
          sum = sum + arr(i)(j);
          n = n + 1;
          if (arr(i)(j) > max) {
            max = arr(i)(j);
            max_date = n;
            max_day = j;
          }
          if (arr(i)(j) < min) {
            min = arr(i)(j);
            min_date = n;
            min_day = j;
          }
        }
      }
    }


    n = 0;


    var max_date_arr = new Array[Int](31);
    var min_date_arr = new Array[Int](31);
    var max_day_arr = new Array[Int](31);
    var min_day_arr = new Array[Int](31);
    var max_week_arr = new Array[Int](31);
    var min_week_arr = new Array[Int](31);


    for (i <- 0 to 5) {

      for (j <- 0 to 6) {
        if (arr(i)(j) != -1) {
          n = n + 1;
          if (arr(i)(j) == max) {
            max_date_arr(r) = n
            max_day_arr(r) = j;
            if(n<=(7-a))
              {
                max_week_arr(r)=n;
              }
            else
              {
                if((n-(7-a))%7==0)
                {
                  max_week_arr(r)= ((n-(7-a))/7)+1;
                }
                else{
                  max_week_arr(r)= ((n-(7-a))/7)+2;
                }
              }

            r = r + 1;

          }
          if (arr(i)(j) == min) {
            min_date_arr(w) = n;
            min_day_arr(w) = j;
            if(n<=(7-a))
              {
                min_week_arr(w)=n;
              }
            else {
              if ((n-(7-a))%7==0) {
                min_week_arr(w) = ((n-(7-a))/7)+1;
              }
              else {
                min_week_arr(w) = ((n-(7-a))/7)+2;
              }
            }
            w = w + 1;
          }
        }
      }

    }

     median=find_median(days);;
      average = sum.toFloat / days.toFloat;

      var weekarr = Array("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday");

      println("Maximum harvest value:" + max);
      print("Maximum harvest obtained date/dates: ");
      for (i <- 0 until r) {

        print(max_date_arr(i) + ", ")
      }
      print("\n")

      print("Maximum harvest obtained day/days: ");
      for (i <- 0 until r) {

        print(weekarr(max_day_arr(i)) + ", ");
      }
      print("\n")

      print("Maximum harvest obtained week/weeks: ");
      for (i <- 0 until r) {

        print(max_week_arr(i) + ", ");
      }

      print("\n")
      println("Minimum harvest value: " + min);
      print("Mimimum harvest obtained date/dates: ");
      for (i <- 0 until w) {

        print(min_date_arr(i) + ", ")
      }

      print("\n")
      print("Minimum harvest obatained day/days: ")
      for (i <- 0 until w) {

        print(weekarr(min_day_arr(i)) + ", ");
      }
      print("\n")


      print("Minimum harvest obatained week/weeks: ")
      for (i <- 0 until w) {

        print(min_week_arr(i)+ ", ");
      }

      print("\n")
      println("Difference between maximum and minimum: "+(max-min));

      println(f"Average: $average%2.2f");
      println("Median :" + median);


    }

  }

