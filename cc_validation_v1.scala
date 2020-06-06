import scala.io.Source		//Header file to import I/O source 

object CCValidation {		//Creates object CCValidation

  val validNumPrefix = List("4", "5", "37", "6")

  /*Definition of main function to check for the file name "numbers.txt) and 
make it as the source file*/
  
  def main(args: Array[String]) {
    val sourceFile = "numbers.txt"
    for (line <- Source.fromFile(sourceFile).getLines) {
      var cardNo = line;
      var length = cardNo.length();
      if (creditCardFirstNum(cardNo) && validateCardNumber(cardNo.toLong, length)) {
        println("valid");
      } else
        println("invalid");
    }
  }
  
	/*Definition of creditCardFirstNum function to check the starting digit of each line from the input file and return the output based on the above listed prefix*/
  
  def creditCardFirstNum(cardNum: String): Boolean =
    {
      if (validNumPrefix.exists(cardNum => cardNum.startsWith(cardNum)))
        return true;
      else
        return false;
    }

	/*Definition of digit function implementing Step #1 to make the given numbers into a single digit number when the number is greater than 10 */
	
  def digit(value: Int): Int =
    {
      if (value >= 10) {
        var number = value;
        var digiSum = 0;
        while (number > 0) {
          digiSum = digiSum + number % 10;
          number = number / 10;
        }
        return digiSum;
      } else
        return value;
    }
	
	/*Definition of addEvenDoubDigit function implementing Step #2 to sum all the digits in even places*/
	
  def addEvenDoubDigit(cardNum: Long, length: Int): Int =
    {
      var evenNum = 0;
      var temp = 0;
      var check = cardNum;
      check = check / 10;
      while (temp < length) {
        temp = temp + 2;
        evenNum = evenNum + digit(2 * (check % 10).toInt);
        check = check / 100;
      }
      return evenNum;
    }
 
  /*Definition of oddNumDigit function implementing Step #3 to sum all the digits in odd places*/
  
  def oddNumDigit(cardNum: Long, length: Int): Int =
    {
      var oddNum = 0;
      var temp = 0;
      var check = cardNum;
      while (temp < length) {
        temp = temp + 2;
        oddNum = oddNum + (check % 10).toInt;
        check = check / 100;
      }
      return oddNum;
    }
	
	/*Definition of validateCardNumber function implementing step #4 and step #5*/

  def validateCardNumber(cardNum: Long, length: Int): Boolean =
    {
      if (length >= 13 && length <= 16) {
        if (((oddNumDigit(cardNum, length) + addEvenDoubDigit(cardNum, length)) % 10 == 0))
          return true;
        else
          return false;
      } else
        return false;
    }
}