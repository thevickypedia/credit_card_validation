import scala.io.Source
object CrediCardValidation {
      
      def checkCardPrefix(cardNum:String) : Boolean =
      {
          if(cardNum.startsWith("4")||cardNum.startsWith("5")||cardNum.startsWith("37")||cardNum.startsWith("6")) //to verify if the card is valid based on the number in which it starts
           return true;
          else
           return false;
      }
      
      def oddDigitSum(cardNum:Long, len:Int) : Int =  //calculations of Odd numbers 
      {
          var oddSum = 0;
          var i = 0;
          var cdn = cardNum;
          while(i<len)
          {
              i += 2;
              oddSum = oddSum + (cdn%10).toInt;
              cdn = cdn/100;
          }
          return oddSum;
      }
      
      def digitOf(value:Int) : Int =
      {
          if(value>=10)   
          {
              var num = value;
              var digiSum = 0;
              while(num>0)
              {
                  digiSum = digiSum + num %10;
                  num  = num/10;
              }
              return digiSum;
          }
          else
            return value;
      }
      def evenDoubleDigitSum(cardNum:Long, len:Int) : Int =   //calculations of even numbers 
      {
          var evenSum = 0;
          var i = 0;
          var cdn = cardNum;
          cdn = cdn/10;
          while(i<len)
          {
              i += 2;
              evenSum = evenSum + digitOf(2*(cdn%10).toInt);
              cdn = cdn/100;
          }
          return evenSum;
      }
      
      def validateCardNumber(cardNum:Long, len:Int) : Boolean =   
      {
          if(len>=13 && len<=16)
          {
            if(((oddDigitSum(cardNum,len) + evenDoubleDigitSum(cardNum,len)) % 10 == 0)) //summing up odd and even numbers and dividing it by 10 to find if it is valid or not
              return true;
            else
              return false;
          }
          else
            return false;
      }

      def main(args: Array[String]) {   //main string
        val filename = "numbers.txt"   //numbers are placed in the bin folder of scala
         for (line <- Source.fromFile(filename).getLines) {
            var CardNum = line;
            var len = CardNum.length();
            if(checkCardPrefix(CardNum) && validateCardNumber(CardNum.toLong,len))

            {
                println("valid");
            }
            else
               println("invalid");
        }
      }
   }