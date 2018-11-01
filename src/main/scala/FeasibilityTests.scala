import scala.annotation.tailrec

object FeasibilityTests extends App {

  def myFold[T, Acc](data: List[T])(accZero: Acc)(f: (Acc, T) => Acc): Acc = {
    @tailrec
    def inner(acc: Acc, remaining: List[T]): Acc = {
      if (remaining.isEmpty)
        acc
      else {
        val newAcc: Acc = f(acc, remaining.head)
        inner(newAcc, remaining.tail)
      }
    }
    inner(accZero, data)
  }

  type EitherAcc = Either[String, Unit]

  def myTerminatingFold[T](data: List[T])(f : (EitherAcc, T) => EitherAcc): EitherAcc = {
//    myFold[T, EitherAcc](data)(Right(()))(f)
    @tailrec
    def inner(acc: EitherAcc, remaining: List[T]): EitherAcc = {

    }

    inner(Right(()), data)
  }

  val inList1 = List[Int](1, 2, -3, 4)
  val inList2 = List[Int](1, 2, 2, 4)

  def isPos(ea: EitherAcc, t: Int): EitherAcc =
    ea match {
      case Left(errMsg) => ea
      case Right(_) => if (t > 0) ea
        else Left(s"$t is not positive")
    }

  val result: EitherAcc = myTerminatingFold(inList1)(isPos)

  val combinedResult: EitherAcc =
    for {
      ea1 <- myTerminatingFold(inList1)(isPos)
      _ = println("combinedResult")
      ea2 <- myTerminatingFold(inList2)(isPos)
    } yield ea2
  
  type Answer = Double

  def addHundredth(acc: Double, it: Int): Double = acc + it / 100.0

  def mySumSmall(myInts: List[Int]): Double =
    myFold(myInts)(0.0){ (acc, it ) =>
      acc + it / 100.0
    }

  def mySumSmall2(myInts: List[Int]): Double =
    myFold(myInts)(0.0)(addHundredth)
}
