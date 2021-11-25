
import scala.annotation.tailrec
object Sequence {

  /* a) Найдите последний элемент Seq.*/
  def testLastElement[A](seq:Seq[A]):Option[A]={
    @tailrec
    def loop(myseq:Seq[A]):Option[A]={
      myseq match{
        case Nil => None
        case Seq(a)=>Some(a)
        case _::tail=>loop(tail)
      }
    }
    loop(seq)
  }

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.*/

  def testZip2[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop(a1: Seq[A], b1: Seq[A],res:Seq[(A, A)]):Seq[(A, A)]={
      a1 match {
        case head1::Nil => b1 match {
          case Nil=>res
          case head2::_=>res:+(head1,head2)
        }
        case head1::tail1=>b1 match {
          case Nil=>res
          case head2::tail2=>loop(tail1,tail2,res:+(head1,head2))
        }
        case Nil =>res
      }
    }
    loop(a,b,Nil)
  }

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.*/
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop(myseq:Seq[A],res:Boolean):Boolean={
      myseq match {
        case head::tail=>loop(tail,res && cond(head))
        case Nil=>res
      }
    }
    loop(seq,true)
  }

  /* d) Проверьте, является ли Seq палиндромом*/
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop(myseq:Seq[A],numb:Int):Int={
      myseq.length match {
        case 0 => numb
        case 1 => numb
        case _ => if (myseq.head==myseq.last) loop(myseq.tail.init,numb+1) else numb
      }
    }
    return loop(seq,0)==seq.length/2
  }

  /* e) Реализуйте flatMap используя foldLeft.*/
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((x,y)=>x++:f(y))

  var seq=Seq(9,8,7,6,5,6,7,8,9)
  var seq2=Seq(9,8,7,6,5,4,3)
  def func(num:Int):Boolean=num>3
  def func2(x:Int):Seq[Double]=Seq(x,x+x)

  def main(args: Array[String]) = {
    print("a)  ")
    println(testLastElement(seq))
    print("b)  ")
    println(testZip2(seq,seq2))
    print("c)  ")
    println(testForAll(seq)(func))
    print("d)  ")
    println(testPalindrom(seq))
    print("e)  ")
    println(testFlatMap(seq)(func2))
  }
}
