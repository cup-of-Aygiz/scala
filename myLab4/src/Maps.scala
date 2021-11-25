object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`*/
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def srage(num:Seq[Int])=num.sum/num.length

    users.groupBy(_.name).map{
      myKey=>(myKey._1, srage(myKey._2.map(_.age)))
    }
  }


  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?*/
  def testNumberFrodos(map: Map[String, User]): Int = {
    map.count(x=>x._2.name=="Adam")
  }


  /* c) Удалите всех пользователей возраст которых менее 35 лет.*/
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
      map.filter(x=>x._2.age>35)
  }

  def main(args: Array[String]) = {
    print("a)  ")
    println(testGroupUsers(Seq(User("Stas",15),User("Stas",26),User("Oleg",11),User("Oleg",60),User("Aygiz",20))))
    print("b)  ")
    var map=Map("1"->User("Adam",15),"2"->User("Aygiz",22),"3"->User("Adam",36))
    println(testNumberFrodos(map))
    print("c)  ")
    println(testUnderaged(map))
  }
}
