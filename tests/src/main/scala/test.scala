package optometry

import estrapade._
import contextual.data.fqt._
import contextual.data.scalac._

object Test extends TestApp {
 
  case class Address(lines: List[String], postcode: Option[String], country: String)
  case class Person(name: String, address: Address, age: Int)
  case class Employee(person: Person, role: String)
  case class Company(name: String, employees: List[Employee])
  case class Directory(companies: List[Company])

  def tests() = {

    val directory = test("construct a data value") {
      Directory(List(
        Company("Acme Inc", List(
          Employee(Person("Richard Jones", Address(
            List("648 East Avenue", "Newtown"), Some("84792"), "USA"
          ), 29), "CFO"),
          Employee(Person("John Smith", Address(
            List("1 High Street", "Townville"), None, "USA"
          ), 54), "CEO")
        )),
        Company("International Widgets Corp", List(
          Employee(Person("Michael O'Sullivan", Address(
            List("41 South Road", "Dublin"), None, "Ireland"
          ), 27), "Managing Director"),
          Employee(Person("David Jackson", Address(
            List("388 Bellevue Street", "Cork"), None, "Ireland"
          ), 44), "Technical Director"),
          Employee(Person("James Murphy", Address(
            List("7 North Street", "Dublin"), None, "Ireland"
          ), 48), "Human Resources Director")
        ))
      ))
    }.returns()

    val employee = test("extract a single employe") {
      directory().companies.head.employees.head
    }.returns()

    val person = test("extract a single person") {
      employee().person
    }.returns()

    val company = test("extract a single company") {
      directory().companies.head
    }.returns()

    test("construct simple lens") {
      scalac"Lens[Person](_.name)"
    }.assert(_ == Returns(fqt"optometry.Lens[optometry.Test.Person,String,String]"))

    test("apply a simple lens") {
      val nameLens = Lens[Person](_.name)
      nameLens(person())
    }.assert(_ == "Richard Jones")
    
    test("apply a list lens") {
      val getLines = Lens[Address](_.lines(each))
      getLines(person().address)
    }.assert(_ == List("648 East Avenue", "Newtown"))
    
    test("apply a list lens two levels deep") {
      val getLines = Lens[Person](_.address.lines(each))
      getLines(person())
    }.assert(_ == List("648 East Avenue", "Newtown"))
    
    test("apply a list lens three levels deep") {
      val getLines = Lens[Employee](_.person.address.lines(each))
      getLines(employee())
    }.assert(_ == List("648 East Avenue", "Newtown"))
    
    test("apply a list lens with a profunctor in the middle") {
      val getAges = Lens[Company](_.employees(each).person.age)
      getAges(company())
    }.assert(_ == List(29, 54))
    
    test("combine two profunctors") {
      val getAges = Lens[Directory](_.companies(each).employees(each).person.age)
      getAges(directory())
    }.assert(_ == List(List(29, 54), List(27, 44, 48)))
    
    test("combine two different profunctors") {
      val getPostcodes = Lens[Company](_.employees(each).person.address.postcode(option))
      getPostcodes(company())
    }.assert(_ == List(Some("84792"), None))
    
    test("simple lens update") {
      val postcodeLens = Lens[Address](_.postcode)
      postcodeLens(person().address) = Some("12345")
    }.assert(_ == Address(List("648 East Avenue", "Newtown"), Some("12345"), "USA"))
    
    test("profunctor lens has correct type") {
      scalac"Lens[Address](_.postcode(option))"
    }.assert(_ == Returns(fqt"optometry.Lens[optometry.Test.Address,String,Option[String]]"))
    
    test("simple profunctor lens update") {
      val postcodeLens = Lens[Address](_.postcode(option))
      postcodeLens(person().address) = "12345"
    }.assert(_ == Address(List("648 East Avenue", "Newtown"), Some("12345"), "USA"))
    
    test("simple profunctor lens update") {
      val postcodeLens = Lens[Address](_.postcode(option))
      postcodeLens(person().address) = "12345"
    }.assert(_ == Address(List("648 East Avenue", "Newtown"), Some("12345"), "USA"))
    
  }

}

