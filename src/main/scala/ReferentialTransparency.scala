object ReferentialTransparency extends App {

  case class Point(x: Int, y: Int)

  def calculation1(a: Point, b: Point): Double = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    math.sqrt(dx * dx + dy * dy)
  }
  calculation1(Point(1, 2), Point(3, 4))

  def calculation2(a: Point, b: Point): Double = {
    math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y))
  }
  calculation2(Point(1, 2), Point(3, 4))

  def calculation3(): Double = {
    math.sqrt((1 - 3) * (1 - 3) + (2 - 4) * (2 - 4))
  }
  calculation3()

  def calculation4(): Double = {
    math.sqrt(2 * 2 + 2 * 2)
  }

  def calculation5(): Double = {
    math.sqrt(8)
  }

  def calculation6(): Double = {
    2.8284271247461903
  }

  class IdGenerator {
    private var counter = 0
    def nextId(): Int = {
      counter += 1
      counter
    }
  }

  case class Person(id: Int, name: String, group: Int)

  def generatePerson1(
      name: String,
      idGenerator: IdGenerator
  ) = {
    val id = idGenerator.nextId()
    Person(id, name, id % 2)
  }

  val idGenerator = new IdGenerator
  generatePerson1("Leandro", idGenerator) // == Person(1, "Leandro", 1)
  generatePerson1("Maria", idGenerator) // == Person(2, "Maria", 0)
  generatePerson1("Jose", idGenerator) // == Person(3, "Jose", 1)

  def generatePerson2(
      name: String,
      idGenerator: IdGenerator
  ) = {
    Person(idGenerator.nextId(), name, idGenerator.nextId() % 2)
  }

  val idGenerator2 = new IdGenerator
  generatePerson2("Leandro", idGenerator2) // == Person(1, "Leandro", 0)
  generatePerson2("Maria", idGenerator2) // == Person(3, "Maria", 0)
  generatePerson2("Jose", idGenerator2) // == Person(5, "Jose", 0)

  // -----------------

  abstract class IO[+A] {
    def run: A
  }

  object IO {
    def delay[A](computation: => A): IO[A] = new IO[A] {
      def run: A = computation
    }
  }


  def greet(name: String): Unit =
    println(s"Hello, $name!")

  greet("Marlen")


  def greet2(name: String): IO[Unit] =
    IO.delay(println(s"Hello, $name!"))


  greet2("Marlen")
  greet2("Marlen").run

  // -----------------
  class InMemoryPersonDb(initialPersonById: Map[Int, Person]) {

    private var personById = initialPersonById

    def createPerson(person: Person): Unit =
      personById += (person.id -> person)

    def readPerson(id: Int): Option[Person] =
      personById.get(id)

    def deletePerson(id: Int): Unit =
      personById -= id
  }

  def program1: Option[Person] = {
    val db = new InMemoryPersonDb(Map.empty)
    db.createPerson(Person(2, "Maria", 0))
    db.deletePerson(2)
    db.createPerson(Person(2, "Maria", 0))
    db.readPerson(2)
  }

  def program2: Option[Person] = {
    val db = new InMemoryPersonDb(Map.empty)
    val addPerson = db.createPerson(Person(2, "Maria", 0))

    addPerson
    db.deletePerson(2)
    addPerson
    db.readPerson(2)
  }
}
