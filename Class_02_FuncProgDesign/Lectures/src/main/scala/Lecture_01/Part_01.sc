case class Book(title: String, authors: List[String])

val books = Set(
  Book(title = "Moby Dick", authors = List("Melville, Herman")),
  Book(title = "Harry Potter", authors = List("Rowling, J.K.")),
  Book(title = "How to Write a Book in a Group", authors = List("Person, One", "Person, Two")),
  Book(title = "A Second Book", authors = List("Person, One", "Person, Three"))
)

// find all books whose author's last name is "Melville"
for (b <- books; a <- b.authors if a startsWith "Melville,") yield b.title

// find all book which have the word "Book" in the title
for (b <- books if (b.title indexOf "Book") >= 0) yield b.title

// authors who have written at least two books
for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1