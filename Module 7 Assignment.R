# Determining OO systems
class(myGeneric)
isS4(myGeneric)
isS3stdGeneric(myGeneric)


# Determining base types of objects
x <- 21L
typeof(x)
mode(x)
class(x)
is.integer(x)

y <- list(a = 2, b = 1)
typeof(y)
mode(y)
class(y)
is.list(y)
is.integer(y)


# myGeneric arguments
myGeneric <- function(x) UseMethod("myGeneric")
myGeneric.default <- function(x) cat("Default method")
myGeneric.character <- function(x) cat("Character method")

myGeneric(10)
myGeneric("text")


# S3 classes
vehicle <- function(make, model, year) {
  structure(list(make = make, model = model, year = year), class = "vehicle")
}

print.vehicle <- function(x) {
  cat("Vehicle:", x$make, x$model, "-", x$year)
}

v1 <- vehicle("Volkswagon", "Jetta", 2006)
v1
isS4(vehicle)


book <- function(title, author){
  structure(list(title = title, author = author), class = "book")
}

print.book <- function(x){
  cat("Book:", x$title, "by", x$author)
}

b1 <- book("Now you see it", "Stephen Few")
b1
isS4(book)


# S4 Classes
setClass("Car",
         slots = list(make = "character", model = "character", year = "numeric")
)

setMethod("show", "Car", function(object) {
  cat("Car:", object@make, object@model, "-", object@year, "\n")
})

car1 <- new("Car", make = "Honda", model = "Civic", year = 2020)
car1
isS4(car1)


setClass("Book",
         slots = list(title = "character", author = "character")
)

setMethod("show", "Book", function(object) {
  cat("Title:", object@title, "by", object@author)
})

book1 <- new("Book", title = "Now you see it", author = "Stephen Few")
book1
isS4(book1)
