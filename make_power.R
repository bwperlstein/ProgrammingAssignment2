make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) {
  x*y
}

nothing <- function() {

}