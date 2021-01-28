m <- matrix(1:20, nrow=4)

## apply
apply(m, MARGIN=1, FUN=sum)
apply(m, MARGIN=2, FUN=sum)

sapply(1:nrow(m), FUN=function(i) sum(m[i,]))

dd <- data.frame(bird.mass=runif(20),
                 bird.age=sample(1:4, size=20, replace=TRUE),
                 bird.iq=sample(1:3, size=20, replace=TRUE))

## bird.score = bird.mass + bird.age^bird.iq

eq7 <- function(a, b, c) a + b^c
dd$bird.score <- 
  mapple(eq7, a=dd$bird.mass, b=dd$bird.age, c=dd$bird.iq)

dd$bird.mass + dd$bird.age^dd$bird.iq

f <- function(x) x^2

## using a for loop
x <- rep(NA, 20)
for(i in 1:20)
  x[i] <- f(i)
# this is messy and amateurish

## instead, use an sapply
sapply(1:20, f)