### Workshop 1 2021-01-25----------------------------------------

## Calculator
# 1. Try log and a few other functions
log(2)
sum(3,5,6)
exp(42)

# 2. Assign the number 3 to a variable
x <- 3

# 3. Assign character strings to named variables
z <- "Hello"

# 4. Assign a single number to variable x and another number to 
# the variable y. Store the result as a third variable
x <- 18
y <- 10.7
z <- x*y
print(z)

# 5. TRUE or FALSE logical operations
2 + 2 == 4 #TRUE
3 <= 2 #FALSE
"A" > "a" #TRUE
"Hi" != "hi" #TRUE


## Vectors
# 1. Assign a set of 10 numbers to a variable x
x <- c(-27, -52, 98, 87, -67, -10, -23, -41, 16, 48)
is.vector(x)

# 2. Specify the vector elements of x. 

x[5]
x[1:3]
x[c(2, 4, 9)]
x[c(-1, -3)]

# Print the 3rd and 6th elements of x with a single command
x[c(3,6)]

# 3. Enter the function
length(x)

# It is ok to use as follows
x[length(x)]

# 4. Logical operations can also be used to generate indicators.
x > 0

# Print all elements of x that are non-negative
x[x > 0]

# The which command will identify the elements corresponding to TRUE
which(x > 0)

# 5. change the fifth elements of x to 0
x[5] <- 0

# Change the 2nd, 6th, and 10th values of x all to 3 new numbers
# with a single command
x[c(2, 6, 10)] <- c(563, -4586, 342)

# 6. Change the 2nd value of x to a missing value.
x[2] <- NA
is.na(x)
x[!is.na(x)]

# 7. Create a second numerical vector y of the same length as x.
# Try out a few mathematical operations on the whole vectors of numbers

y <- c(12, 34, 16, -43, 68, 36, -67, -32, 1, 76)
z <- x * y
z <- y - 2 * x

z2 <- x >= y
z3 <- x[abs(x) < abs(y)]

z4 <- 2 * x

x <- c(dog=1, cat=2, mouse = 333)
x["mouse"]


## Lists
x <- list (5, "hello", matrix (1:4))
x[[2]]

x2 <- list(a=5, b="hello", c=matrix(1:4))
x2[['a']]
x2[['d']] <- 17

empty.list <- vector(mode = "list", length = 5)


## Functions
# 1. Syntax

myfunction1 <- function(x) {
  out <- 2*x^2 + 3
  return(out)
}

z <- myfunction1(5)
z

z2 <- myfunction1(3.14)
z2

# 2. Try typing 'out' in the R console
out()

# Equivalents
myfunction1 <- function(x) {
  out <- 2*x^2 + 3
  out
}

myfunction1 <- function(x) {
  2*x^2 + 3
}

myfunction1 <- function(x)
  2*x^2 + 3

# 3. Construct your own function with two arguments.
myfunction2 <- function(x, y) {
  result <- 9*x + y^-3
  result
}

a1 <- myfunction2(3,6)
a2 <- myfunction2(100, 56)


## For loops
for (i in 1:50) {
  print(i)
}

# 1. Write a function that squares a number, and then use a for loop to apply
# this function to the numbers 1,...,20

myfunction3 <- function(x) {
  x^2
}

for (x in myfunction3(1:20)) {
  print(x)
}

# 2. Write a for loop to fill in a vector with the above values
v <- c()

for (x in 1:20) {
  v[x] = myfunction3(x)
}


# 3. Write a for loop to fill in a list with the above values
l <- list()

for (x in 1:20) {
  l[[x]] = myfunction3(x)
}


## Apply statements
x <- c(1,2,333,65,45,-88)
lapply(x, myfunction1)

# 1. Try using sapply
sapply(x, myfunction1)

# 2. Try using mapply
y <- c(64, 452, 897, -9, 38, 12)
mapply(myfunction2, x, y)

# 3. Try the following command. What happened?
sapply(1:10, myfunction2, y=5)

# A: For each value x between 1 and 10 in the function, y was kept constant as 5.


### Workshop 2 2021-01-28-----------------------------------

## Probability Distributions
runif(n=1)
runif(n=5)
rnorm(n=10)
rnorm(n=10, mean=3, sd=2)

# 1. Use the mean and sd function to confirm that... rnorm... gives you a mean
# and sd very close to specified values (0 and 1 respectively)
mean(rnorm(n=1000))
sd(rnorm(n=1000))


## Basic plotting
plot(x=1:5, y=c(1,5,4,2,3))

plot(x=1:5, y=c(1,5,4,2,3), pch=16, col='red', las=1,
     xlab='Speed', ylab='Distance')

plot(NA, las=1,
     xlab='Speed', ylab='Distance',
     xlim=c(1,5), ylim=c(0,10))
points(x=1:5, y=c(1,5,4,2,3), pch=16, col='red')

# 1. Use an apply statement to generate a set of values for a parabola
# and then create a plot of these points
# 2. Use lines cmd to add lines connective consecutive points.
# 3. Try adding a smooth parabola using the curve command.
# 4. Use abline to add a horizontal, vertical, and sloped line to the plot.

parfun <- function(x)
  2*x^2 - 3*x + 4

x <- c(2, 5, -6, -3, 4, -7, 1)
y <- sapply(x, parfun)
plot(x,y)

plot(NA, xlim=range(x), ylim=range(y), 
     xlab='x', ylab='y')
points(x=x, y=y, pch=16, col='blue')
lines(x=x[order(x)], y=y[order(x)])
curve(parfun, lwd=2, col='blue', add = TRUE)
abline(a=5, b=5, lty=2, col='red')
abline(v=2, lty=3, col='red')
abline(h=39, lty=4, col='red')


## Arrows and polygons

age <- 1:10
body.size <- rnorm(10, mean=log(age), sd=0.1)
uncertainty <- runif(10)

# 1. Plot these data and use arrows
plot(NA, xlim=range(age), ylim=c(-1.0,4.0),
     xlab="Age", ylab="Body size",
     axes=FALSE)
axis(2, at = c(-1, 0, 1, 2, 3, 4), labels = c('-1.0', '0.0', '1.0', '2.0', '3.0', '4.0'))
axis(1, at= c(2, 4, 6, 8, 10), labels=c('2', '4', '6', '8', '10'))
box()
points(x=age, y=body.size, pch=16)
arrows(x0=age, y0=body.size-uncertainty, 
       x1=age, y1=body.size+uncertainty,
       length=0)

# 2. Create a shaded polygon
plot(NA, xlim=range(age), ylim=c(-1.0,4.0),
     xlab="Age", ylab="Body size",
     axes=FALSE)
axis(2, 
     at = c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), 
     labels = c('-1.0', '-0.5', '0.0', '0.5', '1.0', '1.5', 
                '2.0', '2.5', '3.0', '3.5', '4.0'))
axis(1, at= c(2, 4, 6, 8, 10), labels=c('2', '4', '6', '8', '10'))
box()
points(x=age, y=body.size, pch=16)

poly.x <- c(age, rev(age))
poly.y <- c(body.size+uncertainty, rev(body.size-uncertainty))
polygon(poly.x, poly.y, col= rgb(1, 0, 0,0.3), border=NA)


## 2D plots
mm <- matrix(c(0,0,1,2), ncol=2)
image(mm)

mm.reordered <- t(mm[nrow(mm):1,])
image(mm.reordered, col=gray(10:0/10), las=1) 

smile <- matrix(c(0,0,1,1,1,1,0,0,
                  0,1,0,0,0,0,1,0,
                  1,0,1,0,0,1,0,1,
                  1,0,0,0,0,0,0,1,
                  1,0,1,0,0,1,0,1,
                  1,0,0,1,1,0,0,1,
                  0,1,0,0,0,0,1,0,
                  0,0,1,1,1,1,0,0),
                  ncol=8,
                  byrow=TRUE)
rotate <- function(x) 
  t(apply(x, 2, rev))

image(rotate(smile))


## Vector graphics
pdf('my_figure.pdf', height=3, width=3)
par(oma=c(0.1,0.1,0.4,0.4), mar=c(3,3,0.1,0.1), mgp=c(2,0.2,0))
plot(1:10, xlab='x-label', ylab='y-label', las=1)
dev.off()


## Multi-panel plots
layout(matrix(1:2, nrow=1, ncol=2))
plot(1:10, pch=16, col='red')
plot(1:10, pch=16, col='blue')

layout(matrix(1:2, nrow=2, ncol=1))
plot(1:10, pch=16, col='red')
plot(1:10, pch=16, col='blue')

plot.mat <- matrix(c(0,1,0,4,
                     2,2,0,4,
                     0.,0,3,4), nrow=3, ncol=4, byrow=TRUE)
layout(plot.mat)
plot(1:10, pch=16, col='red')
plot(1:10, pch=16, col='blue')
plot(1:10, pch=16, col='green4')
plot(1:10, pch=16, col='purple')

pdf('my_figure.pdf', height=6, width=6)
par(oma=c(0.1,0.1,0.4,0.4), mar=c(3,3,0.1,0.1), mgp=c(2,0.2,0))
layout(plot.mat, widths=c(1,1,1,3), heights=c(1,3,2))
plot(1:10, pch=16, col='red')
plot(1:10, pch=16, col='blue')
plot(1:10, pch=16, col='green4')
plot(1:10, pch=16, col='purple')
dev.off()

layout(matrix(c(1,0,2,0,3,4,4,4,4,4,4,4,
                1,0,2,0,3,0,0,0,0,0,0,0,
                1,0,2,0,3,0,5,6,0,7,8,0,
                1,0,2,0,3,0,0,0,0,0,0,0,
                1,0,2,0,3,0,9,10,0,11,12,0),
              5, 12, byrow=TRUE),
       heights=c(1,0.4,1.5,0.2,1.5),
       widths=c(0.1,0,0.25,0.05,0.25,0.3,0.07,0.07,0.3,0.08,0.07,0.3))
par(oma=c(2,4,0,0), mar=c(0.1, 0.1, 1, 0.1),
    mgp=c(2,0.2,0), tcl=0, cex.axis=0.7, cex.main=0.7)


# 1 Clock

pdf('my_clock.pdf', height=6, width=6)
clock <- function(x){
  plot(NA, xlim=c(1,5), ylim=c(0,10), xaxt='n', yaxt='n', ann=FALSE)
  points(3.05, 4.95, pch=16, cex=13, col='#30618c')
  text(3.05, 4.95, x, cex=4, font=5)
}

m <- matrix(c(0,0,0,12,0,0,0,
              0,0,11,0,1,0,0,
              0,10,0,0,0,2,0,
              9,0,0,13,0,0,3,
              0,8,0,0,0,4,0,
              0,0,7,0,5,0,0,
              0,0,0,6,0,0,0),
            nrow=7,ncol=7,byrow = TRUE)
par(mar=c(0.1, 0.1, 0.1, 0.1))
layout(m)

for (i in 1:12)
  clock(i)

plot(1, pch=16, cex=6, axes=FALSE)
dev.off()


# 2 Multi-figure panel
par(mar=c(0.5, 0.1, 0.5, 0.1))
layout(matrix(c(0,1,0,
                2,5,3,
                0,4,0), nrow=3,ncol=3))

fractal1 <- function(a){
  b <- cbind(a,a,a)
  c <- cbind(a,a*0,a)
  d <- cbind(a,a,a)
  e <- rbind(b,c,d)
  return(e)
}

fractal2 <- function(v){
  w <- cbind(v,v*0,v)
  x <- cbind(v*0,v,v*0)
  y <- cbind(v, v*0, v)
  z <- rbind(w,x,y)
  return(z)
}


t <- matrix(1,1,1)
for (i in 1:4)
  t <- fractal1(t)

u <- matrix(1,1,1)
for(i in 1:4)
  u <- fractal2(u)


image(u, col=c(0,6), axes=FALSE, asp=1)
image(u, col=c(0,12), axes=FALSE, asp=1)
image(u, col=c(0,12), axes=FALSE, asp=1)
image(u, col=c(0,6), axes=FALSE, asp=1)
image(t, col=c(0,4), axes=FALSE, asp=1)
