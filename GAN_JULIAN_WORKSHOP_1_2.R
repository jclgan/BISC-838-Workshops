### Workshop 1 2021-01-25

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

