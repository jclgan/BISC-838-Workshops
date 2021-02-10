## SQUIRREL MATRIX
L <- matrix(c(0,6,10,2,
              0.2,0,0,0,
              0,0.25,0,0,
              0,0,0.1,0.05), ncol=4, byrow=TRUE)
##

# 1. Find the eigenvalues and right eigenvectors of L
r.vals.vecs <- eigen(L)
values <- r.vals.vecs$val
vectors <- r.vals.vecs$vectors

eval.r <- r.vals.vecs$values[1]
evec.r <- r.vals.vecs$vectors[,1]
vright <- r.vals.vecs$vectors[,1]/sum(r.vals.vecs$vectors[,1])

# 2. What is the long-term growth rate?
eval.r
# 1.2655

## What proportion of the pop will be in each age class once it has reached
## stable age distribution?
vright
# A: 1 y.o.: 0.8391, 2 y.o.: 0.1326, 3 y.o.: 0.0262, 4+ y.o.: 0.0022


# 3. Calculate N_100
N0 <- matrix(c(1,1,1,1))
years <- 100
N.projected <- matrix(0, nrow = nrow(L), ncol=years+1)
N.projected[,1] <- N0

for(i in 1:years){
  N.projected[, i+1] <- L %*% N.projected[,i]
  }

N.projected[,100]
dist <- N.projected[,100]/sum(N.projected[,100])
round(dist,4)

# [1] 0.8391 0.1326 0.0262 0.0022
# Age class proportions are similar to the right eigenvector


# 4. Repeat for different age distributions & number of iterations
N0 <- matrix(c(1,3,2,60))
years <- 342
N.projected <- matrix(0, nrow = nrow(L), ncol=years+1)
N.projected[,1] <- N0

for(i in 1:years){
  N.projected[, i+1] <- L %*% N.projected[,i]
}

dist <- N.projected[,100]/sum(N.projected[,100])
round(dist,4)

# [1] 0.8391 0.1326 0.0262 0.0022


N0 <- matrix(c(100,1,100,0))
years <- 60
N.projected <- matrix(0, nrow = nrow(L), ncol=years+1)
N.projected[,1] <- N0

for(i in 1:years){
  N.projected[, i+1] <- L %*% N.projected[,i]
}

dist <- N.projected[,100]/sum(N.projected[,100])
round(dist,4)

# [1] 0.8391 0.1326 0.0262 0.0022


# 5. Repeat Q3 using the matrix power function instead of a for loop
matrix.pow <- function(A,n){
  if(n>1) {
    B <- A
    for (i in 2:n) A <- A %*% B
  }
  return(A)
}

years <- 100
N0 <- matrix(c(1,1,1,1))

N100 <- matrix.pow(L,100) %*% N0
dist <- t(round(N100/sum(N100),4))
dist
# [1,] 0.8391 0.1326 0.0262 0.0022


# 6. Calculate the left eigenvector
Lt <- t(L)
l.vals.vecs <- eigen(Lt)
evec.l <- l.vals.vecs$vectors[,1]

vleft <- l.vals.vecs$vectors[,1]/l.vals.vecs$vectors[1,1]
vleft

vleft.adj <- vleft / (vleft %*% vright)

vleft.adj %*% vright
# [1,]    1

lhs <- vleft.adj %*% L
lhs
# [1,] 0.6688773 4.232452 5.372278 1.100544

rhs <- eval.r %*% vleft.new
rhs
# [1,] 0.6688773 4.232452 5.372278 1.100544


# 7. Which class has the highest age reproductive value?
# A: 3 year-olds


# 8. Compute the total pop size after 10 gens with 1 individual per age class
N0.1 <- c(1,0,0,0)
N0.1.adj <- N0.1 %*% vleft.adj  
N10.1 <- N0.1.adj %*% eval.r^10
N10.1

N10.2 <- (c(0,1,0,0) %*% vleft.adj) %*% eval.r^10
N10.2

N10.3 <- (c(0,0,1,0) %*% vleft.adj) %*% eval.r^10
N10.3

N10.4 <- (c(0,0,0,1) %*% vleft.adj) %*% eval.r^10
N10.4

# Yes, I agree with my inferences; the 3 year-olds are approximately 5 times as fertile
# as 4+ year-olds
