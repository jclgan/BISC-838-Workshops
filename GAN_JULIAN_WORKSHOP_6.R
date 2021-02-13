# PROBABILITY DISTRIBUTIONS

## Uniform

runif(n=1) # between [0,1]
runif(n=1, min=-2, max=3) # is the same as
runif(n=1)*5 - 2 #because the range is the same

## Binomial
rbinom(n=1, size=80, prob=0.7)

sapply(1:10, function(x) 
  rbinom(n=1, size=100, prob=0.7)
  )

### 1. Calculate the mean of each output
mean(rbinom(n=1e6, size=80, prob=0.7))

mean(sapply(1:1e6, function(x) 
  rbinom(n=1, size=100, prob=0.7)
  )
  )

### output falls within expectations

time.start <- proc.time()
mean(sapply(1:10, function(x) 
  rbinom(n=1, size=100, prob=0.7)
)
)
time.end <- proc.time()
time.end-time.start
### elapsed = 0.04s

time.start <- proc.time()
mean(sapply(1:1e6, function(x) 
  rbinom(n=1, size=100, prob=0.7)
)
)
time.end <- proc.time()
time.end-time.start
### elapsed = 3.11s


## Poisson

rpois(n=1, lambda=10)

### 1. Confirm for a large # of draws, mean & var = lambda
mean(rpois(n=1e6, lambda=10))
### [1] 10.00347
var(rpois(n=1e6, lambda=10))
### [1] 10.0048

### 2. Verify "the sum of k draws from a Poisson with mean lambda" = 
### "a single draw from a Poisson with mean k*lambda"
set.seed(1234)
k <- 1e6
l <- 42
sum(rpois(n=k, lambda=l))
### [1] 42007771
set.seed(1234)
rpois(n=1, lambda = (k*l))
### [1] 42007038


## Vector arguments

set.seed(1234)
my.probs <- 1:10/10
rbinom(n=10, size=100, prob=my.probs)
### [1]   6  21  29  38  48  57  68  82  92 100

### 1. Increase replication to verify that output from rbinom match those specified
### by the probability vector.

set.seed(1234)
m <- matrix(rbinom(n=100, size=100, prob=my.probs), ncol=10)
m.2 <- cbind(m, rowMeans(m)/100)
### Row means are approximately equal to the probability vector
m.2[,11]


## Lotka-Volterra predator-prey model

### 1. Iterate the model

run.lv <- function(r,b,c,d,N0,num.iter) {
  seed <- floor(runif(1)*1e6)
  H <- rep(NA,num.iter)
  P <- rep(NA,num.iter)
  H[1] <- N0[1]
  P[1] <- N0[2]
  
  for(t in 2:num.iter) {
    H[t] <- max(0, H[t-1] + r*H[t-1] - b*H[t-1]*P[t-1])
    P[t] <- max(0, P[t-1] + c*H[t-1]*P[t-1] - d*P[t-1])
  }
  return(list(H=H,P=P,seed=seed))
}

out <- run.lv(r=0.05,
              b=0.001,
              c=0.0002,
              d=0.1,
              N0=c(500,45),
              num.iter=2000)

plot(out$H, type="l", col="blue", 
     ylim=c(0,max(out$H,out$P)),
     xlab = "Generation",
     ylab = "Population size")
lines(out$P, col="red")
legend("topleft", legend=c("Prey", "Predators"),
       text.col=c("blue", "red"),
       bty="n")

### 2. Plot predators vs. prey through time
plot(out$P, out$H, type = "l",
     xlab = "Predators",
     ylab = "Prey")

### 3. Run modified plot
run.lv <- function(r,b,c,d,N0,num.iter) {
  seed <- floor(runif(1)*1e6)
  H <- rep(NA,num.iter)
  P <- rep(NA,num.iter)
  H[1] <- N0[1]
  P[1] <- N0[2]
  
  for(t in 2:num.iter) {
    H[t] <- max(0, H[t-1] + r*H[t-1] - b*H[t-1]*P[t-1])
    P[t] <- max(0, P[t-1] + c*H[t-1]*P[t-1] - d*P[t-1])
  }
  return(list(H=H,P=P))
}

out <- run.lv(r=0.05,
              b=0.001,
              c=0.0002,
              d=0.1,
              N0=c(500,45),
              num.iter=2000)

H <- unlist(out$H)
P <- unlist(out$P)

plot(NA,
     xlim=c(0,max(P)), ylim=c(0,max(H)),
     xlab="Predators", ylab="Prey", las=1)
for(i in 1:length(out$P)) {
  lines(P[i:(i+1)], H[i:(i+1)], col="black")
}


### 4. Poisson distribution
run.lv.pois <- function(r,b,c,d,N0,num.iter) {
  seed <- floor(runif(1)*1e6)
  H <- rep(NA,num.iter)
  P <- rep(NA,num.iter)
  H[1] <- N0[1]
  P[1] <- N0[2]
  
  for(t in 2:num.iter) {
    H[t] <- max(0, rpois(n=1, lambda=(H[t-1] * (1 + r - b*P[t-1]))))
    P[t] <- max(0, rpois(n=1, lambda=(P[t-1] * (1 + c*H[t-1] - d))))
  }
  return(list(H=H,P=P,seed=seed))
}

out <- run.lv.pois(r=0.05,
                   b=0.001,
                   c=0.0002,
                   d=0.1,
                   N0=c(500,45),
                   num.iter=200)

plot(out$H, type="l", col="blue", 
     ylim=c(0,max(out$H,out$P)),
     xlab = "Generation",
     ylab = "Population size")
lines(out$P, col="red")
legend("topleft", legend=c("Prey", "Predators"),
       text.col=c("blue", "red"),
       bty="n")
