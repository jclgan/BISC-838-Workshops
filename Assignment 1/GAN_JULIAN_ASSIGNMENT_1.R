## ASSIGNMENT 1
## Julian Gan
## Recreation of Hughes, T.P. (1984).

## Matrix during the Calm year
C <- matrix(c(0.6020, 0.1167, 0.0217, 0.0741,
              0.1681, 0.6167, 0.1087, 0.0370,
              0.0000, 0.2000, 0.8043, 0.1481,
              0.0000, 0.0171, 0.0217, 0.9259),
            nrow=4, byrow=TRUE)

val.vec.C <- eigen(C)

# Calculate the equilibrium growth rate and stable size distribution
lambda.C <- val.vec.C$val[1] 
rvec.C <- val.vec.C$vectors[,1]/sum(val.vec.C$vectors[,1])


## Matrix during the Stormy year
S <- matrix(c(0.5135, 0.0794, 0.0727, 0.0714,
              0.2072, 0.6349, 0.2364, 0.2500,
              0.0000, 0.1746, 0.6545, 0.0714,
              0.0000, 0.0000, 0.0000, 0.7500),
            nrow=4, byrow=TRUE)


val.vec.S <- eigen(S)

# Calculate the equilibrium growth rate and stable size distribution
lambda.S <- val.vec.S$val[1] # It matches!
rvec.C <- val.vec.S$vectors[,1]/sum(val.vec.S$vectors[,1])


## Project the changes in a population over time
## using both the calm and storm matrices
N0 <- matrix(c(1000,0,0,0))
N0

pop <- function(m) {
  N.projected <- matrix(0, nrow = nrow(m), ncol=years+1)
  N.projected[,1] <- N0
  for(i in 1:years){
    N.projected[, i+1] <- m %*% N.projected[,i]
  }
  return(N.projected)
}

par(mfrow=c(1,2)) 
years <- 75
matplot(0:years,
        t(pop(C)),
        type= "l",
        lwd = 2,
        lty = 1,
        ylab = "Cohort size",
        xlab = "Time (years)",
        main = "A. Calm (1977/78)",
        log = "y",
        xlim = c(0,75)
)
legend("topright", legend = c("I", "I", "III", "IV"), 
       col = 1:4, lwd = 2, lty = 1)

years <- 21
matplot(0:years,
        t(pop(S)),
        type= "l",
        lwd = 2,
        lty = 1,
        ylab = "Cohort size",
        xlab = "Time (years)",
        main = "B. Storm (1978/79)",
        log = "y",
        xlim = c(0,50)
)
legend("topright", legend = c("I", "I", "III", "IV"), 
       col = 1:4, lwd = 2, lty = 1)