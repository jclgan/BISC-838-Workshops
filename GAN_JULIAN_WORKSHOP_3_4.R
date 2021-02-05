# WORKSHOP 3 -----------------------------------------------------------
## If statements

for (i in 1:10) {
  if(i <= 5) {
    print(paste(i, "is less than or equal to 5"))
  } else{
    print(paste(i, "is greater than 5"))
  }
}

# 1.
for (i in 1:10) {
  if(i %% 2 == 0) {
    print(paste(i, "is an even number"))
  } else{
    print(paste(i, "is an odd number"))
  }
}


## Recursive functions
N <- function(r, t) {
  return((1+r) * N(r, t-1))
}

N(r=0.1, t=5)
# "Error: C stack usage  15925344 is too close to the limit"

N <- function(r, N0, t) {
  if (t == 0) {
    return(N0)
  } else{
    return((1 + r) * N(r, N0, t - 1))
  }
}

N(r=0.1, N0=10, t=10)

# 1.

# Three functions
require(dplyr)
require(ggplot2)

t <- c(0:20)

pop1 <- sapply(t, N, N0=5, r=0.1)
curve1 <- data.frame(t, pop1) %>% 
  mutate(lambda = "1.1") %>% 
  rename(pop = pop1)

pop2 <- sapply(t, N, N0=5, r=0.0)
curve2 <- data.frame(t, pop2) %>% 
  mutate(lambda = "1.0") %>% 
  rename(pop = pop2)

pop3 <- sapply(t, N, N0=5, r=-0.1)
curve3 <- data.frame(t, pop3) %>% 
  mutate(lambda = "0.9") %>% 
  rename(pop = pop3)

plotdata <- rbind(curve1, curve2, curve3)

ggplot(data = plotdata, aes(x=t, y=pop, colour = lambda)) +
  geom_point() +
  ylim(0,50) +
  labs(x = expression(paste("Time, ", italic("t"))),
       y = expression(paste("Population size, ", italic("N")[italic("t")]))) +
  scale_colour_manual(labels = c(expression(paste(italic(lambda), " = 0.9")),
                                 expression(paste(italic(lambda), " = 1.0")),
                                 expression(paste(italic(lambda), " = 1.1"))),
                      guide = guide_legend(reverse=TRUE),
                      values = c("red", "black", "blue")) +
  theme(legend.position = c(0.12,0.9),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.3, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggsave("exponential_growth.pdf", width=6, height=6)


## Numerical iteration

N.next <- function(r, N.current) {
  return((1+r)*N.current)
}

N1 <- 10
N2 <- N.next(r=0.1, N.current=N1)
N3 <- N.next(r=0.1, N.current=N2)
N4 <- N.next(r=0.1, N.current=N3)
c(N1, N2, N3, N4)

# 1
N <- c(10)

for (i in 1:19){
  N[i+1] <- N.next(r=0.1, N.current=N[i])
}
N[20]


# 2 Discrete time equation for logistical growth
require(ggplot2)
gen <- c(1:50)
r1 <- 2.56
r2 <- 2
r3 <- 3
r4 <- 3.01

N <- function(r, K, N0, t){
  res <- rep(0,t)
  res[1] <- N0
  for(Nt in 2:t)
    res[Nt] <- res[Nt-1] * (1 + r * (1 - res[Nt-1]/K))
  round(res)
}

# Graph 1

pop <- N(r1, 1000, 20, 50)

plot_N <- data.frame(gen,pop)

graph_a <- ggplot(plot_N, aes(x=gen, y=pop))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r1),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
graph_a

# Graph 2

pop <- N(r2, 1000, 20, 50)

plot_N <- data.frame(gen,pop)

graph_b <- ggplot(plot_N, aes(x=gen, y=pop))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r2),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
graph_b

# Graph 3

pop <- N(r3, 1000, 20, 50)

plot_N <- data.frame(gen,pop)

graph_c <- ggplot(plot_N, aes(x=gen, y=pop))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r3),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
graph_c

# Graph 4

pop <- N(r4, 1000, 20, 50)

plot_N <- data.frame(gen,pop)

graph_d <- ggplot(plot_N, aes(x=gen, y=pop))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r4),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
graph_d

# Combine all plots
require(cowplot)
plot_grid(graph_a, graph_b, graph_c, graph_d)


# 3 Bifurcation plot

rmax <- 3
plot(-1, -1, xlim = c(1.5, rmax), ylim = c(0, 1400), xlab = "r", ylab = "Observed population sizes")
K <- 1000
r <- seq(from = 0, to = rmax, by = 0.001)

n <- 100

for (z in 1:length(r)) {
  Nt <- vector()
  Nt[1] <- 10
  for (i in 2:n) {
    Nt[i] <- Nt[i-1] * (1 + r[z] * (1 - (Nt[i-1])/K)) 
  }
  uniqueN <- unique(Nt[85:n])
  points(rep(r[z], length(uniqueN)), uniqueN, cex = 0.1, pch = 19)
}

# WORKSHOP 4 ------------------------------------------------------------------

## MATRICES

vals <- 1:6
m <- matrix(vals, nrow=2, ncol=3)
m <- matrix(vals, nrow=2, ncol=3, byrow=TRUE)
vals <- runif(6)
m <- matrix(vals, nrow=2, ncol=3)

matrix(1:4, nrow=2, ncol=3)
matrix(1:8, nrow=2, ncol=3)

dim(matrix(1:6, nrow=2, ncol=3))
m[2,3]
m[2,]
m[,3]

m <- matrix(runif(12), nrow=3, ncol=4)
m.subset <- m[c(1,2), c(1,3)]

m[1,]
m[1,,drop=FALSE]
dim(m[1,])
dim(m[1,,drop=FALSE])

m[3]

a <- c(1,2,3)
b <- c(-3,-4,-5)
cbind(a,b)
rbind(a,b)

# 1. Create some matrices of your own
m1 <- matrix(1:9, nrow = 3, ncol = 3)

x <- 1:5
y <- 6:10
z <- 11:15
m2 <- cbind(x,y,z)

# 2. Use vector commands to remove rows or columns
m3 <- matrix(1:25, nrow=5, ncol=5)
m3_dropc <- m3[, -c(3,4)]
m3_dropr <- m3[-c(2,5),]


## Basic matrix operations
M <- matrix(sample(1:9), nrow=3)
N <- matrix(sample(1:9)/10, nrow=3)

# 1. Compute the "dot product" of two vectors
v <- c(2,3,4)
dp <- v %*% M
dp

# 2. Matrix multiplication is not commutative
MN <- M %*% N
NM <- N %*% N
# M x N != N %*% M

# 3. Matrix multiplication is associative
P <- matrix(sample(1:9)*2, nrow=3)
M_NP <- M %*% (N %*% P)
M_NP
MN_P <- (M %*% N) %*% P
M_NP
# M(NP) = MN(P)

# 4. Matrix multiplying a matrix by its inverse yields the identity matrix
invM <- solve(M)
I <- zapsmall(M %*% invM)


## Arrays

vals <- 1:24
a <- array(vals, dim=c(3,2,4))
a[2,1,3]

# 1,2 Create your own array and try the dim command
vals <- sample(1:100)
b <- array(vals, dim = c(4, 8, 12))
b[3,6,4]
# Row 3, col 6, matrix 4 = 64


## Dimnames

phy.mat <- matrix(0, nrow=3, ncol=3)
phy.dists <- runif(3)
phy.mat[upper.tri(phy.mat)] <- phy.dists
phy.mat[lower.tri(phy.mat)] <- phy.dists

phy.mat[1,2]

sp.names <- c('dog', 'cat', 'mouse')
colnames(phy.mat) <- sp.names
rownames(phy.mat) <- sp.names
dimnames(phy.mat) <- list(sp.names, sp.names)

phy.mat['dog', 'cat']

# 1. Create a 5x4 matrix, and assign row & column names to it

beau <- c(10,20,16,19,18)
cad <- c(10,14,16,9,20)
caleb <- c(10,12,14,20,16)
fjord <- c(13,11,18,14,7)
names <- c("Beau", "Caduceus", "Caleb", "Fjord")
stats <- c("Str", "Dex", "Con", "Int", "Wis")
m9.mat <- cbind(beau, cad, caleb, fjord)
dimnames(m9.mat) <- list(stats, names)
m9.mat

# 2. Create a smaller subset

emp.kids <- m9.mat[c("Dex", "Int"),c("Beau","Caleb")]
emp.kids


## Multi-variable models using matrix notation
require(tidyr)
require(ggplot2)

a <- 1
b <- 1
num.iter <- 250

N1 <- rep(NA,num.iter)
N2 <- rep(NA, num.iter)

N1[1] <- 20
N2[1] <- 980

for (t in 2:num.iter){
  N1[t] <- (1-a) * N1[t-1] + b*(N2[t-1])
  N2[t] <- (1-b) * N2[t-1] + a*(N1[t-1])
}

df <- data.frame(1:250, N1, N2) %>% 
  pivot_longer(N1:N2, names_to = "population", values_to = "size") %>% 
  rename(gen = X1.250)
  
plot1 <- ggplot(data=df, aes(x=gen, y=size, colour=population)) +
  geom_line(size=2) +
  ylim(0,1000) +
  labs(x = "Generation",
       y = "Population size",
       colour = "Population") +
  annotate("text", x=230, y=1000,
           label = paste("alpha ==", a),
           parse=TRUE,
           size=5) +
  annotate("text", x=230, y=950,
           label = paste("beta ==", b),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
plot1

## 2. Repeat code with alpha = 0.98 and beta = 0.99
a <- 0.98
b <- 0.99

for (t in 2:num.iter){
  N1[t] <- (1-a) * N1[t-1] + b*(N2[t-1])
  N2[t] <- (1-b) * N2[t-1] + a*(N1[t-1])
}

df <- data.frame(1:250, N1, N2) %>% 
  pivot_longer(N1:N2, names_to = "population", values_to = "size") %>% 
  rename(gen = X1.250)

plot2 <- ggplot(data=df, aes(x=gen, y=size, colour=population)) +
  geom_line() +
  ylim(0,1000) +
  labs(x = "Generation",
       y = "Population size",
       colour = "Population") +
  annotate("text", x=230, y=1000,
           label = paste("alpha ==", a),
           parse=TRUE,
           size=5) +
  annotate("text", x=230, y=950,
           label = paste("beta ==", b),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
plot2


## 3. Update code as function

dispersal <- function(a, b){
  num.iter <- 250
  N1 <- rep(NA,num.iter)
  N2 <- rep(NA, num.iter)
  N1[1] <- 20
  N2[1] <- 980
  for (t in 2:num.iter){
    N1[t] <- (1-a) * N1[t-1] + b*(N2[t-1])
    N2[t] <- (1-b) * N2[t-1] + a*(N1[t-1])
  }
  
  df <- data.frame(1:250, N1, N2) %>% 
    pivot_longer(N1:N2, names_to = "population", values_to = "size") %>% 
    rename(gen = X1.250)
  
  plot2 <- ggplot(data=df, aes(x=gen, y=size, colour=population)) +
    geom_line() +
    ylim(0,1000) +
    labs(x = "Generation",
         y = "Population size",
         colour = "Population") +
    annotate("text", x=230, y=1000,
             label = paste("alpha ==", a),
             parse=TRUE,
             size=5) +
    annotate("text", x=230, y=950,
             label = paste("beta ==", b),
             parse=TRUE,
             size=5) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  plot2
}

dispersal(0.01,0.02)


## 4. Create a plot where x-axis is alpha & y-axis is beta

alpha.vals <- seq(from=0,to=1,length=10)
beta.vals <- seq(from=0,to=1,length=10)

combinations <- expand.grid(alpha=alpha.vals,beta=beta.vals)

myfun <- function(a, b) {
  num.iter <- 100
  N1 <- rep(NA, num.iter)
  N2 <- rep(NA, num.iter)
  N1[1] <- 20
  N2[1] <- 980
  for (t in 2:num.iter) {
    N1[t] <- (1 - a) * N1[t - 1] + b * (N2[t - 1])
    N2[t] <- (1 - b) * N2[t - 1] + a * (N1[t - 1])
  }
  return(N1[100])
}

res <- round(mapply(myfun, combinations[,'alpha'], combinations[,'beta']))

mat <- matrix(res, nrow=10, ncol=10)
image(mat,
      xlab = expression(alpha),
      ylab = expression(beta))