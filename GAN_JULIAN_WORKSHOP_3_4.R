# WORKSHOP 3
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

N.next <- function(r, N.current, K) {
  return((1 + r * (1 - (N.current/K))) * N.current)
}
n <- 50

# Graph 1
for (i in 1:50){
  Nt <- vector()
  Nt[1] <- 10
  r <- 2.56
  K <- 1000
  for (i in 2:n){
    Nt[i] <- Nt[i-1] * (1 + r * (1 - (Nt[i-1])/K))
  }
}

plot_N <- data.frame(gen,Nt)

graph_a <- ggplot(plot_N, aes(x=gen, y=Nt))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


# Graph 2
for (i in 1:50){
  Nt <- vector()
  Nt[1] <- 10
  r <- 2.0
  K <- 1000
  for (i in 2:n){
    Nt[i] <- Nt[i-1] * (1 + r * (1 - (Nt[i-1])/K))
  }
}

plot_N <- data.frame(gen,Nt)

graph_b <- ggplot(plot_N, aes(x=gen, y=Nt))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


# Graph 3
for (i in 1:50){
  Nt <- vector()
  Nt[1] <- 10
  r <- 3.0
  K <- 1000
  for (i in 2:n){
    Nt[i] <- Nt[i-1] * (1 + r * (1 - (Nt[i-1])/K))
  }
}

plot_N <- data.frame(gen,Nt)

graph_c <- ggplot(plot_N, aes(x=gen, y=Nt))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


# Graph 4
for (i in 1:50){
  Nt <- vector()
  Nt[1] <- 10
  r <- 3.01
  K <- 1000
  for (i in 2:n){
    Nt[i] <- Nt[i-1] * (1 + r * (1 - (Nt[i-1])/K))
  }
}

plot_N <- data.frame(gen,Nt)

graph_d <- ggplot(plot_N, aes(x=gen, y=Nt))+
  geom_line()+
  ylim(-10,2000)+
  labs(x = "Generation",
       y = "Population size") +
  annotate("text", x=25, y=1900,
           label = paste("italic(r)==", r),
           parse=TRUE,
           size=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

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

