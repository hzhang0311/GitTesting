set.seed(1987)

v.n <- c("healthy", "sick", "dead")
n.s <- length(v.n)
n.t <- 60
n.i <- 10000
v.M_Init <- rep("healthy", times = n.i)

# Transition probabilities
p.HS <- 0.05
p.HD <- 0.02
p.SD <- 0.1

# Costs, utilities, discount rate
c.H <- 1500
c.S <- 5000
c.D <- 0
u.H <- 1
u.S <- 0.85
u.D <- 0
d.r <- 0.03

# Construct probability function
Probs <- function(M_it){
  p.it <- vector("numeric", length(v.n))
  p.it[M_it == "healthy"] <- c(1 - p.HD - p.HS, p.HS, p.HD)
  p.it[M_it == "sick"] <- c(0, 1 - p.SD, p.SD)
  p.it[M_it == "dead"] <- c(0, 0, 1)
  
  return(p.it)
}

# Construct cost function
Costs <- function(M_it){
  c.it <- c()
  c.it[M_it == "dead"] <- c.D
  c.it[M_it == "healthy"] <- c.H
  c.it[M_it == "sick"] <- c.S
  
  return(c.it)
}

# Construct effectiveness function
Effs <- function(M_it){
  u.it <- c()
  u.it[M_it == "dead"] <- u.D
  u.it[M_it == "healthy"] <- u.H
  u.it[M_it == "sick"] <- u.S
  
  return(u.it)
}

# Start simulation
m.M <- matrix(NA, n.i, n.t+1)
m.C <- matrix(NA, n.i, n.t+1)
m.E <- matrix(NA, n.i, n.t+1)

for (i in 1:n.i) {
  m.M[i, 1] <- v.M_Init[i]
  m.C[i, 1] <- Costs(m.M[i, 1])
  m.E[i, 1] <- Effs(m.M[i, 1])
  
  for (t in 1:n.t) {
    v.p <- Probs(m.M[i, t])
    m.M[i, t+1] <- sample(x = v.n, prob = v.p, size = 1)
    m.C[i, t+1] <- Costs(m.M[i, t+1])
    m.E[i, t+1] <- Effs(m.M[i, t+1])
  }
}

# Calculate total costs and QALYs
v.dw <- 1 / (1 + d.r) ^ (0:n.t)

tc <- m.C %*% v.dw
te <- m.E %*% v.dw

tc_avg <- mean(tc)
te_avg <- mean(te)