####################
#Setup
####################

install.packages(c("dplyr", "plyr", "reshape"), repos = "http://cran.us.r-project.org")

library(dplyr)
library(plyr)
library(reshape)

# setwd()

set.seed(9998877)

####################
#Simulation functions
####################

#Row level intransitive calculation (simplified)
comb.row.fn.sim <- function (df, comb.row) {
  # INPUT 
  #   df: individual participants data
  #   comb.row: comb data frame split by row
  j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
  #Record choice for A vs B
  ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
  
  #Find trial row in df that has the B vs C choice in comb
  k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for B vs C
  BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
  
  #Find trial row in df that has the A vs C choice in comb
  l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for A vs C
  CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
  
  #A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A (the second is the reverse)
  Intrans <- ifelse (ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, 
                     ifelse(ApreftoB == 0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
  
  # setup data to return
  ret.dat <- data.frame(A = comb.row$A, 
                        B = comb.row$B,
                        C = comb.row$C,
                        ApreftoB = ApreftoB,
                        BpreftoC = BpreftoC,
                        CpreftoA = CpreftoA,
                        Intrans = Intrans)
  return(ret.dat)
}

#Apply row level intransitive calculation to each subject (simplified) 
comb.fn.sim <- function(Data.cut, bars) {
  images <- rep(1:bars)
  comb.sim <- t(combn(images,3))
  comb.sim <- as.data.frame(comb.sim)
  names(comb.sim) <- c("A", "B", "C")
  comb.sim$ApreftoB <- rep(NA, nrow(comb.sim))
  comb.sim$BpreftoC <- rep(NA, nrow(comb.sim))
  comb.sim$CpreftoA <- rep(NA, nrow(comb.sim))
  comb.sim$Intrans <- rep(NA, nrow(comb.sim))
  comb.sim <- mutate(comb.sim, 
                     id = 1:nrow(comb.sim))
  ddply(comb.sim, .(id), comb.row.fn.sim, df = Data.cut)
}

#Generate random values for one person
create.new.data <- function(bars){
  values <- rnorm(bars, 0, 1)
  images <- rep(1:bars)
  noise.right <- rnorm(factorial(bars)/(factorial(2)*factorial(bars-2)), 0, 1)
  noise.left <-  rnorm(factorial(bars)/(factorial(2)*factorial(bars-2)), 0, 1)
  new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2)), noise.right, noise.left))
  return(new.data)
}

#Create choices based on values adding noise for n number of subjects
#noise levels: 0.01, 0.05, 0.10, 0.25, 0.50
simulate.choice <- function(n, alpha, bars){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data(bars = bars))),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  new.data$Choice.left.P <- 1/(1+exp(((1-alpha)*new.data$Value_right + (alpha * new.data$noise.right)) - 
                                       ((1-alpha)*new.data$Value_left + (alpha * new.data$noise.left))))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}


####################
#Simulation transitivity grid
####################


simulate.transitivity <- function(n, alpha, bars){
  
  mean.intransitivity.percent <- vector(1, mode = "numeric")
  
  sd.intransitivity.percent <- vector(1, mode = "numeric")
  
  choice.sims <- simulate.choice(n = n, alpha = alpha, bars = bars)
  
  triplet.sims <- ddply(choice.sims, .(f.id), comb.fn.sim, bars = bars, .progress="text")
  
  intransitive.sims <- ddply(triplet.sims, .(f.id), summarise, 
                                    Intrans = sum(Intrans))
  
  intransitive.sims$PercentIntr <- intransitive.sims$Intrans/(factorial(bars)/(factorial(3)*factorial(bars - 3)))*100
  
  mean.intransitivity.percent <- mean(intransitive.sims$PercentIntr)
  
  sd.intransitivity.percent <- sd(intransitive.sims$PercentIntr)
  
  out <- list(choice.sims = choice.sims,
              triplet.sims = triplet.sims,
              intransitive.sims = intransitive.sims,
              mean.intransitivity.percent = mean.intransitivity.percent,
              sd.intransitivity.percent = sd.intransitivity.percent)
  
  return(out)
}

####################
#Simulate
####################

sim.1.00.30 <- simulate.transitivity(n = 100, alpha = 1, bars = 30)
save(sim.1.00.30, file = paste("sim.1.00.n.", Sys.Date(), ".RData", sep = ""))

sim.0.75.30 <- simulate.transitivity(n = 100, alpha = 0.75, bars = 30)
save(sim.0.75.30, file = paste("sim.0.75.n.", Sys.Date(), ".RData", sep = ""))

sim.0.50.30 <- simulate.transitivity(n = 100, alpha = 0.50, bars = 30)
save(sim.0.50.30, file = paste("sim.0.50.n.", Sys.Date(), ".RData", sep = ""))

sim.0.25.30 <- simulate.transitivity(n = 100, alpha = 0.25, bars = 30)
save(sim.0.25.30, file = paste("sim.0.25.n.", Sys.Date(), ".RData", sep = ""))

sim.0.10.30 <- simulate.transitivity(n = 100, alpha = 0.10, bars = 30)
save(sim.0.10.30, file = paste("sim.0.10.n.", Sys.Date(), ".RData", sep = ""))

sim.1.00.40 <- simulate.transitivity(n = 100, alpha = 1, bars = 40)
save(sim.1.00.40, file = paste("sim.1.00.n.", Sys.Date(), ".RData", sep = ""))

sim.0.75.40 <- simulate.transitivity(n = 100, alpha = 0.75, bars = 40)
save(sim.0.75.40, file = paste("sim.0.75.n.", Sys.Date(), ".RData", sep = ""))

sim.0.50.40 <- simulate.transitivity(n = 100, alpha = 0.50, bars = 40)
save(sim.0.50.40, file = paste("sim.0.50.n.", Sys.Date(), ".RData", sep = ""))

sim.0.25.40 <- simulate.transitivity(n = 100, alpha = 0.25, bars = 40)
save(sim.0.25.40, file = paste("sim.0.25.n.", Sys.Date(), ".RData", sep = ""))

sim.0.10.40 <- simulate.transitivity(n = 100, alpha = 0.10, bars = 40)
save(sim.0.10.40, file = paste("sim.0.10.n.", Sys.Date(), ".RData", sep = ""))










