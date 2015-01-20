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
comb.fn.sim <- function(Data.cut) {
  images <- rep(1:20)
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
create.new.data <- function(){
  values <- rnorm(20, 0, 1)
  images <- rep(1:20)
  new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2))))
  return(new.data)
}

#Create choices based on values adding noise for n number of subjects
#noise levels: 0.01, 0.05, 0.10, 0.25, 0.50
simulate.choice <- function(n, noise){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right")
  new.data$Choice.left.P <- 1/(1+exp(jitter(new.data$Value_right, amount = sqrt(3*noise)) - jitter(new.data$Value_left, amount = sqrt(3*noise))))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}


####################
#Simulation transitivity grid
####################


simulate.transitivity <- function(nsim, n, noise){
  choice.sims <- list()
  triplet.sims <- list()
  intransitive.sims <- list()
  names.choice.sims <- vector(nsim, mode = "character")
  names.triplet.sims <- vector(nsim, mode = "character")
  names.intransitive.sims <- vector(nsim, mode = "character")
  mean.intransitivity.percent <- vector(nsim, mode = "numeric")
  sd.intransitivity.percent <- vector(nsim, mode = "numeric")
  
  for(i in 1:nsim){
    choice.sims[[i]] <- simulate.choice(n = n, noise = noise)
    names.choice.sims[i] <- paste("sim.", noise,".",  i, sep = "")
    names(choice.sims)[[i]] <- names.choice.sims[i]
    
    triplet.sims[[i]] <- ddply(choice.sims[[i]], .(f.id), comb.fn.sim, .progress="text")
    names.triplet.sims[i] <- paste("triplets.sim.", noise,".", i, sep = "")
    names(triplet.sims)[[i]] <- names.triplet.sims[i]
    
    intransitive.sims[[i]] <- ddply(triplet.sims[[i]], .(f.id), summarise, 
                                    Intrans = sum(Intrans))
    names.intransitive.sims[i] <- paste("intransitive.sim.", noise,".", i, sep = "")
    names(intransitive.sims)[[i]] <- names.intransitive.sims[i]
    intransitive.sims[[i]]$PercentIntr <- intransitive.sims[[i]]$Intrans/1140*100
    
    mean.intransitivity.percent[i] <- mean(intransitive.sims[[i]]$PercentIntr)
    sd.intransitivity.percent[i] <- sd(intransitive.sims[[i]]$PercentIntr)
  }
  
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

sim.0.01 <- simulate.transitivity(1, 1000, 0.01)
save(sim.0.01, file = paste("sim.0.01.RData.", Sys.Date(), sep = ""))

sim.0.05 <- simulate.transitivity(1, 1000, 0.05)
save(sim.0.05, file = paste("sim.0.05.RData.", Sys.Date(), sep = ""))

sim.0.10 <- simulate.transitivity(1, 1000, 0.10)
save(sim.0.10, file = paste("sim.0.10.RData.", Sys.Date(), sep = ""))

sim.0.25 <- simulate.transitivity(1, 1000, 0.25)
save(sim.0.25, file = paste("sim.0.25.RData.", Sys.Date(), sep = ""))

sim.0.50 <- simulate.transitivity(1, 1000, 0.50)
save(sim.0.50, file = paste("sim.0.50.RData.", Sys.Date(), sep = ""))

sim.0.75 <- simulate.transitivity(1, 1000, 0.75)
save(sim.0.75, file = paste("sim.0.75.RData.", Sys.Date(), sep = ""))

sim.1.00 <- simulate.transitivity(1, 1000, 1.00)
save(sim.1.00, file = paste("sim.1.00.RData.", Sys.Date(), sep = ""))






