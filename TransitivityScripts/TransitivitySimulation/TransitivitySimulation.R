
#Rangel's Question:
####################

I would like to know how to interpret the size of your main result
is 4.11% a large or small amount of intransitivity? 
One easy way to do this is to simulate a logistic choice process, 
with different amounts of noise, and then compute your statistic to see where it falls.


####################
#Prep
####################

values <- rnorm(20, 0, 1)
images <- rep(1:20)
new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2))))

head(new.data)
names(new.data) <- c("Value_left", "Value_right", "Image_left", "Image_right")

# new.data$Choice.left.P <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
new.data$Choice.left.P <- 1/(1+exp(jitter(new.data$Value_right, amount = 0.5) - jitter(new.data$Value_left, amount = 0.5)))
# 
new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
# 
new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)

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
  noise.right <- rnorm(190, 0, 1)
  noise.left <-  rnorm(190, 0, 1)
  new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2)), noise.right, noise.left))
  return(new.data)
}

#Create choices based on values adding noise for n number of subjects
#noise levels: 0.01, 0.05, 0.10, 0.25, 0.50

# Add noise from uniform distribution
simulate.choice.uniform <- function(n, noise){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right")
  new.data$Choice.left.P <- 1/(1+exp(jitter(new.data$Value_right, amount = noise) - jitter(new.data$Value_left, amount = noise)))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}

# Add noise from normal distribution
simulate.choice <- function(n, alpha){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  new.data$Choice.left.P <- 1/(1+exp((new.data$Value_right + (alpha * new.data$noise.right)) - 
                                       (new.data$Value_left + (alpha * new.data$noise.left))))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}

sim.01.00.n <- simulate.choice(n = 50, alpha = 01.00)
triplets.sim.01.00.n <- ddply(sim.01.00.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.01.00.n <- ddply(triplets.sim.01.00.n, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.01.00.n$PercentIntr <- intransitive.sim.01.00.n$Intrans/1140*100

sim.02.00.n <- simulate.choice(n = 50, alpha = 02.00)
triplets.sim.02.00.n <- ddply(sim.02.00.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.02.00.n <- ddply(triplets.sim.02.00.n, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.02.00.n$PercentIntr <- intransitive.sim.02.00.n$Intrans/1140*100

sim.02.00.n$Pref.Change <- ifelse(sim.02.00.n$Choice.left1.right0 != sim.02.00.n$Choice.left1.right0.pure, 1, 0)
aggregate(Pref.Change ~ f.id, data = sim.02.00.n, FUN=sum.190)
#Change the pair preference a third of the time 
mean(aggregate(Pref.Change ~ f.id, data = sim.02.00.n, FUN=sum.190)$Pref.Change)

#What percent of noise was added to value?
sim.02.00.n$alpha.noise.right <- 2 * sim.02.00.n$noise.right
sim.02.00.n$alpha.noise.left <- 2 * sim.02.00.n$noise.left
sim.02.00.n$value.right.plus.alpha.noise <- sim.02.00.n$alpha.noise.right + sim.02.00.n$Value_right
sim.02.00.n$value.left.plus.alpha.noise <- sim.02.00.n$alpha.noise.left + sim.02.00.n$Value_left
sim.02.00.n$value.right.percent.change <- ((sim.02.00.n$Value_right - sim.02.00.n$value.right.plus.alpha.noise)/sim.02.00.n$Value_right)*100
sim.02.00.n$value.left.percent.change <- ((sim.02.00.n$Value_left - sim.02.00.n$value.left.plus.alpha.noise)/sim.02.00.n$Value_left)*100
sim.02.00.n$Choice.P.change.percent <- ((sim.02.00.n$Choice.left.P.pure - sim.02.00.n$Choice.left.P)/sim.02.00.n$Choice.left.P.pure)*100


sim.01.00.n$alpha.noise.right <- 1 * sim.01.00.n$noise.right
sim.01.00.n$alpha.noise.left <- 1 * sim.01.00.n$noise.left
sim.01.00.n$value.right.plus.alpha.noise <- sim.01.00.n$alpha.noise.right + sim.01.00.n$Value_right
sim.01.00.n$value.left.plus.alpha.noise <- sim.01.00.n$alpha.noise.left + sim.01.00.n$Value_left
sim.01.00.n$value.right.percent.change <- ((sim.01.00.n$Value_right - sim.01.00.n$value.right.plus.alpha.noise)/sim.01.00.n$Value_right)*100
sim.01.00.n$value.left.percent.change <- ((sim.01.00.n$Value_left - sim.01.00.n$value.left.plus.alpha.noise)/sim.01.00.n$Value_left)*100
sim.01.00.n$Choice.P.change.percent <- ((sim.01.00.n$Choice.left.P.pure - sim.01.00.n$Choice.left.P)/sim.01.00.n$Choice.left.P.pure)*100


sim.03.00.n <- simulate.choice(n = 50, alpha = 03.00)
triplets.sim.03.00.n <- ddply(sim.03.00.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.03.00.n <- ddply(triplets.sim.03.00.n, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.03.00.n$PercentIntr <- intransitive.sim.03.00.n$Intrans/1140*100

sim.03.00.n$alpha.noise.right <- 3 * sim.03.00.n$noise.right
sim.03.00.n$alpha.noise.left <- 3 * sim.03.00.n$noise.left
sim.03.00.n$value.right.plus.alpha.noise <- sim.03.00.n$alpha.noise.right + sim.03.00.n$Value_right
sim.03.00.n$value.left.plus.alpha.noise <- sim.03.00.n$alpha.noise.left + sim.03.00.n$Value_left
sim.03.00.n$value.right.percent.change <- ((sim.03.00.n$Value_right - sim.03.00.n$value.right.plus.alpha.noise)/sim.03.00.n$Value_right)*100
sim.03.00.n$value.left.percent.change <- ((sim.03.00.n$Value_left - sim.03.00.n$value.left.plus.alpha.noise)/sim.03.00.n$Value_left)*100
sim.03.00.n$Choice.P.change.percent <- ((sim.03.00.n$Choice.left.P.pure - sim.03.00.n$Choice.left.P)/sim.03.00.n$Choice.left.P.pure)*100


sim.04.00.n <- simulate.choice(n = 50, alpha = 04.00)
triplets.sim.04.00.n <- ddply(sim.04.00.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.04.00.n <- ddply(triplets.sim.04.00.n, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.04.00.n$PercentIntr <- intransitive.sim.04.00.n$Intrans/1140*100

sim.04.00.n$alpha.noise.right <- 4 * sim.04.00.n$noise.right
sim.04.00.n$alpha.noise.left <- 4 * sim.04.00.n$noise.left
sim.04.00.n$value.right.plus.alpha.noise <- sim.04.00.n$alpha.noise.right + sim.04.00.n$Value_right
sim.04.00.n$value.left.plus.alpha.noise <- sim.04.00.n$alpha.noise.left + sim.04.00.n$Value_left
sim.04.00.n$value.right.percent.change <- ((sim.04.00.n$Value_right - sim.04.00.n$value.right.plus.alpha.noise)/sim.04.00.n$Value_right)*100
sim.04.00.n$value.left.percent.change <- ((sim.04.00.n$Value_left - sim.04.00.n$value.left.plus.alpha.noise)/sim.04.00.n$Value_left)*100
sim.04.00.n$Choice.P.change.percent <- ((sim.04.00.n$Choice.left.P.pure - sim.04.00.n$Choice.left.P)/sim.04.00.n$Choice.left.P.pure)*100


#Calculate p differently
simulate.choice.deep <- function(n, alpha){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  new.data$Choice.left.P <- exp(alpha*(new.data$Value_left)) /(exp(alpha*(new.data$Value_right)) + exp(alpha*(new.data$Value_left)))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- exp(new.data$Value_left) /(exp(new.data$Value_right) + exp(new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}

#No change in choice p
sim.01.00.deep <- simulate.choice.deep(n = 50, alpha = 01.00)
triplets.sim.01.00.deep <- ddply(sim.01.00.deep, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.01.00.deep <- ddply(triplets.sim.01.00.deep, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.01.00.deep$PercentIntr <- intransitive.sim.01.00.deep$Intrans/1140*100

#All 50%
sim.00.00.deep <- simulate.choice.deep(n = 50, alpha = 00.00)
triplets.sim.00.00.deep <- ddply(sim.00.00.deep, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.00.00.deep <- ddply(triplets.sim.00.00.deep, .(f.id), summarise, 
                                     Intrans = sum(Intrans))
intransitive.sim.00.00.deep$PercentIntr <- intransitive.sim.00.00.deep$Intrans/1140*100


#Calculate p from noise only when alpha = 1
new.data <- create.new.data()
names(new.data) <- c("Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
new.data$Choice.left.P <- 1/(1+exp(new.data$noise.right - new.data$Value_left))
new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
triplets.test <- comb.fn.sim(new.data)
sum(triplets.test$Intrans)/1140*100

#Add noise from normal distribution to one side only (adding to both cancels out half? - no.)
simulate.choice.one <- function(n, alpha){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  new.data$Choice.left.P <- 1/(1+exp((new.data$Value_right + (alpha * new.data$noise.right)) - 
                                       (new.data$Value_left)))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}

# Practice one side noise
## Simulation for noise level 01.00 for 10 subjects
sim.01.00.n.one <- simulate.choice.one(n = 50, alpha = 01.00)
triplets.sim.01.00.n.one <- ddply(sim.01.00.n.one, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.01.00.n.one <- ddply(triplets.sim.01.00.n.one, .(f.id), summarise, 
                                  Intrans = sum(Intrans))
intransitive.sim.01.00.n.one$PercentIntr <- intransitive.sim.01.00.n.one$Intrans/1140*100

#Change noise consistently for an option
create.new.data.const.noise <- function(){
  values <- rnorm(20, 0, 1)
  images <- rep(1:20)
  noise <- rnorm(20, 0, 1)
  new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2)), t(combn(noise,2))))
  return(new.data)
}

simulate.choice.const.noise <- function(n, alpha){
  require(plyr)
  new.data <- ldply(as.data.frame(replicate(n, create.new.data.const.noise())),data.frame)
  names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  new.data$Choice.left.P <- 1/(1+exp((new.data$Value_right + (alpha * new.data$noise.right)) - 
                                       (new.data$Value_left + (alpha * new.data$noise.left))))
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  new.data$Choice.left.P.pure <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  new.data$Choice.left1.right0.pure <- round(new.data$Choice.left.P.pure)
  new.data$Choice.image <- ifelse(new.data$Choice.left1.right0 == 1, new.data$Image_left, new.data$Image_right)
  return(new.data)
}

sim.01.00.n.const.noise <- simulate.choice.const.noise(n = 50, alpha = 01.00)
triplets.sim.01.00.n.const.noise <- ddply(sim.01.00.n.const.noise, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.01.00.n.const.noise <- ddply(triplets.sim.01.00.n.const.noise, .(f.id), summarise, 
                                      Intrans = sum(Intrans))
intransitive.sim.01.00.n.const.noise$PercentIntr <- intransitive.sim.01.00.n.const.noise$Intrans/1140*100


####################
#Simulation practice normal dist
####################

triplets.test <- comb.fn.sim(new.data)

## Simulation for noise level 0.01 for 10 subjects
sim.0.01.n <- simulate.choice(n = 50, alpha = 0.01)
triplets.sim.0.01.n <- ddply(sim.0.01.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.01.n <- ddply(triplets.sim.0.01.n, .(f.id), summarise, 
                               Intrans = sum(Intrans))
intransitive.sim.0.01.n$PercentIntr <- intransitive.sim.0.01.n$Intrans/1140*100

## Simulation for noise level 0.05 for 10 subjects
sim.0.05.n <- simulate.choice(n = 50, alpha = 0.05)
triplets.sim.0.05.n <- ddply(sim.0.05.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.05.n <- ddply(triplets.sim.0.05.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.05.n$PercentIntr <- intransitive.sim.0.05.n$Intrans/1140*100

## Simulation for noise level 0.10 for 10 subjects
sim.0.10.n <- simulate.choice(n = 50, alpha = 0.10)
triplets.sim.0.10.n <- ddply(sim.0.10.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.10.n <- ddply(triplets.sim.0.10.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.10.n$PercentIntr <- intransitive.sim.0.10.n$Intrans/1140*100

## Simulation for noise level 0.25 for 10 subjects
sim.0.25.n <- simulate.choice(n = 50, alpha = 0.25)
triplets.sim.0.25.n <- ddply(sim.0.25.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.25.n <- ddply(triplets.sim.0.25.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.25.n$PercentIntr <- intransitive.sim.0.25.n$Intrans/1140*100

## Simulation for noise level 0.50 for 10 subjects
sim.0.50.n <- simulate.choice(n = 50, alpha = 0.50)
triplets.sim.0.50.n <- ddply(sim.0.50.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.50.n <- ddply(triplets.sim.0.50.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.50.n$PercentIntr <- intransitive.sim.0.50.n$Intrans/1140*100

## Simulation for noise level 0.75 for 10 subjects
sim.0.75.n <- simulate.choice(n = 50, alpha = 0.75)
triplets.sim.0.75.n <- ddply(sim.0.75.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.75.n <- ddply(triplets.sim.0.75.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.75.n$PercentIntr <- intransitive.sim.0.75.n$Intrans/1140*100

## Simulation for noise level 0.95 for 10 subjects
sim.0.95.n <- simulate.choice(n = 50, alpha = 0.95)
triplets.sim.0.95.n <- ddply(sim.0.95.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.0.95.n <- ddply(triplets.sim.0.95.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.0.95.n$PercentIntr <- intransitive.sim.0.95.n$Intrans/1140*100

## Simulation for noise level 01.00 for 10 subjects
sim.01.00.n <- simulate.choice(n = 50, alpha = 01.00)
triplets.sim.01.00.n <- ddply(sim.01.00.n, .(f.id), comb.fn.sim, .progress="text")
intransitive.sim.01.00.n <- ddply(triplets.sim.01.00.n, .(f.id), summarise, 
                                 Intrans = sum(Intrans))
intransitive.sim.01.00.n$PercentIntr <- intransitive.sim.01.00.n$Intrans/1140*100


data.frame(Noise.level = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 1.00),
           Mean.intransitivity.percent = c(mean(intransitive.sim.0.01.n$PercentIntr),
                                           mean(intransitive.sim.0.05.n$PercentIntr),
                                           mean(intransitive.sim.0.25.n$PercentIntr),
                                           mean(intransitive.sim.0.50.n$PercentIntr),
                                           mean(intransitive.sim.0.75.n$PercentIntr),
                                           mean(intransitive.sim.0.95.n$PercentIntr),
                                           mean(intransitive.sim.01.00.n$PercentIntr)))

plot.df <- data.frame(Noise.level = c(rep(0.01,50), rep(0.05, 50), rep(0.25, 50), rep(0.50, 50), rep(0.75, 50), rep(0.95, 50), rep(1.00, 50)),
                      Intransitivity.percent = c(intransitive.sim.0.01.n$PercentIntr,
                                                      intransitive.sim.0.05.n$PercentIntr,
                                                      intransitive.sim.0.25.n$PercentIntr,
                                                      intransitive.sim.0.50.n$PercentIntr,
                                                      intransitive.sim.0.75.n$PercentIntr,
                                                      intransitive.sim.0.95.n$PercentIntr,
                                                      intransitive.sim.01.00.n$PercentIntr))


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

test.sim <- simulate.transitivity(3, 4, 0.10)

load("test.sim.RData")


###################
# Post simulations
setwd("~/Dropbox/CDS/Transitivity/grid files")


mean.intransitivity.percent <- data.frame(Noise.level = c(0.01, 0.05, 0.25, 0.50, 0.75, 1.00),
                                          Mean.intransitivity.percent.u = rep(NA, 6),
                                          Mean.intransitivity.percent.n = rep(NA, 6))

load("sim.0.01.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 0.01] <- sim.0.01$mean.intransitivity.percent
rm(sim.0.01)

load("sim.0.05.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 0.05] <- sim.0.05$mean.intransitivity.percent
rm(sim.0.05)

load("sim.0.25.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 0.25] <- sim.0.25$mean.intransitivity.percent
rm(sim.0.25)

load("sim.0.50.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 0.50] <- sim.0.50$mean.intransitivity.percent
rm(sim.0.50)

load("sim.0.75.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 0.75] <- sim.0.75$mean.intransitivity.percent
rm(sim.0.75)

load("sim.1.00.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.u[mean.intransitivity.percent$Noise.level == 1.00] <- sim.1.00$mean.intransitivity.percent
rm(sim.1.00)

load("sim.0.01.n.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.n[mean.intransitivity.percent$Noise.level == 0.01] <- sim.0.01.n$mean.intransitivity.percent
rm(sim.0.01.n)

load("sim.0.25.n.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.n[mean.intransitivity.percent$Noise.level == 0.25] <- sim.0.25.n$mean.intransitivity.percent
rm(sim.0.25.n)

load("sim.0.50.n.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.n[mean.intransitivity.percent$Noise.level == 0.50] <- sim.0.50.n$mean.intransitivity.percent
rm(sim.0.50.n)

load("sim.0.75.n.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.n[mean.intransitivity.percent$Noise.level == 0.75] <- sim.0.75.n$mean.intransitivity.percent
rm(sim.0.75.n)

load("sim.1.00.n.2014-04-28.RData")
mean.intransitivity.percent$Mean.intransitivity.percent.n[mean.intransitivity.percent$Noise.level == 1.00] <- sim.1.00.n$mean.intransitivity.percent
rm(sim.1.00.n)
