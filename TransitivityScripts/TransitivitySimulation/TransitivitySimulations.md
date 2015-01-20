Transitivity Simulation
========================================================

Question
--------------------------------------------------------

I would like to know how to interpret the size of your main result
is 4.11% a large or small amount of intransitivity? 
One easy way to do this is to simulate a logistic choice process, 
with different amounts of noise, and then compute your statistic to see where it falls.

Answer
--------------------------------------------------------

If preferences were completely random there would be 25% intransitivity.

Simulations
--------------------------------------------------------

### Simulation Functions

#### Row level intransitive calculation (simplified)


```r
comb.row.fn.sim <- function(df, comb.row) {
    # INPUT df: individual participants data comb.row: comb data frame split by
    # row
    j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
    # Record choice for A vs B
    ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
    
    # Find trial row in df that has the B vs C choice in comb
    k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for B vs C
    BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
    
    # Find trial row in df that has the A vs C choice in comb
    l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for A vs C
    CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
    
    # A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A
    # (the second is the reverse)
    Intrans <- ifelse(ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, ifelse(ApreftoB == 
        0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
    
    # setup data to return
    ret.dat <- data.frame(A = comb.row$A, B = comb.row$B, C = comb.row$C, ApreftoB = ApreftoB, 
        BpreftoC = BpreftoC, CpreftoA = CpreftoA, Intrans = Intrans)
    return(ret.dat)
}
```


#### Apply row level intransitive calculation to each subject (simplified) 


```r
comb.fn.sim <- function(Data.cut) {
    images <- rep(1:20)
    comb.sim <- t(combn(images, 3))
    comb.sim <- as.data.frame(comb.sim)
    names(comb.sim) <- c("A", "B", "C")
    comb.sim$ApreftoB <- rep(NA, nrow(comb.sim))
    comb.sim$BpreftoC <- rep(NA, nrow(comb.sim))
    comb.sim$CpreftoA <- rep(NA, nrow(comb.sim))
    comb.sim$Intrans <- rep(NA, nrow(comb.sim))
    comb.sim <- mutate(comb.sim, id = 1:nrow(comb.sim))
    ddply(comb.sim, .(id), comb.row.fn.sim, df = Data.cut)
}
```


### Got 25%

#### Rbinom choice P


```r
create.new.data.rbinom <- function() {
    values <- rnorm(20, 0, 1)
    images <- rep(1:20)
    new.data <- as.data.frame(cbind(t(combn(values, 2)), t(combn(images, 2))))
    return(new.data)
}

simulate.choice.rbinom <- function(n) {
    require(plyr)
    new.data <- ldply(as.data.frame(replicate(n, create.new.data.rbinom())), 
        data.frame)
    names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", 
        "Image_right")
    
    new.data$Choice.left.P <- 1/(1 + exp(new.data$Value_right - new.data$Value_left))
    
    new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
    
    new.data$Choice.left1.right0.binom <- rbinom(nrow(new.data), 1, 0.5)
    
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.binom == 1, 
        new.data$Image_left, new.data$Image_right)
    
    return(new.data)
}

sim.rbinom <- simulate.choice.rbinom(n = 50)
triplets.sim.rbinom <- ddply(sim.rbinom, .(f.id), comb.fn.sim)
intransitive.sim.rbinom <- ddply(triplets.sim.rbinom, .(f.id), summarise, Intrans = sum(Intrans))
intransitive.sim.rbinom$PercentIntr <- intransitive.sim.rbinom$Intrans/1140 * 
    100
summary(intransitive.sim.rbinom$PercentIntr)

```


#### DEEP paper delta = N(0,1)


```r

create.new.data.deep <- function() {
    values <- rnorm(20, 0, 1)
    images <- rep(1:20)
    delta <- rnorm(190, 0, 1)
    new.data <- as.data.frame(cbind(t(combn(values, 2)), t(combn(images, 2)), 
        delta))
    return(new.data)
}

simulate.choice.deep <- function(n) {
    require(plyr)
    new.data <- ldply(as.data.frame(replicate(n, create.new.data.deep())), data.frame)
    names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", 
        "Image_right", "delta")
    
    new.data$Choice.left.P <- 1/(1 + exp(new.data$Value_right - new.data$Value_left))
    
    new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
    
    new.data$Choice.left.P.deep <- exp(new.data$delta * new.data$Value_left)/(exp(new.data$delta * 
        new.data$Value_left) + exp(new.data$delta * new.data$Value_right))
    
    new.data$Choice.left1.right0.deep <- ifelse(new.data$Choice.left.P.deep >= 
        0.5, 1, 0)
    
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.deep == 1, 
        new.data$Image_left, new.data$Image_right)
    
    return(new.data)
}

sim.deep <- simulate.choice.deep(n = 50)
triplets.sim.deep <- ddply(sim.deep, .(f.id), comb.fn.sim)
intransitive.sim.deep <- ddply(triplets.sim.deep, .(f.id), summarise, Intrans = sum(Intrans))
intransitive.sim.deep$PercentIntr <- intransitive.sim.deep$Intrans/1140 * 100
summary(intransitive.sim.deep$PercentIntr)

```


#### Add noise from rlogis(190, 0.5, 1) to choice P


```r

create.new.data.rlogis <- function() {
    values <- rnorm(20, 0, 1)
    images <- rep(1:20)
    noise.rlogis <- rlogis(190, 0.5, 1)
    new.data <- as.data.frame(cbind(t(combn(values, 2)), t(combn(images, 2)), 
        noise.rlogis))
    return(new.data)
}

simulate.choice.rlogis <- function(n, alpha) {
    require(plyr)
    new.data <- ldply(as.data.frame(replicate(n, create.new.data.rlogis())), 
        data.frame)
    names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", 
        "Image_right", "noise.rlogis")
    
    new.data$Choice.left.P <- 1/(1 + exp(new.data$Value_right - new.data$Value_left))
    
    new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
    
    new.data$Choice.left.P.rlogis <- new.data$Choice.left.P + alpha * new.data$noise.rlogis
    
    new.data$Choice.left1.right0.rlogis <- ifelse(new.data$Choice.left.P.rlogis >= 
        0.5, 1, 0)
    
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.rlogis == 1, 
        new.data$Image_left, new.data$Image_right)
    
    return(new.data)
}

sim.rlogis <- simulate.choice.rlogis(n = 50, alpha = 1)
triplets.sim.rlogis <- ddply(sim.rlogis, .(f.id), comb.fn.sim)
intransitive.sim.rlogis <- ddply(triplets.sim.rlogis, .(f.id), summarise, Intrans = sum(Intrans))
intransitive.sim.rlogis$PercentIntr <- intransitive.sim.rlogis$Intrans/1140 * 
    100
summary(intransitive.sim.rlogis$PercentIntr)


sim.rlogis.0.5 <- simulate.choice.rlogis(n = 50, alpha = 0.5)
triplets.sim.rlogis.0.5 <- ddply(sim.rlogis.0.5, .(f.id), comb.fn.sim)
intransitive.sim.rlogis.0.5 <- ddply(triplets.sim.rlogis.0.5, .(f.id), summarise, 
    Intrans = sum(Intrans))
intransitive.sim.rlogis.0.5$PercentIntr <- intransitive.sim.rlogis.0.5$Intrans/1140 * 
    100
summary(intransitive.sim.rlogis.0.5$PercentIntr)

sim.rlogis.2 <- simulate.choice.rlogis(n = 50, alpha = 2)
triplets.sim.rlogis.2 <- ddply(sim.rlogis.2, .(f.id), comb.fn.sim)
intransitive.sim.rlogis.2 <- ddply(triplets.sim.rlogis.2, .(f.id), summarise, 
    Intrans = sum(Intrans))
intransitive.sim.rlogis.2$PercentIntr <- intransitive.sim.rlogis.2$Intrans/1140 * 
    100
summary(intransitive.sim.rlogis.2$PercentIntr)
```


#### Add noise to values from N(0,1) at level alpha (THE RIGHT WAY CONTROLLING THE NOISE LEVEL)


```r

create.new.data <- function() {
    values <- rnorm(20, 0, 1)
    images <- rep(1:20)
    noise.right <- rnorm(190, 0, 1)
    noise.left <- rnorm(190, 0, 1)
    new.data <- as.data.frame(cbind(t(combn(values, 2)), t(combn(images, 2)), 
        noise.right, noise.left))
    return(new.data)
}

simulate.choice <- function(n, alpha) {
    require(plyr)
    new.data <- ldply(as.data.frame(replicate(n, create.new.data())), data.frame)
    names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", 
        "Image_right", "noise.left", "noise.right")
    
    new.data$Choice.left.P <- 1/(1 + exp(new.data$Value_right - new.data$Value_left))
    
    new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
    
    new.data$Choice.left.P.noise <- 1/(1 + exp(((1 - alpha) * new.data$Value_right + 
        (alpha * new.data$noise.right)) - ((1 - alpha) * new.data$Value_left + 
        (alpha * new.data$noise.left))))
    
    new.data$Choice.left1.right0.noise <- ifelse(new.data$Choice.left.P.noise >= 
        0.5, 1, 0)
    
    
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.noise == 1, 
        new.data$Image_left, new.data$Image_right)
    return(new.data)
}
```


### Plot intransitivity at different noise levels w 1000 simulations


```r

setwd("~/Dropbox/CDS/Transitivity/grid files")
library(ggplot2)

load("sim.0.01.n.2014-05-06.RData")
load("sim.0.05.n.2014-05-06.RData")
load("sim.0.10.n.2014-05-06.RData")
load("sim.0.25.n.2014-05-06.RData")
load("sim.0.50.n.2014-05-06.RData")
load("sim.0.75.n.2014-05-06.RData")
load("sim.1.00.n.2014-05-06.RData")

sim.0.01.PIntr <- sim.0.01.n$intransitive.sims$PercentIntr
sim.0.05.PIntr <- sim.0.05.n$intransitive.sims$PercentIntr
sim.0.10.PIntr <- sim.0.10.n$intransitive.sims$PercentIntr
sim.0.25.PIntr <- sim.0.25.n$intransitive.sims$PercentIntr
sim.0.50.PIntr <- sim.0.50.n$intransitive.sims$PercentIntr
sim.0.75.PIntr <- sim.0.75.n$intransitive.sims$PercentIntr
sim.1.00.PIntr <- sim.1.00.n$intransitive.sims$PercentIntr

PIntr <- data.frame(noise.level = c(rep(0.01, 1000), rep(0.05, 1000), rep(0.1, 
    1000), rep(0.25, 1000), rep(0.5, 1000), rep(0.75, 1000), rep(1, 1000)), 
    PercentIntr = c(sim.0.01.PIntr, sim.0.05.PIntr, sim.0.10.PIntr, sim.0.25.PIntr, 
        sim.0.50.PIntr, sim.0.75.PIntr, sim.1.00.PIntr))

ggplot(data = PIntr, aes(x = as.factor(noise.level), y = PercentIntr)) + geom_boxplot() + 
    theme_classic() + xlab("Noise level") + ylab("Percent of Intransitivities")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

