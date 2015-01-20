# Transitivty Dan Wall

library(dplyr)
library(plyr)

data_ind <- group_by(Data, f.id)

# So I need to make a function looks at an individual row of comb

comb.row.fn <- function (df, comb.row) {
  # INPUT 
  #   df: individual participants data
  #   comb.row: comb data frame split by row
  j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
  #Record choice for A vs B
  ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
  #Record if the trial was error (i.e. timed out)
  ErrApreftoB <- ifelse(df$RT[j] < 0 , 1, 0)
  #Find trial row in df that has the B vs C choice in comb
  k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for B vs C
  BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
  #Record if the trial was error (i.e. timed out)
  ErrBpreftoC <- ifelse(df$RT[k] <0 , 1, 0)
  #Find trial row in df that has the A vs C choice in comb
  l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for A vs C
  CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
  #Record if the trial was error (i.e. timed out)
  ErrCpreftoA <- ifelse(df$RT[l] <0 , 1, 0)
  #A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A (the second is the reverse)
  Intrans <- ifelse (ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, 
                             ifelse(ApreftoB == 0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
  #A triplet involves and Error if any of the three trials involved has timed out
  Err <- ifelse(ErrApreftoB == 1 | ErrBpreftoC == 1 | ErrCpreftoA == 1, 1, 0)
  #Mark triplets where there is both an error and an intransitivity
  ErrIntrans <- ifelse((ErrApreftoB == 1 | ErrBpreftoC == 1 | ErrCpreftoA == 1) & Intrans==1, 1, 0)
  # setup data to return
  ret.dat <- data.frame(A = comb.row$A, 
                        B = comb.row$B,
                        C = comb.row$C,
                        ApreftoB = ApreftoB,
                        BpreftoC = BpreftoC,
                        CpreftoA = CpreftoA,
                        Intrans = Intrans,
                        ErrApreftoB = ErrApreftoB,
                        ErrBpreftoC = ErrBpreftoC,
                        ErrCpreftoA = ErrCpreftoA,
                        Err = Err,
                        ErrIntrans = ErrIntrans)
  return(ret.dat)
}

comb <- mutate(comb, 
               id = 1:nrow(comb))


# ddply(comb, .(id), comb.row.fn, df = Data.cut)

comb.fn <- function(Data.cut, comb) {
  ddply(comb, .(id), comb.row.fn, df = Data.cut)
}

all <- ddply(Data, .(f.id), comb.fn, comb = comb, .progress="text")

sum <- ddply(all, .(f.id), summarise, 
             Intrans = sum(Intrans),
             Err = sum(Err), 
             ErrIntrans = sum(ErrIntrans))

# group_by(comb, id) %.%
#   summarise(comb.row.fn(df = Data.cut)
