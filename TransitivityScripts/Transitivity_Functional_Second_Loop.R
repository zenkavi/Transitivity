# Transitivity Functional second loop

library(dplyr)
library(data.table)
comb2.row.fn <- function (df, comb2.row) {
  # INPUT 
  #   df: Data
  #   comb2.row: comb2 data frame split by row
  j <- which((df$Image_left == comb2.row$A | df$Image_right == comb2.row$A) &
               (df$Image_left == comb2.row$B | df$Image_right == comb2.row$B) &
               Data$Subject == comb2.row$Subject)
  #Record choice for A vs B
  ApreftoB <- ifelse(df$Choice.image[j] == comb2.row$A, 1, 0)
  #Record if the trial was error (i.e. timed out)
  ErrApreftoB <- ifelse(df$RT[j] < 0 , 1, 0)
  #Find trial row in df that has the B vs C choice in comb
  k <- which((df$Image_left == comb2.row$B | df$Image_right == comb2.row$B) &
               (df$Image_left == comb2.row$C | df$Image_right == comb2.row$C)&
               Data$Subject == comb2.row$Subject)
  #Record choice for B vs C
  BpreftoC <- ifelse(df$Choice.image[k] == comb2.row$B, 1, 0)
  #Record if the trial was error (i.e. timed out)
  ErrBpreftoC <- ifelse(df$RT[k] <0 , 1, 0)
  #Find trial row in df that has the A vs C choice in comb
  l <- which((df$Image_left == comb2.row$A | df$Image_right == comb2.row$A) &
               (df$Image_left == comb2.row$C | df$Image_right == comb2.row$C))
  #Record choice for A vs C
  CpreftoA <- ifelse(df$Choice.image[l] == comb2.row$C, 1, 0)
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
  ThirdTrial = max(df$Trialnumber[j], df$Trialnumber[j], df$Trialnumber[l])
  ret.dat <-  data.frame( Subject = comb2.row$Subject, 
                          Group = comb2.row$Group,
                          A = comb2.row$A, 
                          B = comb2.row$B,
                          C = comb2.row$C,
                          ApreftoB = ApreftoB,
                          BpreftoC = BpreftoC,
                          CpreftoA = CpreftoA,
                          Intrans = Intrans,
                          ErrApreftoB = ErrApreftoB,
                          ErrBpreftoC = ErrBpreftoC,
                          ErrCpreftoA = ErrCpreftoA,
                          Err = Err,
                          ErrIntrans = ErrIntrans,  
                          TrialA = df$Trialnumber[j],
                          TrialB = df$Trialnumber[k],
                          TrialC = df$Trialnumber[l],
                          ThirdTrial = ThirdTrial,
                          PasteA = df$Paste[j],
                          PasteB = df$Paste[k],
                          PasteC = df$Paste[l])
  return(ret.dat)
}

# colnames(comb2.row.fn(Data, comb2[1, ]))
# create row index
comb2$id <- 1:nrow(comb2)

# create a dplyr dataframe
comb2 <- tbl_df(comb2)

# apply the function to each row
bigone <- group_by(comb2, id) %.%
  do(comb2.row.fn, df = Data)


# combine list into one big data frame
bigone_df <- rbindlist(bigone)

