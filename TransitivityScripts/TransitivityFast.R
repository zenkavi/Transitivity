### Organization code

# Scroll below to descriptive if not interested.

# Read Data in:

######### Read Data in
setwd("~/Dropbox/CDS/Transitivity/grid files")
Data <- read.csv("complete_data.csv")
TrialNumber <- read.csv("TrialNumbers.csv")
options(digits=3)
# names(Data)

# install.packages(c("lme4", "ggplot2", "reshape"))
library(lme4)
library(ggplot2)
library(reshape)

# Organize Data

######### Organize Data
#Create variables
Data$Choice <- ifelse(Data$Choice_1left_4right == 1, "left", "right")

Data$Choice.image <- ifelse(Data$Choice_1left_4right == 1, Data$Image_left, Data$Image_right)

Data$Choice.left1.right0 <- ifelse(Data$Choice_1left_4right == 1, 1, 0)

Data$Group <- ifelse(Data$Group == 1, "MTL", 
                     ifelse(Data$Group == 2, "ETP","C"))

Data$f.id <- as.factor(Data$SubjectID)

TrialNumber$Image_right <- ifelse(TrialNumber$Image_right <10, paste(0,as.character(TrialNumber$Image_right,sep="")), TrialNumber$Image_right)
TrialNumber$Image_left <- ifelse(TrialNumber$Image_left <10, paste(0,as.character(TrialNumber$Image_left,sep="")), TrialNumber$Image_left)
TrialNumber$Paste <- paste(TrialNumber$Image_right, TrialNumber$Image_left, sep="")
TrialNumber$Paste<- gsub(" ", "", TrialNumber$Paste)

Data$Image_right <- ifelse(Data$Image_right <10, paste(0,as.character(Data$Image_right,sep="")), Data$Image_right)
Data$Image_left <- ifelse(Data$Image_left <10, paste(0,as.character(Data$Image_left,sep="")), Data$Image_left)
Data$Paste <- paste(Data$Image_right, Data$Image_left, sep="")
Data$Paste<- gsub(" ", "", Data$Paste)

Data2 <- read.csv("complete_data.csv")
Data$Image_right <- Data2$Image_right
Data$Image_left <- Data2$Image_left
rm(Data2)

Data$Trial<- rep(NA, nrow(Data))
for (i in 1:nrow(Data)){
  Data$Trial[i]<- TrialNumber$Trial[which(Data$Paste[i] == TrialNumber$Paste)] 
}

# Count intransitive choices

# Corrected on 11.14.13 to include reverse transitivity as well.
# Includes counting error trials (trials where participant took too long to respond) 12.01.13

#Count intransitive choices
#Create matrix to store intransitivity data on subject level
# Intransitive <- as.data.frame(matrix(NA, nrow=length(unique(Data$f.id)), ncol = 5))
#Columns are: subject, number of missed trials, number of intransitive trials, number of triplets with error, number of triplets with intransitivity and error
# names(Intransitive) <- c("Subject", "Missed", "NumIntr", "Err1140", "IntrErr")
#Assign subject numbers to subject column
# Intransitive$Subject <- unique(Data$f.id)
#Tabulate the number of trials where trial timed out
#Creates df with subject id in col 1 and number of timed out trials in col 2
Error <- as.data.frame(table(Data[which(Data$RT<0), "SubjectID"]))
#Assign number of missed trials from the Error df to the correct subject in Intrans df
# for (i in 1:nrow(Error)){
#   j <- which(as.character(Intransitive$Subject) == as.character(Error$Var1[i]))
#   Intransitive$Missed[j] <- Error$Freq[i]
# }
#Or give 0 if there are no missed trials
# Intransitive$Missed <- ifelse(is.na(Intransitive$Missed) == T, 0, Intransitive$Missed)

#Create temp df where intransitive choices will be counted by creating the 1140 triplets possible out of 20 options (C(20,3) = 1140)
comb <- t(combn(unique(Data$Image_left),3))
comb <- as.data.frame(comb)
names(comb) <- c("A", "B", "C")
comb$ApreftoB <- rep(NA, nrow(comb))
comb$BpreftoC <- rep(NA, nrow(comb))
comb$CpreftoA <- rep(NA, nrow(comb))
comb$Intrans <- rep(NA, nrow(comb))
comb$ErrApreftoB <- rep(NA, nrow(comb))
comb$ErrBpreftoC <- rep(NA, nrow(comb))
comb$ErrCpreftoA <- rep(NA, nrow(comb))
comb$Err <- rep(NA, nrow(comb))
comb$ErrIntrans <- rep(NA, nrow(comb))
comb$TrialA <- rep(NA, nrow(comb))
comb$TrialB <- rep(NA, nrow(comb))
comb$TrialC <- rep(NA, nrow(comb))
comb$ThirdTrial <- rep(NA, nrow(comb))
comb$PasteA <- rep(NA, nrow(comb))
comb$PasteB <- rep(NA, nrow(comb))
comb$PasteC <- rep(NA, nrow(comb))

#Functions to calculate intransitives

library(dplyr)
library(plyr)

data_ind <- group_by(Data, f.id)

# Function 1: Check intransitivity on one row of comb (in one triplet)
# Output: Record choice, intransitivity, error, error+intransitivity, trial number for each pair, last trial of triplet, pair identifier in one row

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
  TrialA <- df$Trialnumber[j]
  TrialB <- df$Trialnumber[k]
  TrialC <- df$Trialnumber[l]
  ThirdTrial <- max(TrialA, TrialB, TrialC)
  PasteA <- Data$Paste[j]
  PasteB <- Data$Paste[k]
  PasteC <- Data$Paste[l]
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
                        ErrIntrans = ErrIntrans,
                        TrialA = TrialA,
                        TrialB = TrialB,
                        TrialC = TrialC,
                        ThirdTrial = ThirdTrial,
                        PasteA = PasteA,
                        PasteB = PasteB,
                        PasteC = PasteC)
  return(ret.dat)
}

# Create continuous triplet variable
comb <- mutate(comb, 
               id = 1:nrow(comb))

# Function 2: Apply function 1 (row level intransitivity calculation) to subset of data including all triplets for one participant
comb.fn <- function(Data.cut, comb) {
  ddply(comb, .(id), comb.row.fn, df = Data.cut)
}

# Combine intransitivity calculation for all participant by applying to subject level calculation to all participants
# Output: 1140*number of subject rows recording with output of comb.row.fn function
all <- ddply(Data, .(f.id, Group), comb.fn, comb = comb, .progress="text")

# Summarize triplet level intransitivity on subject level 
# Output: nrow = number of subjects
Intransitive <- ddply(all, .(f.id, Group), summarise, 
             Intrans = sum(Intrans),
             Err = sum(Err), 
             ErrIntrans = sum(ErrIntrans))

# Create additional subject level variables and calculations of percentages
Intransitive$Missed <- rep(NA, nrow(Intransitive))

#Assign number of missed trials from the Error df to the correct subject in Intrans df
for (i in 1:nrow(Error)){
  j <- which(as.character(Intransitive$f.id) == as.character(Error$Var1[i]))
  Intransitive$Missed[j] <- Error$Freq[i]
}
#Or give 0 if there are no missed trials
Intransitive$Missed <- ifelse(is.na(Intransitive$Missed) == T, 0, Intransitive$Missed)

Intransitive$PercentMissed <- Intransitive$Missed/190*100
Intransitive$PercentIntr <- Intransitive$Intrans/1140*100
Intransitive$PercentMissedTrialsAffect <- Intransitive$Err/1140*100
#Percent of intransitivies that include an error
Intransitive$PIntrError<- Intransitive$ErrIntrans/ Intransitive$Intrans *100

# Create pair level intransitivity count

# Temp df before aggregating. Has all three "Paste"s (ie trials in one column)
all.m <- melt(all[, c("f.id", "Group", "Intrans", "Err", "ErrIntrans","TrialA", "TrialB", "TrialC")], id = c("f.id", "Group", "Intrans", "Err", "ErrIntrans"))

# Aggregate df to sum (I think Err and ErrIntrans are less usefull here. Using only Intrans so far.)
# Note: Total intransitivities will be triple counted this way (because all three pairs that are involved in an intransitivity take the dummy variable 1)

all.ag <- aggregate(all.m[,c("Intrans","Err", "ErrIntrans")], by = list(f.id = all.m$f.id, Group = all.m$Group, Trial = all.m$value), FUN = "sum")

# Merge Intransitivity info with rest of trial level data

all.data <- merge(Data, all.ag, by = c("f.id", "Group", "Trial"))

# Create centered trial number and quadratic trial term

all.data$c.Trialnumber <- all.data$Trialnumber - mean(all.data$Trialnumber)
all.data$c.TrialQuad <- all.data$c.Trialnumber^2

save.image("./TransitivityFastWorkSpace020114.RData")