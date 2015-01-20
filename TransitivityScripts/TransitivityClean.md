Transitivity - clean
========================================================




Organization code
--------------------------------------------------------

Scroll below to descriptives if not interested.

### Read Data in:


```r
######### Read Data in
setwd("~/Dropbox/CDS/Transitivity/grid files")
Data <- read.csv("complete_data.csv")
TrialNumber <- read.csv("TrialNumbers.csv")
options(digits = 3)
# names(Data)
```


### Organize Data 

#### Code choice

Code wrong button presses as left (This changed on 4/18/14. Before the default coding was right. Changed it because the most of the mispresses are "2" so more likely to have been meant as "1" i.e. left hence even the wrong count would give a closer number. Regardless this is accounted for later.)


```r
Data$Choice <- ifelse(Data$Choice_1left_4right == 4, "left", "right")

Data$Choice.image <- ifelse(Data$Choice_1left_4right == 4, Data$Image_right, 
    Data$Image_left)

Data$Choice.left1.right0 <- ifelse(Data$Choice_1left_4right == 4, 0, 1)

# Data$Choice <- ifelse(Data$Choice_1left_4right == 1, 'left', 'right')
# Data$Choice.image <- ifelse(Data$Choice_1left_4right == 1,
# Data$Image_left, Data$Image_right) Data$Choice.left1.right0 <-
# ifelse(Data$Choice_1left_4right == 1, 1, 0)
```


#### Assign groups


```r
Data$Group <- ifelse(Data$Group == 1, "MTL", ifelse(Data$Group == 2, "ETP", 
    "C"))
```


#### Factorize id's


```r
Data$f.id <- as.factor(Data$SubjectID)
```


#### Create unique identifier for each pair (Paste variable)


```r
TrialNumber$Image_right <- ifelse(TrialNumber$Image_right < 10, paste(0, as.character(TrialNumber$Image_right, 
    sep = "")), TrialNumber$Image_right)
TrialNumber$Image_left <- ifelse(TrialNumber$Image_left < 10, paste(0, as.character(TrialNumber$Image_left, 
    sep = "")), TrialNumber$Image_left)
TrialNumber$Paste <- paste(TrialNumber$Image_right, TrialNumber$Image_left, 
    sep = "")
TrialNumber$Paste <- gsub(" ", "", TrialNumber$Paste)

Data$Image_right <- ifelse(Data$Image_right < 10, paste(0, as.character(Data$Image_right, 
    sep = "")), Data$Image_right)
Data$Image_left <- ifelse(Data$Image_left < 10, paste(0, as.character(Data$Image_left, 
    sep = "")), Data$Image_left)
Data$Paste <- paste(Data$Image_right, Data$Image_left, sep = "")
Data$Paste <- gsub(" ", "", Data$Paste)

Data2 <- read.csv("complete_data.csv")
Data$Image_right <- Data2$Image_right
Data$Image_left <- Data2$Image_left
rm(Data2)

Data$Trial <- rep(NA, nrow(Data))
for (i in 1:nrow(Data)) {
    Data$Trial[i] <- TrialNumber$Trial[which(Data$Paste[i] == TrialNumber$Paste)]
}
```


#### Create Error df with counts of number of trials subjects mispressed or timed out

This will be merged with the Intransitive df with individual intransitivity counts later. 

Note: Mispress doesn't include timeout trials.


```r
# Creates df with subject id in col 1 and number of timed out trials in col
# 2
TimeOut <- as.data.frame(table(Data[which(Data$RT < 0), "SubjectID"]))

# Creates df with subject id in col 1 and number of mispressed trials in col
# 2
MisPress <- as.data.frame(table(Data[which(Data$Choice_1left_4right != 1 & Data$Choice_1left_4right != 
    4 & Data$Choice_1left_4right != (-1)), "SubjectID"]))

# Creates df with subject id in col 1 and number of both kinds of errors in
# col 2
EitherError <- as.data.frame(table(Data[which(Data$RT < 0 | (Data$Choice_1left_4right != 
    1 & Data$Choice_1left_4right != 4 & Data$Choice_1left_4right != (-1))), 
    "SubjectID"]))

# Merge above three dfs to get numbers of all errors
Error <- merge(MisPress, TimeOut, by = "Var1", all = T)
Error <- merge(Error, EitherError, by = "Var1", all = T)
names(Error) <- c("f.id", "MisPress", "TimeOut", "EitherError")
rm(TimeOut, MisPress, EitherError)
# TO BE MERGED TO INTRANSITIVE DF LATER

```


### Count intransitive choices

11.14.13 Corrected to include reverse transitivity as well.  
12.01.13 Includes counting error trials (trials where participant took too long to respond)  
01.30.14 Made more efficient creating functions and using dplyr
03.19.14 Trialnumber vs Trial corrected in all.ag and merge
04.17.14 Accounting for mispressed buttons (other than 1 and 4)

#### Create temp df where intransitive choices will be counted by creating the 1140 triplets possible out of 20 options (C(20,3) = 1140)


```r
comb <- t(combn(unique(Data$Image_left), 3))
comb <- as.data.frame(comb)
names(comb) <- c("A", "B", "C")
comb$ApreftoB <- rep(NA, nrow(comb))
comb$BpreftoC <- rep(NA, nrow(comb))
comb$CpreftoA <- rep(NA, nrow(comb))
comb$Intrans <- rep(NA, nrow(comb))
comb$TimeOutApreftoB <- rep(NA, nrow(comb))
comb$TimeOutBpreftoC <- rep(NA, nrow(comb))
comb$TimeOutCpreftoA <- rep(NA, nrow(comb))
comb$TimeOut <- rep(NA, nrow(comb))
comb$TimeOutIntrans <- rep(NA, nrow(comb))
comb$MisPressApreftoB <- rep(NA, nrow(comb))
comb$MisPressBpreftoC <- rep(NA, nrow(comb))
comb$MisPressCpreftoA <- rep(NA, nrow(comb))
comb$MisPress <- rep(NA, nrow(comb))
comb$MisPressIntrans <- rep(NA, nrow(comb))
comb$Error <- rep(NA, nrow(comb))
comb$ErrorIntrans <- rep(NA, nrow(comb))
comb$TrialA <- rep(NA, nrow(comb))
comb$TrialB <- rep(NA, nrow(comb))
comb$TrialC <- rep(NA, nrow(comb))
comb$ThirdTrial <- rep(NA, nrow(comb))
comb$PasteA <- rep(NA, nrow(comb))
comb$PasteB <- rep(NA, nrow(comb))
comb$PasteC <- rep(NA, nrow(comb))
```


#### Group data by subjects 

To later apply the function to count intransitivites to each subject.


```r
data_ind <- group_by(Data, f.id)
```


#### Function 1: Check intransitivity on one row of comb (in one triplet)

Output: Record choice, intransitivity, TimeOut, MisPress, EitherError, TimeOut+intransitivity, MissPress+intransitivity, EitherError+intransitivity, trial number for each pair, last trial of triplet, pair identifier in one row


```r
comb.row.fn <- function(df, comb.row) {
    # INPUT df: individual participants data comb.row: comb data frame split by
    # row
    j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
    # Record choice for A vs B
    ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
    # Record if the trial was TimeOut (i.e. timed out)
    TimeOutApreftoB <- ifelse(df$RT[j] < 0, 1, 0)
    # Record if the trial was MisPressed
    MisPressApreftoB <- ifelse(df$Choice_1left_4right[j] != 1 & df$Choice_1left_4right[j] != 
        4 & df$Choice_1left_4right[j] != (-1), 1, 0)
    # Find trial row in df that has the B vs C choice in comb
    k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for B vs C
    BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
    # Record if the trial was TimeOutor (i.e. timed out)
    TimeOutBpreftoC <- ifelse(df$RT[k] < 0, 1, 0)
    # Record if the trial was MisPressed
    MisPressBpreftoC <- ifelse(df$Choice_1left_4right[k] != 1 & df$Choice_1left_4right[k] != 
        4 & df$Choice_1left_4right[k] != (-1), 1, 0)
    # Find trial row in df that has the A vs C choice in comb
    l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for A vs C
    CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
    # Record if the trial was TimeOutor (i.e. timed out)
    TimeOutCpreftoA <- ifelse(df$RT[l] < 0, 1, 0)
    # Record if the trial was MisPressed
    MisPressCpreftoA <- ifelse(df$Choice_1left_4right[l] != 1 & df$Choice_1left_4right[l] != 
        4 & df$Choice_1left_4right[l] != (-1), 1, 0)
    # A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A
    # (the second is the reverse)
    Intrans <- ifelse(ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, ifelse(ApreftoB == 
        0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
    # A triplet involves a TimeOut if any of the three trials involved has timed
    # out
    TimeOut <- ifelse(TimeOutApreftoB == 1 | TimeOutBpreftoC == 1 | TimeOutCpreftoA == 
        1, 1, 0)
    # A triplet involves a MisPress if any of the three trials were mispressed
    MisPress <- ifelse(MisPressApreftoB == 1 | MisPressBpreftoC == 1 | MisPressCpreftoA == 
        1, 1, 0)
    # A triplet involves an Error if any of the three trials were timed out or
    # mispressed
    Error <- ifelse(TimeOutApreftoB == 1 | TimeOutBpreftoC == 1 | TimeOutCpreftoA == 
        1 | MisPressApreftoB == 1 | MisPressBpreftoC == 1 | MisPressCpreftoA == 
        1, 1, 0)
    # Mark triplets where there is both an TimeOut and an intransitivity
    TimeOutIntrans <- ifelse((TimeOutApreftoB == 1 | TimeOutBpreftoC == 1 | 
        TimeOutCpreftoA == 1) & Intrans == 1, 1, 0)
    # Mark triplets where there is both an MisPress and an intransitivity
    MisPressIntrans <- ifelse((MisPressApreftoB == 1 | MisPressBpreftoC == 1 | 
        MisPressCpreftoA == 1) & Intrans == 1, 1, 0)
    # Mark triplets where there is both an TimeOut or Mispress and an
    # intransitivity
    ErrorIntrans <- ifelse((TimeOutApreftoB == 1 | TimeOutBpreftoC == 1 | TimeOutCpreftoA == 
        1 | MisPressApreftoB == 1 | MisPressBpreftoC == 1 | MisPressCpreftoA == 
        1) & Intrans == 1, 1, 0)
    TrialA <- df$Trialnumber[j]
    TrialB <- df$Trialnumber[k]
    TrialC <- df$Trialnumber[l]
    ThirdTrial <- max(TrialA, TrialB, TrialC)
    PasteA <- Data$Paste[j]
    PasteB <- Data$Paste[k]
    PasteC <- Data$Paste[l]
    # setup data to return
    ret.dat <- data.frame(A = comb.row$A, B = comb.row$B, C = comb.row$C, ApreftoB = ApreftoB, 
        BpreftoC = BpreftoC, CpreftoA = CpreftoA, Intrans = Intrans, TimeOutApreftoB = TimeOutApreftoB, 
        TimeOutBpreftoC = TimeOutBpreftoC, TimeOutCpreftoA = TimeOutCpreftoA, 
        TimeOut = TimeOut, TimeOutIntrans = TimeOutIntrans, MisPressApreftoB = MisPressApreftoB, 
        MisPressBpreftoC = MisPressBpreftoC, MisPressCpreftoA = MisPressCpreftoA, 
        MisPress = MisPress, MisPressIntrans = MisPressIntrans, Error = Error, 
        ErrorIntrans = ErrorIntrans, TrialA = TrialA, TrialB = TrialB, TrialC = TrialC, 
        ThirdTrial = ThirdTrial, PasteA = PasteA, PasteB = PasteB, PasteC = PasteC)
    return(ret.dat)
}
```


#### Create continuous triplet variable 

To act as levels when applying the row level function to the subject level df


```r
comb <- mutate(comb, id = 1:nrow(comb))
```


#### Function 2: Apply function 1 (row level intransitivity calculation) to subset of data including all triplets for one participant


```r
comb.fn <- function(Data.cut, comb) {
    ddply(comb, .(id), comb.row.fn, df = Data.cut)
}
```


#### Count intransitivities for all subjects [takes time ~ 15 min]

Combine intransitivity calculation for all participant by applying to subject level calculation to all participants  
Output: 1140*number of subject rows recording with output of comb.row.fn function


```r
all <- ddply(Data, .(f.id, Group), comb.fn, comb = comb, .progress = "text")
```


#### Subject level: Summarize triplet level intransitivity on subject level 

Output: nrow = number of subjects  
Note TimeOut, MisPress and Error are large numbers (not matching the Error df) because they are the number of triplets (not trials!) that involve one OR MORE of these errors (so not triple counting)


```r
Intransitive <- ddply(all, .(f.id, Group), summarise, Intrans = sum(Intrans), 
    TimeOutTriplets = sum(TimeOut), MisPressTriplets = sum(MisPress), ErrorTriplets = sum(Error), 
    TimeOutIntransTriplets = sum(TimeOutIntrans), MisPressIntransTriplets = sum(MisPressIntrans), 
    ErrorIntransTriplets = sum(ErrorIntrans))
```


#### Subject level: Assign number of missed trials from the Error df to the correct subject in Intrans df


```r
# Create additional subject level variables and calculations of percentages
Intransitive$TimeOutTrials <- rep(NA, nrow(Intransitive))
Intransitive$MisPressTrials <- rep(NA, nrow(Intransitive))
Intransitive$EitherErrorTrials <- rep(NA, nrow(Intransitive))

# Assign number of missed trials from the Error df to the correct subject in
# Intrans df
for (i in 1:nrow(Error)) {
    j <- which(as.character(Intransitive$f.id) == as.character(Error$f.id[i]))
    Intransitive$TimeOutTrials[j] <- Error$TimeOut[i]
    Intransitive$MisPressTrials[j] <- Error$MisPress[i]
    Intransitive$EitherErrorTrials[j] <- Error$EitherError[i]
}

# Or give 0 if there are no missed trials
Intransitive$TimeOutTrials <- ifelse(is.na(Intransitive$TimeOutTrials) == T, 
    0, Intransitive$TimeOutTrials)
Intransitive$MisPressTrials <- ifelse(is.na(Intransitive$MisPressTrials) == 
    T, 0, Intransitive$MisPressTrials)
Intransitive$EitherErrorTrials <- ifelse(is.na(Intransitive$EitherErrorTrials) == 
    T, 0, Intransitive$EitherErrorTrials)
```


#### Subject level: Calculate percentages of intransitivities and timed out/mispressed trials



```r
# Trial level calculations
Intransitive$PercentTimeOutTrials <- Intransitive$TimeOutTrials/190 * 100
Intransitive$PercentMisPressTrials <- Intransitive$MisPressTrials/190 * 100
Intransitive$PercentEitherErrorTrials <- Intransitive$EitherErrorTrials/190 * 
    100

# Triplet level calculations
Intransitive$PercentIntr <- Intransitive$Intrans/1140 * 100
Intransitive$PercentTimeOutTriplets <- Intransitive$TimeOutTriplets/1140 * 100
Intransitive$PercentMisPressTriplets <- Intransitive$MisPressTriplets/1140 * 
    100
Intransitive$PercentEitherErrorTriplets <- Intransitive$ErrorTriplets/1140 * 
    100

# Percent of intransitivies that include an error
Intransitive$PIntrError <- Intransitive$ErrorIntrans/Intransitive$Intrans * 
    100

Intransitive$CleanTriplets <- 1140 - Intransitive$ErrorTriplets
Intransitive$CleanIntr <- Intransitive$Intrans - Intransitive$ErrorIntransTriplets
Intransitive$CleanPercentIntr <- Intransitive$CleanIntr/Intransitive$CleanTriplets * 
    100
```


#### Trial level: Create pair level intransitivity count

**Temp df before aggregating. Has all three "Paste"s (ie trials in one column)**


```r
all.m <- melt(all[, c("f.id", "Group", "Intrans", "TimeOut", "TimeOutIntrans", 
    "MisPress", "MisPressIntrans", "Error", "ErrorIntrans", "TrialA", "TrialB", 
    "TrialC")], id = c("f.id", "Group", "Intrans", "TimeOut", "TimeOutIntrans", 
    "MisPress", "MisPressIntrans", "Error", "ErrorIntrans"))
```


**Aggregate df to sum (I think Err and ErrIntrans are less usefull here. Using only Intrans so far.) Note: Total intransitivities will be triple counted this way (because all three pairs that are involved in an intransitivity take the dummy variable 1)**


```r
all.ag <- aggregate(all.m[, c("Intrans", "TimeOut", "TimeOutIntrans", "MisPress", 
    "MisPressIntrans", "Error", "ErrorIntrans")], by = list(f.id = all.m$f.id, 
    Group = all.m$Group, Trialnumber = all.m$value), FUN = "sum")
```


**Merge Intransitivity info with rest of trial level data**


```r
all.data <- merge(Data, all.ag, by = c("f.id", "Group", "Trialnumber"))

rm(all.ag, all.m)
```


#### Trial level: Create centered trial number and quadratic trial term


```r
all.data$c.Trialnumber <- all.data$Trialnumber - mean(all.data$Trialnumber)
all.data$c.TrialQuad <- all.data$c.Trialnumber^2
```


#### Trial level: Create df counting how many times each trial was the last trial (C>A) leading to intransitivity


```r

Trialnumber.df <- as.data.frame(table(all$ThirdTrial, all$Intrans, all$f.id))

# head(Trialnumber.df, 190)

names(Trialnumber.df) <- c("Trialnumber", "Intrans", "f.id", "TimesThirdInIntrans")
```


**Exclude those where there is no intransitivity**


```r
Trialnumber.df <- Trialnumber.df[Trialnumber.df$Intrans == 1, ]
```


**Organize classes of columns**


```r

Trialnumber.df$Trialnumber <- as.numeric(as.character(Trialnumber.df$Trialnumber))
Trialnumber.df$Intrans <- as.numeric(as.character(Trialnumber.df$Intrans))
```


**Merge w pair level data**


```r

all.data <- merge(all.data, Trialnumber.df[, c("Trialnumber", "f.id", "TimesThirdInIntrans")], 
    by = c("Trialnumber", "f.id"), all.x = T)

all.data <- all.data[order(all.data$f.id, all.data$Trialnumber), ]
```


**Replace NA's in IntransFreq w 0's (these trials have never been the largest trial in a triplet)**


```r
all.data$IntransFreq <- ifelse(is.na(all.data$TimesThirdInIntrans), 0, all.data$TimesThirdInIntrans)
```


#### Trial level: variables that are triple counted in trial level df all.data - rename them to avoid confusion later

**Note also these are not triple counted in Intransitive df because that is summarized directly from triplet level**


```r
names(all.data)[which(names(all.data) == "Intrans")] <- "IntransTripleCounted"
names(all.data)[which(names(all.data) == "TimeOut")] <- "TimeOutTripletsTripleCounted"
names(all.data)[which(names(all.data) == "MisPress")] <- "MisPressTripletsTripleCounted"
names(all.data)[which(names(all.data) == "Error")] <- "ErrorTripletsTripleCounted"
names(all.data)[which(names(all.data) == "TimeOutIntrans")] <- "TimeOutIntransTripleCounted"
names(all.data)[which(names(all.data) == "MisPressIntrans")] <- "MisPressIntransTripleCounted"
names(all.data)[which(names(all.data) == "ErrorIntrans")] <- "ErrorIntransTripleCounted"
```


Cleaning Timed out and Mispressed trials
--------------------------------------

### Trial level error indicators


```r
all.data$TimeOut.1.0 <- ifelse(all.data$RT < 0, 1, 0)
all.data$MisPress.1.0 <- ifelse(all.data$Choice_1left_4right != 1 & all.data$Choice_1left_4right != 
    4 & all.data$Choice_1left_4right != (-1), 1, 0)
all.data$Error.1.0 <- ifelse(all.data$TimeOut.1.0 == 1 | all.data$MisPress.1.0 == 
    1, 1, 0)
```


### Time out trials

There are trials where RT<0.  

Participants from all groups have timed out trials.


```r
table(Intransitive$Group[Intransitive$TimeOutTrials > 0])
```

```
## 
##   C ETP MTL 
##  16  16  25
```


They constitute 1.19 % of all trials.  

67.5 % of these timed out trials are in the MTL group.


```r
sum(all.data$TimeOut.1.0)/nrow(all.data) * 100
```

```
## [1] 1.19
```

```r

prop.table(table(all.data$TimeOut.1.0, all.data$Group), 1)[2, ]
```

```
##     C   ETP   MTL 
## 0.107 0.218 0.675
```


#### Average number of timed out trials for each group:


```r
aggregate(TimeOutTrials ~ Group, data = Intransitive, FUN = mean)
```

```
##   Group TimeOutTrials
## 1     C         0.733
## 2   ETP         1.500
## 3   MTL         4.484
```

```r

ggplot(data = Intransitive, aes(x = Group, y = TimeOutTrials)) + geom_boxplot() + 
    theme_classic()
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 


**Are there group differences in number of timed out trials? No. (but looking at the boxplot these are hidden by the outlier)**


```r
summary(aov(TimeOutTrials ~ Group, data = Intransitive))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## Group        2    241   120.3    2.44  0.093 .
## Residuals   88   4337    49.3                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


**How much of the timed out trials is the outlier (8146) responsible for? About a third of all time out trials.** 


```r
Intransitive$TimeOutTrials[Intransitive$f.id == 8146]/sum(Intransitive$TimeOutTrials)
```

```
## [1] 0.32
```


**This participant also has timed out of over a third of all their trials (i.e. we can't confirm preference for these).**


```r
Intransitive$TimeOutTrials[Intransitive$f.id == 8146]/190
```

```
## [1] 0.347
```


**Ignoring these trials 93.7% of this outliers intransitive choices would involve a trial, for which we cannot determine the preference.**



```r
Intransitive$PIntrError[Intransitive$f.id == 8146]
```

```
## [1] 93.7
```


**Excluding this outlier (8146) are there group differences in number of timed out trials? Yes. The MTL group has timed out of significantly more trials than the control group.**


```r
summary(aov(TimeOutTrials ~ Group, data = Intransitive[Intransitive$f.id != 
    8146, ]))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## Group        2     43    21.7    4.43  0.015 *
## Residuals   87    427     4.9                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
pairwise.t.test(Intransitive$TimeOutTrials[Intransitive$f.id != 8146], Intransitive$Group[Intransitive$f.id != 
    8146], p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Intransitive$TimeOutTrials[Intransitive$f.id != 8146] and Intransitive$Group[Intransitive$f.id != 8146] 
## 
##     C     ETP  
## ETP 0.551 -    
## MTL 0.011 0.319
## 
## P value adjustment method: bonferroni
```


Group average timed out trials with exclusion


```r
aggregate(TimeOutTrials ~ Group, data = Intransitive[Intransitive$f.id != 8146, 
    ], FUN = mean)
```

```
##   Group TimeOutTrials
## 1     C         0.733
## 2   ETP         1.500
## 3   MTL         2.433
```


### Mispress trials

There are trials where the raw choice is not 1 or 4 (the response buttons).  

Participants from all groups have timed out trials.


```r
table(Intransitive$Group[Intransitive$MisPressTrials > 0])
```

```
## 
##   C ETP MTL 
##   2   3   4
```


They constitute 0.6 % of all trials.  

These do not include the timed out trials.


```r
sum(all.data$MisPress.1.0)/nrow(all.data) * 100
```

```
## [1] 0.596
```

```r

prop.table(table(all.data$MisPress.1.0, all.data$Group), 1)[2, ]
```

```
##      C    ETP    MTL 
## 0.0291 0.7282 0.2427
```


#### Average number of missed trials for each group:


```r
aggregate(MisPressTrials ~ Group, data = Intransitive, FUN = mean)
```

```
##   Group MisPressTrials
## 1     C          0.100
## 2   ETP          2.500
## 3   MTL          0.806
```

```r

ggplot(data = Intransitive, aes(x = Group, y = MisPressTrials)) + geom_boxplot() + 
    theme_classic()
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40.png) 


**Are there group differences in number of mispressed trials? No. (but looking at the boxplot hidden by the outlier)**


```r
summary(aov(MisPressTrials ~ Group, data = Intransitive))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Group        2     91    45.7    0.88   0.42
## Residuals   88   4551    51.7
```


**How much of the missed trials is outlier (8336) responsible for? One participants is responsible for two thirds of all mispress trials.**


```r
Intransitive$MisPressTrials[Intransitive$f.id == 8336]/sum(Intransitive$MisPressTrials)
```

```
## [1] 0.631
```


**This participant also has timed out of over a third of all their trials (i.e. we can't confirm preference for these)**


```r
Intransitive$MisPressTrials[Intransitive$f.id == 8336]/190
```

```
## [1] 0.342
```


**Ignoring these trials 90.9% of this outliers intransitive choices would involve a trial, for which we cannot determine the preference.**


```r
Intransitive$PIntrError[Intransitive$f.id == 8336]
```

```
## [1] 90.9
```


**Excluding 8336 are there group differences in number of timed out trials? No.** 


```r
summary(aov(MisPressTrials ~ Group, data = Intransitive[Intransitive$f.id != 
    8336, ]))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Group        2      8    3.93    0.67   0.51
## Residuals   87    510    5.86
```


Group average timed out trials with exclusion


```r
aggregate(MisPressTrials ~ Group, data = Intransitive[Intransitive$f.id != 8336, 
    ], FUN = mean)
```

```
##   Group MisPressTrials
## 1     C          0.100
## 2   ETP          0.345
## 3   MTL          0.806
```


### How to deal with these errors?

***Excluding these trials prior to intransitivity calculation completely throws off the intransitivity calculation.***

For example:
One participant missed 2 trials.(2/190*100 = 1.05%)  

Without accounting for these and assuming 1140 "clean" triplets we counted 25 intransitive choices (2.2%).  

These two trials affected 36 triplets out of 1140 (36/1140*100 = 3.16%). Note, this number could have been less.  

4 of the triplets that we had counted as intransitive included at least one missed trial (4/25*100 = 16%).  

So for these trials we can't claim intransitivity.   

We also can't claim no intransitivity for the remaining 32 triplets.  

If we exclude all 36 affected triplets we would have 1104 clean triplets and 21 of these would be clean intransitives so the percentage of intransitive choices would be 21/1104*100 = 1.9 %. (This is what CleanPercentIntr in Intransitive df is)

### CONCLUSION:

#### Exclude subjects 8146 and 8336: Use Intransitive.clean df on subject level

**Because we don't know their preferences for about a third of their trials AND over 90% of their intransitives counted otherwise are compromised (see PIntrError in Intransitive df)**


```r
Intransitive.clean <- Intransitive[Intransitive$f.id != 8146 & Intransitive$f.id != 
    8336, ]

all.data.clean <- all.data[all.data$f.id != 8146 & all.data$f.id != 8336, ]
```


#### Exclude trials: Use all.data.clean df on trial level


```r

sum(all.data.clean$MisPress.1.0)/nrow(all.data.clean) * 100
```

```
## [1] 0.225
```

```r
sum(all.data.clean$TimeOut.1.0)/nrow(all.data.clean) * 100
```

```
## [1] 0.828
```

```r

all.data.clean <- all.data.clean[all.data.clean$Error.1.0 == 0, ]
```






#### Factorize group variables for analyses


```r

Intransitive.clean$Group <- as.factor(Intransitive.clean$Group)
all.data.clean$Group <- as.factor(all.data.clean$Group)

```


Descriptives
-------------------------

### Number of participants


```r
table(Intransitive.clean$Group)
```

```
## 
##   C ETP MTL 
##  30  29  30
```


### Durations

#### Mean and sd RT per trial


```r
summary(all.data.clean$RT[all.data.clean$RT < 5001])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      50     965    1300    1480    1790    4970
```

```r

sd(all.data.clean$RT[all.data.clean$RT < 5001])
```

```
## [1] 710
```


#### Total task completion time


```r
TaskTime <- ddply(all.data.clean, .(f.id, Group), summarise, TaskTotalMs = sum(RT))

TaskTime$TaskTotalMin <- TaskTime$TaskTotalMs/(1000 * 60)

summary(TaskTime$TaskTotalMin)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    2.18    3.68    4.45    4.63    5.54    8.04
```

```r

range(TaskTime$TaskTotalMin)
```

```
## [1] 2.18 8.04
```

```r

sd(TaskTime$TaskTotalMin)
```

```
## [1] 1.22
```


#### Group differences in total task completion time


```r
summary(aov(TaskTotalMin ~ Group, data = TaskTime))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Group        2    6.3    3.17    2.17   0.12
## Residuals   86  125.7    1.46
```





### Intransitivities by groups

#### Number of times a trial was involved in intransitivity


```r
range(all.data.clean$IntransTripleCounted)
```

```
## [1]  0 17
```

```r

mean(all.data.clean$IntransTripleCounted)
```

```
## [1] 0.774
```

```r

sd(all.data.clean$IntransTripleCounted)
```

```
## [1] 1.46
```


#### Number of intransitivity for a subject


```r
range(Intransitive$CleanIntr)
```

```
## [1]   1 267
```

```r

mean(Intransitive$CleanIntr)
```

```
## [1] 44.7
```

```r

summary(Intransitive$CleanIntr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    21.0    37.0    44.7    54.0   267.0
```

```r

sd(Intransitive$CleanIntr)
```

```
## [1] 39.4
```


#### Group differences in intransitivies


```r

Aov.pint <- aov(CleanPercentIntr ~ Group, data = Intransitive.clean)
summary(Aov.pint)  # The group means are different
```

```
##             Df Sum Sq Mean Sq F value  Pr(>F)    
## Group        2    199    99.7    9.31 0.00022 ***
## Residuals   86    922    10.7                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

aggregate(CleanIntr ~ Group, data = Intransitive.clean, mean)
```

```
##   Group CleanIntr
## 1     C      30.9
## 2   ETP      38.4
## 3   MTL      67.3
```

```r
aggregate(CleanIntr ~ Group, data = Intransitive.clean, median)
```

```
##   Group CleanIntr
## 1     C      32.5
## 2   ETP      32.0
## 3   MTL      49.0
```

```r
aggregate(CleanIntr ~ Group, data = Intransitive.clean, sd)
```

```
##   Group CleanIntr
## 1     C      15.8
## 2   ETP      23.8
## 3   MTL      56.0
```

```r

aggregate(CleanPercentIntr ~ Group, data = Intransitive.clean, mean)
```

```
##   Group CleanPercentIntr
## 1     C             2.75
## 2   ETP             3.47
## 3   MTL             6.21
```

```r

kruskal.test(CleanIntr ~ Group, data = Intransitive.clean)
```

```
## Error: all group levels must be finite
```

```r

pairwise.wilcox.test(Intransitive.clean$CleanIntr, Intransitive.clean$Group, 
    p.adj = "bonferroni", exact = F)
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test 
## 
## data:  Intransitive.clean$CleanIntr and Intransitive.clean$Group 
## 
##     C      ETP   
## ETP 0.7846 -     
## MTL 0.0004 0.0186
## 
## P value adjustment method: bonferroni
```


### Other incidental factors (side or type of bar)

#### Side of bar

**The MTL group chooses left less frequently.**


```r
mean(all.data.clean$Choice.left1.right0)
```

```
## [1] 0.502
```

```r

aggregate(Choice.left1.right0 ~ Group, data = all.data.clean, mean)
```

```
##   Group Choice.left1.right0
## 1     C               0.512
## 2   ETP               0.509
## 3   MTL               0.486
```

```r

summary(aov(Choice.left1.right0 ~ Group, data = all.data.clean))
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)   
## Group           2      2    1.16    4.65 0.0096 **
## Residuals   16729   4181    0.25                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov(Choice.left1.right0 ~ Group, data = all.data.clean), conf.level = 0.95)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = Choice.left1.right0 ~ Group, data = all.data.clean)
## 
## $Group
##             diff     lwr       upr p adj
## ETP-C   -0.00355 -0.0258  0.018671 0.926
## MTL-C   -0.02653 -0.0486 -0.004461 0.013
## MTL-ETP -0.02298 -0.0453 -0.000695 0.041
```

```r
pairwise.t.test(all.data.clean$Choice.left1.right0, all.data.clean$Group, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  all.data.clean$Choice.left1.right0 and all.data.clean$Group 
## 
##     C     ETP  
## ETP 1.000 -    
## MTL 0.015 0.047
## 
## P value adjustment method: bonferroni
```


**Only the MTL group chooses left less than 50%.**



```r
left.test <- rep(0.5, nrow(all.data.clean))

t.test(all.data.clean$Choice.left1.right0, left.test)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  all.data.clean$Choice.left1.right0 and left.test
## t = 0.526, df = 16731, p-value = 0.5991
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.00554  0.00961
## sample estimates:
## mean of x mean of y 
##     0.502     0.500
```

```r

t.test(all.data.clean$Choice.left1.right0[all.data.clean$Group == "C"], left.test[1:nrow(all.data.clean[all.data.clean$Group == 
    "C", ])])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  all.data.clean$Choice.left1.right0[all.data.clean$Group == "C"] and left.test[1:nrow(all.data.clean[all.data.clean$Group == "C", all.data.clean$Choice.left1.right0[all.data.clean$Group == "C"] and     ])]
## t = 1.82, df = 5674, p-value = 0.06897
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.000938  0.025079
## sample estimates:
## mean of x mean of y 
##     0.512     0.500
```

```r

t.test(all.data.clean$Choice.left1.right0[all.data.clean$Group == "ETP"], left.test[1:nrow(all.data.clean[all.data.clean$Group == 
    "ETP", ])])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  all.data.clean$Choice.left1.right0[all.data.clean$Group == "ETP"] and left.test[1:nrow(all.data.clean[all.data.clean$Group == "ETP", all.data.clean$Choice.left1.right0[all.data.clean$Group == "ETP"] and     ])]
## t = 1.26, df = 5454, p-value = 0.208
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.00475  0.02179
## sample estimates:
## mean of x mean of y 
##     0.509     0.500
```

```r

t.test(all.data.clean$Choice.left1.right0[all.data.clean$Group == "MTL"], left.test[1:nrow(all.data.clean[all.data.clean$Group == 
    "MTL", ])])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  all.data.clean$Choice.left1.right0[all.data.clean$Group == "MTL"] and left.test[1:nrow(all.data.clean[all.data.clean$Group == "MTL", all.data.clean$Choice.left1.right0[all.data.clean$Group == "MTL"] and     ])]
## t = -2.17, df = 5601, p-value = 0.03042
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02755 -0.00137
## sample estimates:
## mean of x mean of y 
##     0.486     0.500
```



**But what side is chose does not predict how often a trial is involved in intransitivity.**



```r
summary(lmer(IntransTripleCounted ~ Choice.left1.right0 * Group + (1 | f.id), 
    data = all.data.clean))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ Choice.left1.right0 * Group + (1 | f.id) 
##    Data: all.data.clean 
## 
## REML criterion at convergence: 56966 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.334    0.578   
##  Residual             1.727    1.314   
## Number of obs: 16732, groups: f.id, 89
## 
## Fixed effects:
##                              Estimate Std. Error t value
## (Intercept)                    0.5079     0.1084    4.69
## Choice.left1.right0            0.0222     0.0350    0.63
## GroupETP                       0.1038     0.1546    0.67
## GroupMTL                       0.6552     0.1532    4.28
## Choice.left1.right0:GroupETP   0.0452     0.0500    0.90
## Choice.left1.right0:GroupMTL  -0.0336     0.0498   -0.67
## 
## Correlation of Fixed Effects:
##             (Intr) Ch.1.0 GrpETP GrpMTL C.1.0:GE
## Chc.lft1.r0 -0.165                              
## GroupETP    -0.701  0.116                       
## GroupMTL    -0.707  0.117  0.496                
## Ch.1.0:GETP  0.116 -0.700 -0.165 -0.082         
## Ch.1.0:GMTL  0.116 -0.703 -0.082 -0.162  0.492
```


#### Type of bar (this is not very helpful)


```r
Bars <- as.data.frame(table(all.data.clean$Choice.image, all.data.clean$f.id))

names(Bars) <- c("Bars", "f.id", "Freq")

Bars <- Bars[Bars$f.id != 8336 & Bars$f.id != 8146, ]

Bars$Group <- rep(NA, nrow(Bars))

for (i in 1:nrow(Bars)) {
    Bars$Group[i] <- as.character(Intransitive.clean$Group[which(Intransitive.clean$f.id == 
        Bars$f.id[i])])
}

Bars$Group <- as.factor(Bars$Group)

summary(aov(Freq ~ Bars, data = Bars))
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)    
## Bars          19   6779     357    14.4 <2e-16 ***
## Residuals   1760  43740      25                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

# pairwise.t.test(Bars$Freq, Bars$Bars, p.adjust = 'bonferroni')

rm(Bars)
```


#### Another way to look at type of bars is fitting the BTL models

Testing Luce choice model
  * A model with subject + 190 pairs + brand
  * If there aren't any intransitivities this should fit perfectly
  * I don't think I'm doing the right thing here..  
  * According to these slides [http://www.r-project.org/conferences/useR-2008/slides/Wickelmaier.pdf] (http://www.r-project.org/conferences/useR-2008/slides/Wickelmaier.pdf)

**Prepare data for eba package**


```r

all.data.clean2 <- all.data.clean
all.data.clean2$chose1 <- ifelse(all.data.clean2$Choice.image == 1, 1, 0)
all.data.clean2$chose2 <- ifelse(all.data.clean2$Choice.image == 2, 1, 0)
all.data.clean2$chose3 <- ifelse(all.data.clean2$Choice.image == 3, 1, 0)
all.data.clean2$chose4 <- ifelse(all.data.clean2$Choice.image == 4, 1, 0)
all.data.clean2$chose5 <- ifelse(all.data.clean2$Choice.image == 5, 1, 0)
all.data.clean2$chose6 <- ifelse(all.data.clean2$Choice.image == 6, 1, 0)
all.data.clean2$chose7 <- ifelse(all.data.clean2$Choice.image == 7, 1, 0)
all.data.clean2$chose8 <- ifelse(all.data.clean2$Choice.image == 8, 1, 0)
all.data.clean2$chose9 <- ifelse(all.data.clean2$Choice.image == 9, 1, 0)
all.data.clean2$chose10 <- ifelse(all.data.clean2$Choice.image == 10, 1, 0)
all.data.clean2$chose11 <- ifelse(all.data.clean2$Choice.image == 11, 1, 0)
all.data.clean2$chose12 <- ifelse(all.data.clean2$Choice.image == 12, 1, 0)
all.data.clean2$chose13 <- ifelse(all.data.clean2$Choice.image == 13, 1, 0)
all.data.clean2$chose14 <- ifelse(all.data.clean2$Choice.image == 14, 1, 0)
all.data.clean2$chose15 <- ifelse(all.data.clean2$Choice.image == 15, 1, 0)
all.data.clean2$chose16 <- ifelse(all.data.clean2$Choice.image == 16, 1, 0)
all.data.clean2$chose17 <- ifelse(all.data.clean2$Choice.image == 17, 1, 0)
all.data.clean2$chose18 <- ifelse(all.data.clean2$Choice.image == 18, 1, 0)
all.data.clean2$chose19 <- ifelse(all.data.clean2$Choice.image == 19, 1, 0)
all.data.clean2$chose20 <- ifelse(all.data.clean2$Choice.image == 20, 1, 0)

# Separate choice data in to groups

Data.mtl <- all.data.clean2[all.data.clean2$Group == "MTL", ]
Data.c <- all.data.clean2[all.data.clean2$Group == "C", ]
Data.etp <- all.data.clean2[all.data.clean2$Group == "ETP", ]

# Create three matrices tabulating pairwise counts
TrialNumber <- read.csv("TrialNumbers.csv")
TrialNumber$Image_right <- ifelse(TrialNumber$Image_right < 10, paste(0, as.character(TrialNumber$Image_right, 
    sep = "")), TrialNumber$Image_right)
TrialNumber$Image_left <- ifelse(TrialNumber$Image_left < 10, paste(0, as.character(TrialNumber$Image_left, 
    sep = "")), TrialNumber$Image_left)
TrialNumber$Paste <- paste(TrialNumber$Image_right, TrialNumber$Image_left, 
    sep = "")
TrialNumber$Paste <- gsub(" ", "", TrialNumber$Paste)

pairwise.count <- function(Data) {
    require(reshape)
    TM.m <- melt(TrialNumber[, c("Trial", "Paste")], id = c("Trial"))
    TM.m <- TM.m[order(TM.m$Trial), ]
    TM.m$Ones <- rep(c(1, 2), 190)
    TM.m <- TM.m[TM.m$Ones == 1, ]
    
    test <- aggregate(Data[, c("chose1", "chose2", "chose3", "chose4", "chose5", 
        "chose6", "chose7", "chose8", "chose9", "chose10", "chose11", "chose12", 
        "chose13", "chose14", "chose15", "chose16", "chose17", "chose18", "chose19", 
        "chose20")], by = list(id = Data$Trial), FUN = "sum")
    test <- merge(TM.m[, c("Trial", "value")], test, by.x = "Trial", by.y = "id")
    
    matrix <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("chose1", 
        "chose2", "chose3", "chose4", "chose5", "chose6", "chose7", "chose8", 
        "chose9", "chose10", "chose11", "chose12", "chose13", "chose14", "chose15", 
        "chose16", "chose17", "chose18", "chose19", "chose20"), c("opp1", "opp2", 
        "opp3", "opp4", "opp5", "opp6", "opp7", "opp8", "opp9", "opp10", "opp11", 
        "opp12", "opp13", "opp14", "opp15", "opp16", "opp17", "opp18", "opp19", 
        "opp20")))
    
    for (i in 1:20) {
        matrix[i, i] <- 0
    }
    
    ref <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("01", "02", 
        "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", 
        "15", "16", "17", "18", "19", "20"), c("01", "02", "03", "04", "05", 
        "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20")))
    
    ref <- as.data.frame(ref)
    
    for (i in 1:nrow(ref)) {
        for (j in 1:ncol(ref)) {
            ref[i, j] <- paste(row.names(ref)[i], names(ref)[j], sep = "")
        }
    }
    
    for (i in 1:nrow(ref)) {
        for (j in 1:ncol(ref)) {
            if (is.na(matrix[i, j] == T)) {
                pair <- ref[i, j]
                test.row <- which(test$value == ref[i, j])
                test.chose <- i + 2
                test.opp <- j + 2
                matrix[i, j] <- test[test.row, test.chose]
                matrix[j, i] <- test[test.row, test.opp]
            }
        }
    }
    
    return(matrix)
}

matrix.mtl <- pairwise.count(Data.mtl)
matrix.etp <- pairwise.count(Data.etp)
matrix.c <- pairwise.count(Data.c)

rm(TrialNumber, TM.m)
```


**Fitting BTL models using the eba package**  

Not sure what they are saying..
Checked for number of transitivities (fitting BTL separately for group), comparing the three models and group differences (using the built in function) but not sure what the results are.


```r
library("eba")

strans(matrix.mtl)
```

```
## 
## Stochastic Transitivity
## 
##          Violations ErrorRatio MeanDev MaxDev Deviance Df Pr(>Chi)
## Weak             24     0.0211  0.0373  0.100     9.38 13    0.744
## Moderate        159     0.1395  0.0502  0.233       NA NA       NA
## Strong          473     0.4149  0.0758  0.304       NA NA       NA
## ---
## Number of Tests: 1140
```

```r
strans(matrix.etp)
```

```
## 
## Stochastic Transitivity
## 
##          Violations ErrorRatio MeanDev MaxDev Deviance Df Pr(>Chi)
## Weak             10    0.00877  0.0191 0.0357    0.943  7    0.996
## Moderate         54    0.04737  0.0417 0.1724       NA NA       NA
## Strong          349    0.30614  0.0663 0.2069       NA NA       NA
## ---
## Number of Tests: 1140
```

```r
strans(matrix.c)
```

```
## 
## Stochastic Transitivity
## 
##          Violations ErrorRatio MeanDev MaxDev Deviance Df Pr(>Chi)
## Weak             14     0.0123  0.0288 0.0667     5.02  8    0.756
## Moderate         98     0.0860  0.0485 0.1333       NA NA       NA
## Strong          380     0.3333  0.0731 0.2667       NA NA       NA
## ---
## Number of Tests: 1140
```

```r

btl.mtl <- eba(matrix.mtl)
summary(btl.mtl)
```

```
## 
## Parameter estimates:
##    Estimate Std. Error z value Pr(>|z|)    
## 1   0.04902    0.00405    12.1   <2e-16 ***
## 2   0.07955    0.00650    12.2   <2e-16 ***
## 3   0.02551    0.00227    11.2   <2e-16 ***
## 4   0.04358    0.00366    11.9   <2e-16 ***
## 5   0.03656    0.00309    11.8   <2e-16 ***
## 6   0.02489    0.00222    11.2   <2e-16 ***
## 7   0.04506    0.00371    12.1   <2e-16 ***
## 8   0.06152    0.00502    12.3   <2e-16 ***
## 9   0.05509    0.00454    12.1   <2e-16 ***
## 10  0.04035    0.00338    11.9   <2e-16 ***
## 11  0.05199    0.00426    12.2   <2e-16 ***
## 12  0.05724    0.00467    12.3   <2e-16 ***
## 13  0.07611    0.00623    12.2   <2e-16 ***
## 14  0.06653    0.00543    12.2   <2e-16 ***
## 15  0.06658    0.00545    12.2   <2e-16 ***
## 16  0.02755    0.00241    11.4   <2e-16 ***
## 17  0.05731    0.00471    12.2   <2e-16 ***
## 18  0.02826    0.00247    11.5   <2e-16 ***
## 19  0.09830    0.00810    12.1   <2e-16 ***
## 20  0.06476    0.00530    12.2   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2 logLik1 logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1104.0  -853.6    500.6  2.7e-05 ***
## EBA        19 190  -405.1  -357.1     96.1        1    
## Effect      0  19  -605.5  -405.1    400.8  < 2e-16 ***
## Imbalance   1 190  -498.4  -496.6      3.7        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  848.3 
## Pearson X2: 94.8
```

```r

btl.etp <- eba(matrix.etp)
summary(btl.etp)
```

```
## 
## Parameter estimates:
##    Estimate Std. Error z value Pr(>|z|)    
## 1   0.04897    0.00414    11.8   <2e-16 ***
## 2   0.08747    0.00739    11.8   <2e-16 ***
## 3   0.01634    0.00162    10.1   <2e-16 ***
## 4   0.02603    0.00235    11.1   <2e-16 ***
## 5   0.02558    0.00231    11.1   <2e-16 ***
## 6   0.02644    0.00237    11.2   <2e-16 ***
## 7   0.08040    0.00682    11.8   <2e-16 ***
## 8   0.09498    0.00807    11.8   <2e-16 ***
## 9   0.06148    0.00516    11.9   <2e-16 ***
## 10  0.03762    0.00325    11.6   <2e-16 ***
## 11  0.05858    0.00494    11.9   <2e-16 ***
## 12  0.04588    0.00390    11.8   <2e-16 ***
## 13  0.05416    0.00458    11.8   <2e-16 ***
## 14  0.06273    0.00528    11.9   <2e-16 ***
## 15  0.04967    0.00421    11.8   <2e-16 ***
## 16  0.02315    0.00214    10.8   <2e-16 ***
## 17  0.03933    0.00338    11.6   <2e-16 ***
## 18  0.03346    0.00291    11.5   <2e-16 ***
## 19  0.09540    0.00807    11.8   <2e-16 ***
## 20  0.06997    0.00589    11.9   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2  logLik1  logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1187.57  -844.63   685.88   <2e-16 ***
## EBA        19 190  -386.39  -350.56    71.66        1    
## Effect      0  19  -692.42  -386.39   612.06   <2e-16 ***
## Imbalance   1 190  -495.15  -494.07     2.16        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  810.8 
## Pearson X2: 71.1
```

```r

btl.c <- eba(matrix.c)
summary(btl.c)
```

```
## 
## Parameter estimates:
##    Estimate Std. Error z value Pr(>|z|)    
## 1   0.04826    0.00401    12.0   <2e-16 ***
## 2   0.06098    0.00502    12.2   <2e-16 ***
## 3   0.01849    0.00176    10.5   <2e-16 ***
## 4   0.03701    0.00315    11.8   <2e-16 ***
## 5   0.02320    0.00211    11.0   <2e-16 ***
## 6   0.02448    0.00220    11.1   <2e-16 ***
## 7   0.07365    0.00606    12.2   <2e-16 ***
## 8   0.05091    0.00421    12.1   <2e-16 ***
## 9   0.05641    0.00466    12.1   <2e-16 ***
## 10  0.04826    0.00401    12.1   <2e-16 ***
## 11  0.05971    0.00492    12.1   <2e-16 ***
## 12  0.06291    0.00517    12.2   <2e-16 ***
## 13  0.06108    0.00502    12.2   <2e-16 ***
## 14  0.07936    0.00655    12.1   <2e-16 ***
## 15  0.04285    0.00358    12.0   <2e-16 ***
## 16  0.03914    0.00330    11.9   <2e-16 ***
## 17  0.05402    0.00446    12.1   <2e-16 ***
## 18  0.02341    0.00212    11.0   <2e-16 ***
## 19  0.10569    0.00879    12.0   <2e-16 ***
## 20  0.10826    0.00902    12.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2  logLik1  logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1193.57  -852.55   682.04   <2e-16 ***
## EBA        19 190  -401.28  -354.73    93.10        1    
## Effect      0  19  -695.38  -401.28   588.20   <2e-16 ***
## Imbalance   1 190  -498.18  -497.82     0.73        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  840.6 
## Pearson X2: 92.2
```

```r

anova(btl.mtl, btl.etp, btl.c)
```

```
## Analysis of deviance table for elimination-by-aspect models
## 
##     Model Resid. df Resid. Dev   Test    Df LR stat. Pr(>Chi)
## 1 btl.mtl       171       96.1                               
## 2 btl.etp       171       71.7 1 vs 2     0     24.4        0
## 3   btl.c       171       93.1 2 vs 3     0    -21.4        1
```

```r

array <- array(c(matrix.mtl, matrix.etp, matrix.c), c(20, 20, 3))

group.test(array)
```

```
## 
## Testing for group effects in EBA models:
## 
##            Df1  Df2 logLik1 logLik2 Deviance Pr(>Chi)    
## Overall      1 1140 -3487.3 -2550.8   1873.0   <2e-16 ***
## EBA.g       57  570 -1192.8 -1062.4    260.9        1    
## Group       19   57 -1294.0 -1192.8    202.4   <2e-16 ***
## Effect       0   19 -1993.3 -1294.0   1398.7   <2e-16 ***
## Imbalance    1  570 -1494.0 -1488.5     11.1        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


**Assign group utilities from fitted btl models**


```r
# u.scale the unnormalized utility scale of the stimuli; each utility scale
# value is defined as the sum of aspect values (parameters) that
# characterize a given stimulus

# mu the predicted choice probabilities for the upper triangle

u.scales <- as.data.frame(cbind(btl.mtl$u.scale, btl.etp$u.scale, btl.c$u.scale))
names(u.scales) <- c("MTL", "ETP", "C")

all.data.clean2$Group.utility.of.choice <- rep(NA, nrow(all.data.clean2))

for (i in 1:nrow(all.data.clean2)) {
    col <- as.character(all.data.clean2$Group[i])
    row <- all.data.clean2$Choice.image[i]
    all.data.clean2$Group.utility.choice[i] <- u.scales[row, col]
}
```


**Does the group utility of the chosen bar predict how many times a trial is involved in intransitivity? - Yes? The more it is liked the less it is involved in intransitivity?**


```r
summary(lmer(IntransTripleCounted ~ Group * Group.utility.choice + (1 | f.id), 
    data = all.data.clean2))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ Group * Group.utility.choice + (1 | f.id) 
##    Data: all.data.clean2 
## 
## REML criterion at convergence: 56728 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.331    0.575   
##  Residual             1.705    1.306   
## Number of obs: 16732, groups: f.id, 89
## 
## Fixed effects:
##                               Estimate Std. Error t value
## (Intercept)                      0.761      0.115    6.64
## GroupETP                         0.261      0.164    1.59
## GroupMTL                         0.962      0.165    5.84
## Group.utility.choice            -4.083      0.717   -5.69
## GroupETP:Group.utility.choice   -2.463      1.043   -2.36
## GroupMTL:Group.utility.choice   -5.944      1.171   -5.08
## 
## Correlation of Fixed Effects:
##             (Intr) GrpETP GrpMTL Grp.t. GETP:G
## GroupETP    -0.701                            
## GroupMTL    -0.695  0.487                     
## Grp.tlty.ch -0.371  0.260  0.258              
## GrpETP:Gr..  0.255 -0.372 -0.177 -0.688       
## GrpMTL:Gr..  0.227 -0.159 -0.408 -0.613  0.421
```

```r

ggplot(data = all.data.clean2, aes(x = Group.utility.choice, y = IntransTripleCounted, 
    group = Group, col = Group)) + geom_smooth(method = "loess") + theme_classic()
```

![plot of chunk unnamed-chunk-66](figure/unnamed-chunk-66.png) 


#### BTL models at individual level

**NOTE: eba package cannot deal with missing trials for a subject so use df where only the two subjects are excluded**


```r
all.data.sr <- all.data[all.data$f.id != 8146 & all.data$f.id != 8336, ]
```


**Preparing data**




**Create individual matrices tabulating pairwise counts**





```r
matrices <- ddply(all.data.sr, .(f.id), pairwise.count, .progress = "text")
```


**Fit BTL models at individual level**


```r
fit.btl <- function(matrices) {
    
    btl.fits <- list()
    name.btl.fits <- vector(1, mode = "character")
    
    for (i in 1:length(unique(matrices$f.id))) {
        # Cut matrix for one person
        temp <- matrices[matrices$f.id == unique(matrices$f.id)[i], c(2:21)]
        # Fit btl for one person and assign the list to the i'th item in a list
        btl.fits[[i]] <- eba(temp)
        # Rename list item with subject number
        name.btl.fits[i] <- paste("btl.fit.", unique(matrices$f.id)[i], sep = "")
        names(btl.fits)[[i]] <- name.btl.fits[i]
    }
    
    return(btl.fits)
}

btls <- fit.btl(matrices = matrices)
```


**Extract utility scales per subject**


```r
u.scales.ind <- as.data.frame(matrix(NA, nrow = 20, ncol = length(unique(matrices$f.id))))

for (i in 1:length(unique(matrices$f.id))) {
    j <- which(names(btls) == paste("btl.fit.", unique(matrices$f.id)[i], sep = ""))
    u.scales.ind[, i] <- btls[[j]]$u.scale
    names(u.scales.ind)[i] <- as.character(unique(matrices$f.id)[i])
}
```


**Add utilities to trial level data**


```r
all.data.sr$Util_left <- rep(NA, nrow(all.data.sr))
all.data.sr$Util_right <- rep(NA, nrow(all.data.sr))

for (i in 1:nrow(all.data.sr)) {
    subject <- as.character(all.data.sr$f.id[i])
    opt_left <- all.data.sr$Image_left[i]
    opt_right <- all.data.sr$Image_right[i]
    all.data.sr$Util_left[i] <- u.scales.ind[opt_left, subject]
    all.data.sr$Util_right[i] <- u.scales.ind[opt_right, subject]
}
```


**Variable for difference in utilities**


```r
all.data.sr$Util_diff <- all.data.sr$Util_left - all.data.sr$Util_right
all.data.sr$Util_diff_abs <- abs(all.data.sr$Util_diff)
```


**Does the difference in utility of the two options predict how many times a trial is involved in intransitivity? - Yes. The smaller the difference between the utilities of the bars the more often is a pair involved in an intransitivity**


```r
summary(lmer(IntransTripleCounted ~ Group * Util_diff_abs + (1 | f.id), data = all.data.sr))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ Group * Util_diff_abs + (1 | f.id) 
##    Data: all.data.sr 
## 
## REML criterion at convergence: 57427 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.336    0.58    
##  Residual             1.714    1.31    
## Number of obs: 16910, groups: f.id, 89
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)              0.7925     0.1084    7.31
## GroupETP                 0.0645     0.1543    0.42
## GroupMTL                 0.6224     0.1529    4.07
## Util_diff_abs           -7.8150     0.4801  -16.28
## GroupETP:Util_diff_abs   1.8687     0.6140    3.04
## GroupMTL:Util_diff_abs   2.2681     0.5411    4.19
## 
## Correlation of Fixed Effects:
##             (Intr) GrpETP GrpMTL Utl_d_ GETP:U
## GroupETP    -0.703                            
## GroupMTL    -0.709  0.498                     
## Util_dff_bs -0.148  0.104  0.105              
## GrpETP:Ut__  0.116 -0.133 -0.082 -0.782       
## GrpMTL:Ut__  0.131 -0.092 -0.125 -0.887  0.694
```

```r

ggplot(data = all.data.sr, aes(x = Util_diff_abs, y = IntransTripleCounted, 
    group = Group, col = Group)) + geom_smooth(method = "loess") + theme_classic()
```

![plot of chunk unnamed-chunk-75](figure/unnamed-chunk-75.png) 


### Transitivity across trials (memory and intransitivities)


```r
isNested(all.data.clean$f.id, all.data.clean$Group)
```

```
## [1] TRUE
```

```r

m5 <- lmer(IntransTripleCounted ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * 
    Group, data = all.data.clean)
summary(m5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ (Group | f.id) + c.Trialnumber * c.TrialQuad *      Group 
##    Data: all.data.clean 
## 
## REML criterion at convergence: 57089 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr       
##  f.id     (Intercept) 0.0582   0.241               
##           GroupETP    0.3271   0.572    -0.90      
##           GroupMTL    0.7087   0.842     0.08  0.37
##  Residual             1.7263   1.314               
## Number of obs: 16732, groups: f.id, 89
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                         5.11e-01   5.12e-02    9.98
## c.Trialnumber                       6.33e-04   7.96e-04    0.80
## c.TrialQuad                         2.70e-06   6.50e-06    0.42
## GroupETP                            9.66e-02   8.98e-02    1.08
## GroupMTL                            6.25e-01   1.73e-01    3.61
## c.Trialnumber:c.TrialQuad          -1.77e-07   1.35e-07   -1.31
## c.Trialnumber:GroupETP              7.22e-04   1.14e-03    0.64
## c.Trialnumber:GroupMTL             -8.50e-04   1.13e-03   -0.75
## c.TrialQuad:GroupETP                1.01e-05   9.29e-06    1.09
## c.TrialQuad:GroupMTL                4.35e-06   9.24e-06    0.47
## c.Trialnumber:c.TrialQuad:GroupETP -1.68e-07   1.93e-07   -0.87
## c.Trialnumber:c.TrialQuad:GroupMTL  1.30e-07   1.92e-07    0.68
## 
## Correlation of Fixed Effects:
##             (Intr) c.Trln c.TrlQ GrpETP GrpMTL c.Tr:.TQ c.T:GE c.T:GM
## c.Trialnmbr -0.001                                                   
## c.TrialQuad -0.380  0.004                                            
## GroupETP    -0.570  0.001  0.217                                     
## GroupMTL    -0.296  0.000  0.113  0.169                              
## c.Trlnm:.TQ  0.002 -0.916 -0.007 -0.001 -0.001                       
## c.Trln:GETP  0.001 -0.700 -0.003 -0.001  0.000  0.641                
## c.Trln:GMTL  0.001 -0.705 -0.003  0.000  0.000  0.646    0.493       
## c.TrlQ:GETP  0.266 -0.003 -0.700 -0.310 -0.079  0.005    0.006  0.002
## c.TrlQ:GMTL  0.268 -0.003 -0.704 -0.153 -0.160  0.005    0.002  0.006
## c.T:.TQ:GET -0.001  0.641  0.005  0.002  0.000 -0.700   -0.916 -0.452
## c.T:.TQ:GMT -0.001  0.645  0.005  0.001  0.001 -0.704   -0.451 -0.916
##             c.TQ:GE c.TQ:GM c.T:.TQ:GE
## c.Trialnmbr                           
## c.TrialQuad                           
## GroupETP                              
## GroupMTL                              
## c.Trlnm:.TQ                           
## c.Trln:GETP                           
## c.Trln:GMTL                           
## c.TrlQ:GETP                           
## c.TrlQ:GMTL  0.492                    
## c.T:.TQ:GET -0.010  -0.003            
## c.T:.TQ:GMT -0.003  -0.010   0.492
```

```r

aggregate(IntransTripleCounted ~ Group, data = all.data.clean, FUN = mean)
```

```
##   Group IntransTripleCounted
## 1     C                0.519
## 2   ETP                0.645
## 3   MTL                1.157
```

```r

plotLMER.fnc(m5, pred = "c.Trialnumber", intr = list("Group", c("C", "ETP", 
    "MTL"), "end", list(c("red", "green", "blue"), c(1, 1, 1))))
```

```
## effect sizes (ranges) for the interaction of  c.Trialnumber  and  Group :
##     Group  =  C :  0.0441 
##     Group  =  ETP :  0.109 
##     Group  =  MTL :  0.0612
```

![plot of chunk unnamed-chunk-76](figure/unnamed-chunk-761.png) 

```r

# Plot S1
ggplot(data = all.data.clean, aes(x = Trialnumber, y = IntransTripleCounted, 
    group = Group, col = Group)) + geom_smooth(method = "loess") + theme_classic() + 
    ylab("Number of Times involved in Intransitivity") + xlab("Trial Number") + 
    scale_color_discrete(breaks = c("C", "ETP", "MTL"), labels = c("Control", 
        "ETL", "MTL"))
```

![plot of chunk unnamed-chunk-76](figure/unnamed-chunk-762.png) 

```r

```


### Intransitivies and reaction times

#### RT across trials


```r

mean(all.data.clean$RT)
```

```
## [1] 1478
```

```r
aggregate(RT ~ Group, data = all.data.clean, mean)
```

```
##   Group   RT
## 1     C 1418
## 2   ETP 1407
## 3   MTL 1608
```

```r

RT <- aggregate(all.data.clean[, c("RT")], by = list(id = all.data.clean$f.id), 
    FUN = "mean")
Group <- aggregate(all.data.clean[, c("Group")], by = list(id = all.data.clean$f.id), 
    FUN = "unique")
RT$Group <- Group$x

summary(aov(x ~ Group, data = RT))
```

```
##             Df   Sum Sq Mean Sq F value Pr(>F)  
## Group        2   743408  371704    2.37    0.1 .
## Residuals   86 13506515  157052                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

rm(RT, Group)

t2 <- lmer(RT ~ (1 | Group:f.id) + Trialnumber * Group, data = all.data.clean)  #Fixed effect for trialnumber, group and trialnumber group interaction (ie slopes), random intercept for subject
summary(t2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: RT ~ (1 | Group:f.id) + Trialnumber * Group 
##    Data: all.data.clean 
## 
## REML criterion at convergence: 260539 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  Group:f.id (Intercept) 156170   395     
##  Residual               331491   576     
## Number of obs: 16732, groups: Group:f.id, 89
## 
## Fixed effects:
##                      Estimate Std. Error t value
## (Intercept)          1661.612     73.774   22.52
## Trialnumber            -2.536      0.140  -18.17
## GroupETP              -74.587    105.235   -0.71
## GroupMTL              273.462    104.352    2.62
## Trialnumber:GroupETP    0.711      0.199    3.57
## Trialnumber:GroupMTL   -0.872      0.198   -4.40
## 
## Correlation of Fixed Effects:
##             (Intr) Trlnmb GrpETP GrpMTL T:GETP
## Trialnumber -0.181                            
## GroupETP    -0.701  0.127                     
## GroupMTL    -0.707  0.128  0.496              
## Trlnmb:GETP  0.127 -0.700 -0.181 -0.090       
## Trlnmb:GMTL  0.127 -0.704 -0.089 -0.182  0.493
```

```r

# Raw data plot
ggplot(data = all.data.clean, aes(x = Trialnumber, y = RT, group = Group, col = Group)) + 
    geom_smooth(method = "loess") + theme_classic()
```

![plot of chunk unnamed-chunk-77](figure/unnamed-chunk-771.png) 

```r

# Model based plot
plotLMER.fnc(t2, pred = "Trialnumber", intr = list("Group", c("C", "ETP", "MTL"), 
    "end", list(c("red", "green", "blue"), c(1, 1, 1))))
```

```
## effect sizes (ranges) for the interaction of  Trialnumber  and  Group :
##     Group  =  C :  479 
##     Group  =  ETP :  345 
##     Group  =  MTL :  644
```

![plot of chunk unnamed-chunk-77](figure/unnamed-chunk-772.png) 


#### RT and intransitivities


```r

all.data.clean$RT.quad <- all.data.clean$RT^2

all.data.clean$RT.c <- all.data.clean$RT - mean(all.data.clean$RT)

all.data.clean$RT.quad.c <- all.data.clean$RT.quad - mean(all.data.clean$RT.quad)

a4 <- lmer(IntransTripleCounted ~ RT.c + RT.quad.c + Group + RT.c:Group + (1 | 
    f.id), data = all.data.clean)
summary(a4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ RT.c + RT.quad.c + Group + RT.c:Group +      (1 | f.id) 
##    Data: all.data.clean 
## 
## REML criterion at convergence: 56559 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.379    0.616   
##  Residual             1.677    1.295   
## Number of obs: 16732, groups: f.id, 89
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    5.43e-01   1.14e-01    4.77
## RT.c           7.34e-04   5.87e-05   12.51
## RT.quad.c     -8.65e-08   1.25e-08   -6.91
## GroupETP       1.29e-01   1.62e-01    0.80
## GroupMTL       5.59e-01   1.61e-01    3.47
## RT.c:GroupETP -4.22e-05   4.31e-05   -0.98
## RT.c:GroupMTL  8.07e-06   4.19e-05    0.19
## 
## Correlation of Fixed Effects:
##             (Intr) RT.c   RT.qd. GrpETP GrpMTL RT.:GE
## RT.c         0.009                                   
## RT.quad.c   -0.001 -0.848                            
## GroupETP    -0.701 -0.004 -0.003                     
## GroupMTL    -0.707 -0.015  0.010  0.496              
## RT.c:GrpETP -0.012 -0.352 -0.036  0.017  0.008       
## RT.c:GrpMTL -0.012 -0.234 -0.187  0.009 -0.007  0.542
```

```r

a2 <- lmer(IntransTripleCounted ~ Group + (1 | f.id), data = all.data.clean)
summary(a2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransTripleCounted ~ Group + (1 | f.id) 
##    Data: all.data.clean 
## 
## REML criterion at convergence: 56955 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.334    0.578   
##  Residual             1.727    1.314   
## Number of obs: 16732, groups: f.id, 89
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)    0.519      0.107    4.86
## GroupETP       0.127      0.152    0.83
## GroupMTL       0.638      0.151    4.22
## 
## Correlation of Fixed Effects:
##          (Intr) GrpETP
## GroupETP -0.701       
## GroupMTL -0.707  0.496
```

```r

anova(a4, a2)
```

```
## Data: all.data.clean
## Models:
## a2: IntransTripleCounted ~ Group + (1 | f.id)
## a4: IntransTripleCounted ~ RT.c + RT.quad.c + Group + RT.c:Group + 
## a4:     (1 | f.id)
##    Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
## a2  5 56957 56996 -28474    56947                            
## a4  9 56478 56547 -28230    56460   488      4     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

plotLMER.fnc(a4, pred = "RT.c", intr = list("Group", c("C", "ETP", "MTL"), "end", 
    list(c("red", "green", "blue"), c(1, 1, 1))))
```

```
## effect sizes (ranges) for the interaction of  RT.c  and  Group :
##     Group  =  C :  6.53 
##     Group  =  ETP :  6.15 
##     Group  =  MTL :  6.6
```

![plot of chunk unnamed-chunk-78](figure/unnamed-chunk-781.png) 

```r

ggplot(data = all.data.clean[all.data.clean$RT > 800 & all.data.clean$RT < 5001, 
    ], aes(x = RT, y = IntransTripleCounted, group = Group, col = Group)) + 
    theme_classic() + geom_smooth(method = "loess") + ylab("Number of Times Involved in Intransitivitiy")
```

![plot of chunk unnamed-chunk-78](figure/unnamed-chunk-782.png) 

