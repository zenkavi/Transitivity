Intransitivity
========================================================




### Organization code

Scroll below to descriptive if not interested.

Read Data in:


```r
######### Read Data in
setwd("~/Dropbox/CDS/Transitivity/grid files")
Data <- read.csv("complete_data.csv")
TrialNumber <- read.csv("TrialNumbers.csv")
options(digits = 3)
names(Data)
```





Organize Data


```r
######### Organize Data Create variables
Data$Choice <- ifelse(Data$Choice_1left_4right == 1, "left", "right")

Data$Choice.image <- ifelse(Data$Choice_1left_4right == 1, Data$Image_left, 
    Data$Image_right)

Data$Choice.left1.right0 <- ifelse(Data$Choice_1left_4right == 1, 1, 0)

Data$Group <- ifelse(Data$Group == 1, "MTL", ifelse(Data$Group == 2, "ETP", 
    "C"))

Data$f.id <- as.factor(Data$SubjectID)

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


Count intransitive choices

11.14.13 Corrected to include reverse transitivity as well.  
12.01.13 Includes counting error trials (trials where participant took too long to respond)  
01.30.14 Made more efficient creating functions and using dplyr
03.19.14 Trialnumber vs Trial corrected in all.ag and merge


```r
# Count intransitive choices

# Creates df with subject id in col 1 and number of timed out trials in col
# 2
Error <- as.data.frame(table(Data[which(Data$RT < 0), "SubjectID"]))

# Create temp df where intransitive choices will be counted by creating the
# 1140 triplets possible out of 20 options (C(20,3) = 1140)
comb <- t(combn(unique(Data$Image_left), 3))
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

# Functions to calculate intransitives

library(dplyr)
library(plyr)

data_ind <- group_by(Data, f.id)

# Function 1: Check intransitivity on one row of comb (in one triplet)
# Output: Record choice, intransitivity, error, error+intransitivity, trial
# number for each pair, last trial of triplet, pair identifier in one row

comb.row.fn <- function(df, comb.row) {
    # INPUT df: individual participants data comb.row: comb data frame split by
    # row
    j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
    # Record choice for A vs B
    ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
    # Record if the trial was error (i.e. timed out)
    ErrApreftoB <- ifelse(df$RT[j] < 0, 1, 0)
    # Find trial row in df that has the B vs C choice in comb
    k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for B vs C
    BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
    # Record if the trial was error (i.e. timed out)
    ErrBpreftoC <- ifelse(df$RT[k] < 0, 1, 0)
    # Find trial row in df that has the A vs C choice in comb
    l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) & 
        (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
    # Record choice for A vs C
    CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
    # Record if the trial was error (i.e. timed out)
    ErrCpreftoA <- ifelse(df$RT[l] < 0, 1, 0)
    # A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A
    # (the second is the reverse)
    Intrans <- ifelse(ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, ifelse(ApreftoB == 
        0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
    # A triplet involves and Error if any of the three trials involved has timed
    # out
    Err <- ifelse(ErrApreftoB == 1 | ErrBpreftoC == 1 | ErrCpreftoA == 1, 1, 
        0)
    # Mark triplets where there is both an error and an intransitivity
    ErrIntrans <- ifelse((ErrApreftoB == 1 | ErrBpreftoC == 1 | ErrCpreftoA == 
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
        BpreftoC = BpreftoC, CpreftoA = CpreftoA, Intrans = Intrans, ErrApreftoB = ErrApreftoB, 
        ErrBpreftoC = ErrBpreftoC, ErrCpreftoA = ErrCpreftoA, Err = Err, ErrIntrans = ErrIntrans, 
        TrialA = TrialA, TrialB = TrialB, TrialC = TrialC, ThirdTrial = ThirdTrial, 
        PasteA = PasteA, PasteB = PasteB, PasteC = PasteC)
    return(ret.dat)
}

# Create continuous triplet variable
comb <- mutate(comb, id = 1:nrow(comb))

# Function 2: Apply function 1 (row level intransitivity calculation) to
# subset of data including all triplets for one participant
comb.fn <- function(Data.cut, comb) {
    ddply(comb, .(id), comb.row.fn, df = Data.cut)
}

# Combine intransitivity calculation for all participant by applying to
# subject level calculation to all participants Output: 1140*number of
# subject rows recording with output of comb.row.fn function
all <- ddply(Data, .(f.id, Group), comb.fn, comb = comb, .progress = "text")

# Summarize triplet level intransitivity on subject level Output: nrow =
# number of subjects
Intransitive <- ddply(all, .(f.id, Group), summarise, Intrans = sum(Intrans), 
    Err = sum(Err), ErrIntrans = sum(ErrIntrans))

# Create additional subject level variables and calculations of percentages
Intransitive$Missed <- rep(NA, nrow(Intransitive))

# Assign number of missed trials from the Error df to the correct subject in
# Intrans df
for (i in 1:nrow(Error)) {
    j <- which(as.character(Intransitive$f.id) == as.character(Error$Var1[i]))
    Intransitive$Missed[j] <- Error$Freq[i]
}
# Or give 0 if there are no missed trials
Intransitive$Missed <- ifelse(is.na(Intransitive$Missed) == T, 0, Intransitive$Missed)

Intransitive$PercentMissed <- Intransitive$Missed/190 * 100
Intransitive$PercentIntr <- Intransitive$Intrans/1140 * 100
Intransitive$PercentMissedTrialsAffect <- Intransitive$Err/1140 * 100
# Percent of intransitivies that include an error
Intransitive$PIntrError <- Intransitive$ErrIntrans/Intransitive$Intrans * 100

# Create pair level intransitivity count

# Temp df before aggregating. Has all three 'Paste's (ie trials in one
# column)
all.m <- melt(all[, c("f.id", "Group", "Intrans", "Err", "ErrIntrans", "TrialA", 
    "TrialB", "TrialC")], id = c("f.id", "Group", "Intrans", "Err", "ErrIntrans"))

# Aggregate df to sum (I think Err and ErrIntrans are less usefull here.
# Using only Intrans so far.)  Note: Total intransitivities will be triple
# counted this way (because all three pairs that are involved in an
# intransitivity take the dummy variable 1)

# all.ag <- aggregate(all.m[,c('Intrans','Err', 'ErrIntrans')], by =
# list(f.id = all.m$f.id, Group = all.m$Group, Trial = all.m$value), FUN =
# 'sum')

all.ag <- aggregate(all.m[, c("Intrans", "Err", "ErrIntrans")], by = list(f.id = all.m$f.id, 
    Group = all.m$Group, Trialnumber = all.m$value), FUN = "sum")

# Merge Intransitivity info with rest of trial level data

# all.data <- merge(Data, all.ag, by = c('f.id', 'Group', 'Trial'))

all.data <- merge(Data, all.ag, by = c("f.id", "Group", "Trialnumber"))

# Create centered trial number and quadratic trial term

all.data$c.Trialnumber <- all.data$Trialnumber - mean(all.data$Trialnumber)
all.data$c.TrialQuad <- all.data$c.Trialnumber^2

# Create df counting how many times each trial was involved in
# intransitivity

Trialnumber.df <- as.data.frame(table(all$ThirdTrial, all$Intrans, all$f.id))

head(Trialnumber.df, 190)

names(Trialnumber.df) <- c("Trialnumber", "Intrans", "f.id", "IntransFreq")

# Exclude those where there is no intransitivity

Trialnumber.df <- Trialnumber.df[Trialnumber.df$Intrans == 1, ]

# Organize classes of columns

Trialnumber.df$Trialnumber <- as.numeric(as.character(Trialnumber.df$Trialnumber))
Trialnumber.df$Intrans <- as.numeric(as.character(Trialnumber.df$Intrans))

# Merge w pair level data

all.data <- merge(all.data, Trialnumber.df[, c("Trialnumber", "f.id", "IntransFreq")], 
    by = c("Trialnumber", "f.id"), all.x = T)

all.data <- all.data[order(all.data$f.id, all.data$Trialnumber), ]

# Replace NA's in IntransFreq w 0's (these trials have never been the
# largest trial in a triplet)

all.data$IntransFreq <- ifelse(is.na(all.data$IntransFreq), 0, all.data$IntransFreq)

```


Descriptives
-------------------------

### Number of participants


```r
table(Intransitive$Group)
```

```
## 
##   C ETP MTL 
##  30  30  31
```


### Number of intransitive choices

MTL has significantly more intransitivities than C and marginally more than ETP. 
ETP does not have more intransitivies than C.


```r
######### Descriptives

summary(Intransitive$Intrans)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    24.0    38.0    53.7    67.0   270.0
```

```r
aggregate(Intrans ~ Group, data = Intransitive, mean)
```

```
##   Group Intrans
## 1     C    32.0
## 2   ETP    50.8
## 3   MTL    77.5
```

```r
aggregate(Intrans ~ Group, data = Intransitive, median)
```

```
##   Group Intrans
## 1     C    34.5
## 2   ETP    37.0
## 3   MTL    56.0
```

```r
aggregate(PercentIntr ~ Group, data = Intransitive, mean)
```

```
##   Group PercentIntr
## 1     C        2.81
## 2   ETP        4.45
## 3   MTL        6.80
```

```r
aggregate(PercentIntr ~ Group, data = Intransitive, median)
```

```
##   Group PercentIntr
## 1     C        3.03
## 2   ETP        3.25
## 3   MTL        4.91
```

```r
Aov.pint <- aov(PercentIntr ~ Group, data = Intransitive)
summary(Aov.pint)  # The group means are different
```

```
##             Df Sum Sq Mean Sq F value  Pr(>F)    
## Group        2    246   122.8    8.24 0.00053 ***
## Residuals   88   1312    14.9                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(Aov.pint, conf.level = 0.95)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = PercentIntr ~ Group, data = Intransitive)
## 
## $Group
##         diff     lwr  upr p adj
## ETP-C   1.65 -0.7304 4.02 0.230
## MTL-C   3.99  1.6325 6.35 0.000
## MTL-ETP 2.34 -0.0137 4.70 0.052
```

```r
pairwise.t.test(Intransitive$PercentIntr, Intransitive$Group, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Intransitive$PercentIntr and Intransitive$Group 
## 
##     C       ETP    
## ETP 0.30670 -      
## MTL 0.00035 0.05990
## 
## P value adjustment method: bonferroni
```


### Kruskal-Wallis-Test

Same result as anova above. 
Number of intransitivies differ between groups.
MTL differs from both groups but ETP and C do not.


```r
Intransitive$Group <- as.factor(Intransitive$Group)
kruskal.test(Intrans ~ Group, data = Intransitive)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  Intrans by Group
## Kruskal-Wallis chi-squared = 20, df = 2, p-value = 4.466e-05
```

```r
pairwise.wilcox.test(Intransitive$Intrans, Intransitive$Group, p.adj = "bonferroni", 
    exact = F)
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test 
## 
## data:  Intransitive$Intrans and Intransitive$Group 
## 
##     C       ETP  
## ETP 0.193   -    
## MTL 2.9e-05 0.029
## 
## P value adjustment method: bonferroni
```


03.06.14 - Bootstrapping the median and plot


```r

set.seed(123)

# function which will bootstrap the standard error of the median
b.median <- function(data, num) {
    resamples <- lapply(1:num, function(i) sample(data, replace = T))
    r.median <- sapply(resamples, median)
    std.err <- sqrt(var(r.median))
    list(std.err = std.err, resamples = resamples, medians = r.median)
}

# generating the data to be used (percentages of intransitivities for each
# group)
intr.mtl <- Intransitive$PercentIntr[Intransitive$Group == "MTL"]
intr.etp <- Intransitive$PercentIntr[Intransitive$Group == "ETP"]
intr.c <- Intransitive$PercentIntr[Intransitive$Group == "C"]

# saving the results of the function b.median in the object b1
b.mtl <- b.median(intr.mtl, 1000)
b.etp <- b.median(intr.etp, 1000)
b.c <- b.median(intr.c, 1000)

# displaying the first of the 30 bootstrap samples b.mtl$resamples[1]

# displaying the standard error
b.mtl$std.err
```

```
## [1] 1.08
```

```r
b.etp$std.err
```

```
## [1] 0.795
```

```r
b.c$std.err
```

```
## [1] 0.5
```

```r

# displaying the histogram of the distribution of medians
# hist(b.mtl$medians) hist(b.etp$medians) hist(b.c$medians)

median.intr <- aggregate(PercentIntr ~ Group, data = Intransitive, median)
median.intr$se <- rep(NA, 3)
median.intr$se[median.intr$Group == "C"] <- b.c$std.err
median.intr$se[median.intr$Group == "ETP"] <- b.etp$std.err
median.intr$se[median.intr$Group == "MTL"] <- b.mtl$std.err
levels(median.intr$Group) <- c("C", "ETL", "MTL")

limits <- aes(ymax = PercentIntr + se, ymin = PercentIntr - se)
ggplot(data = median.intr, aes(x = Group, y = PercentIntr)) + geom_point() + 
    geom_errorbar(limits, position = "dodge") + theme_classic() + xlab("") + 
    ylab("Percentage of Intransitivities") + theme(axis.text = element_text(size = 14), 
    axis.title = element_text(size = 14, face = "bold"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


### Percent left

No significant group differences (unless you want to pick on the marginal MTL-C comparison). Near 50% overall.


```r
# Percent left = No group differences
mean(Data$Choice.left1.right0)
```

```
## [1] 0.491
```

```r
aggregate(Choice.left1.right0 ~ Group, data = Data, mean)
```

```
##   Group Choice.left1.right0
## 1     C               0.510
## 2   ETP               0.491
## 3   MTL               0.472
```

```r
# Just to make sure: bring to subject level
Left <- aggregate(Data[, c("Choice.left1.right0")], by = list(id = Data$f.id), 
    FUN = "mean")
Left$Group <- Intransitive$Group
ggplot(data = Left, aes(x = Group, y = x, group = Group)) + geom_boxplot() + 
    theme_classic()
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
summary(aov(x ~ Group, data = Left))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## Group        2  0.021 0.01062    2.43  0.094 .
## Residuals   88  0.385 0.00438                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov(x ~ Group, data = Left), conf.level = 0.95)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = x ~ Group, data = Left)
## 
## $Group
##            diff     lwr     upr p adj
## ETP-C   -0.0191 -0.0599 0.02161 0.505
## MTL-C   -0.0373 -0.0777 0.00308 0.076
## MTL-ETP -0.0182 -0.0586 0.02220 0.533
```

```r
pairwise.t.test(Left$x, Left$Group, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Left$x and Left$Group 
## 
##     C     ETP  
## ETP 0.798 -    
## MTL 0.091 0.857
## 
## P value adjustment method: bonferroni
```

```r
rm(Left)
```


### Do people chose left more/less than 50%?

Overall yes but due to MTL group.


```r
left.test <- rep(0.5, nrow(Data))
t.test(Data$Choice.left1.right0, left.test)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data$Choice.left1.right0 and left.test
## t = -2.42, df = 17289, p-value = 0.01558
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01665 -0.00174
## sample estimates:
## mean of x mean of y 
##     0.491     0.500
```

```r
Data.c <- Data[Data$Group == "C", ]
t.test(Data.c$Choice.left1.right0, left.test)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.c$Choice.left1.right0 and left.test
## t = 1.48, df = 5699, p-value = 0.138
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.00316  0.02281
## sample estimates:
## mean of x mean of y 
##      0.51      0.50
```

```r
Data.etp <- Data[Data$Group == "ETP", ]
t.test(Data.etp$Choice.left1.right0, left.test)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.etp$Choice.left1.right0 and left.test
## t = -1.4, df = 5699, p-value = 0.1603
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02228  0.00368
## sample estimates:
## mean of x mean of y 
##     0.491     0.500
```

```r
Data.mtl <- Data[Data$Group == "MTL", ]
t.test(Data.mtl$Choice.left1.right0, left.test)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.mtl$Choice.left1.right0 and left.test
## t = -4.23, df = 5889, p-value = 2.396e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.0403 -0.0148
## sample estimates:
## mean of x mean of y 
##     0.472     0.500
```


### RT

No group differences.


```r
# RT = No group differences
mean(Data$RT)
```

```
## [1] 1470
```

```r
aggregate(RT ~ Group, data = Data, mean)
```

```
##   Group   RT
## 1     C 1413
## 2   ETP 1408
## 3   MTL 1587
```

```r
RT <- aggregate(Data[, c("RT")], by = list(id = Data$f.id), FUN = "mean")
Group <- aggregate(Data[, c("Group")], by = list(id = Data$f.id), FUN = "unique")
RT$Group <- Group$x
ggplot(data = RT, aes(x = Group, y = x, group = Group)) + geom_boxplot() + theme_classic()
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
summary(aov(x ~ Group, data = RT))
```

```
##             Df   Sum Sq Mean Sq F value Pr(>F)
## Group        2   636673  318337     2.2   0.12
## Residuals   88 12719843  144544
```

```r
TukeyHSD(aov(x ~ Group, data = RT), conf.level = 0.95)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = x ~ Group, data = RT)
## 
## $Group
##           diff    lwr upr p adj
## ETP-C    -5.14 -239.2 229 0.998
## MTL-C   173.87  -58.3 406 0.180
## MTL-ETP 179.01  -53.1 411 0.163
```

```r
pairwise.t.test(RT$x, RT$Group, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  RT$x and RT$Group 
## 
##     C    ETP 
## ETP 1.00 -   
## MTL 0.23 0.21
## 
## P value adjustment method: bonferroni
```

```r
rm(RT, Group)
```


### Any of the bars chosen more often?

Doesn't seem so. But hard to tell from this. See the end of the page for differences (which are apparently significant but not sure whether meaningful).


```r
# Any of the bars chosen more? Doesn't seem so.
Bar <- as.data.frame(prop.table(table(Data$Choice.image)))
names(Bar) <- c("Bar", "Prop")
plot(Prop ~ Bar, data = Bar, ylim = c(0, 1))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
rm(Bar)
```


### Group differences in RT controlling for individual differences?

Not significant. (MTL marginally slower)


```r
# Group differences in RT controlling for individual differences: not
# significant
m1 <- lmer(RT ~ Group + (1 | f.id), data = Data)
summary(m1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: RT ~ Group + (1 | f.id) 
##    Data: Data 
## 
## REML criterion at convergence: 272219 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 142462   377     
##  Residual             394763   628     
## Number of obs: 17290, groups: f.id, 91
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  1412.70      69.41   20.35
## GroupETP       -5.14      98.16   -0.05
## GroupMTL      173.87      97.37    1.79
## 
## Correlation of Fixed Effects:
##          (Intr) GrpETP
## GroupETP -0.707       
## GroupMTL -0.713  0.504
```


### Trial number has  a significant fixed effect on RT 

People get faster in later trials.


```r
# Trial number has a significant fixed effect on RT: People get faster in
# later trials
m2 <- lmer(RT ~ Trialnumber + Group + (1 | f.id), data = Data)
summary(m2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: RT ~ Trialnumber + Group + (1 | f.id) 
##    Data: Data 
## 
## REML criterion at convergence: 271466 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 142559   378     
##  Residual             377814   615     
## Number of obs: 17290, groups: f.id, 91
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept) 1638.9313    69.8891   23.45
## Trialnumber   -2.3689     0.0852  -27.79
## GroupETP      -5.1419    98.1656   -0.05
## GroupMTL     173.8651    97.3707    1.79
## 
## Correlation of Fixed Effects:
##             (Intr) Trlnmb GrpETP
## Trialnumber -0.116              
## GroupETP    -0.702  0.000       
## GroupMTL    -0.708  0.000  0.504
```

```r
par(mfrow = c(1, 2))
# Intercept = a
log.a <- coef(summary(lm(log(RT) ~ log(Trialnumber), data = Data[Data$RT > 0, 
    ])))[1]
a <- exp(log.a)
# Slope = k
k <- coef(summary(lm(log(RT) ~ log(Trialnumber), data = Data[Data$RT > 0, ])))[2]
plot(aggregate(log(RT) ~ log(Trialnumber), data = Data[Data$RT > 0, ], mean), 
    col = "red", pch = 16, cex = 0.5)
abline(log.a, k)
plot(Data$Trialnumber, a * (Data$Trialnumber)^k, pch = 16, cex = 0.5, xlab = "Trialnumber", 
    ylab = "RT")
points(aggregate(RT ~ Trialnumber, data = Data[Data$RT > 0, ], mean), col = "red", 
    pch = 16, cex = 0.5)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


### Does RT predict number of intransitive choices? 

No (as expected). Also shows the significantly higher number of intransitivies for the MTL group.


```r
# Does RT predict number of intransitive choices? No (as expected).
RT.sub <- aggregate(RT ~ f.id, data = Data, mean)
Intransitive <- Intransitive[order(Intransitive$f.id), ]
Intransitive$Mean.RT <- RT.sub$RT
rm(RT.sub)
m3 <- glm(Intrans ~ Group + Mean.RT, data = Intransitive)
summary(m3)
```

```
## 
## Call:
## glm(formula = Intrans ~ Group + Mean.RT, data = Intransitive)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -57.12  -25.47   -9.63   13.86  189.84  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 36.20948   19.30087    1.88  0.06400 .  
## GroupETP    18.75135   11.42604    1.64  0.10439    
## GroupMTL    46.00194   11.53683    3.99  0.00014 ***
## Mean.RT     -0.00298    0.01241   -0.24  0.81078    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 1958)
## 
##     Null deviance: 202402  on 90  degrees of freedom
## Residual deviance: 170368  on 87  degrees of freedom
## AIC: 953.9
## 
## Number of Fisher Scoring iterations: 2
```


### Correlation of RT and number of intransitive choices by group


```r
# Correlation of RT and number of intransitive choices by group
cor(Intransitive$Mean.RT[Intransitive$Group == "C"], Intransitive$Intrans[Intransitive$Group == 
    "C"])
```

```
## [1] 0.13
```

```r
cor(Intransitive$Mean.RT[Intransitive$Group == "ETP"], Intransitive$Intrans[Intransitive$Group == 
    "ETP"])
```

```
## [1] 0.24
```

```r
cor(Intransitive$Mean.RT[Intransitive$Group == "MTL"], Intransitive$Intrans[Intransitive$Group == 
    "MTL"])
```

```
## [1] -0.238
```



![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 


NOTE: Problem with data
--------------------------------------

There are trials where RT<0.  

They constitute 1.19 % of all trials.  

67.5 of these timed out trials are in the MTL group.


```r
sum(Data$RT < 0)
```

```
## [1] 206
```

```r
sum(Data$RT < 0)/nrow(Data)
```

```
## [1] 0.0119
```

```r
all.data$Error <- ifelse(all.data$RT < 0, 1, 0)
prop.table(table(all.data$Error, all.data$Group), 1)
```

```
##    
##         C   ETP   MTL
##   0 0.332 0.331 0.337
##   1 0.107 0.218 0.675
```


***Excluding these trials completely throws off the intransitivity calculation.***

Participants from all groups missed at least one trial like this. 
But the MTL is group is responsible for 67.5% of it.
In fact, one participant (8146) is responsible for 32%. (This participant does not have RT>0 for 66/190 - 34% of trials)




### 57 participants with trials where RT<0


```r
table(Data[which(Data$RT < 0), "SubjectID"])
```

```
## 
##  119 3256 3574 3777 3866 6483 6516 6965 7063 7134 7486 7596 7901 7922 8090 
##   12    1    8    2    1    1    1    5    1    1    2    3    1    1    1 
## 8093 8100 8103 8106 8109 8112 8123 8124 8125 8132 8139 8140 8143 8146 8147 
##    3    1    1    6    2    1    3   12    2    3    1    1    4   66    2 
## 8153 8158 8159 8176 8177 8178 8179 8180 8182 8209 8210 8228 8231 8256 8283 
##    5    3    3    1    2    7    1    1    2    3    1    5    2    4    1 
## 8293 8301 8314 8315 8318 8320 8327 8341 8345 8359 8360 9644 
##    1    2    1    1    1    1    1    4    1    1    1    1
```

```r
prop.table(table(Data[which(Data$RT < 0), "Group"]))
```

```
## 
##     C   ETP   MTL 
## 0.107 0.218 0.675
```


### How many of these trials are involved in intransitives?


```r
all.data$ErrAndIntrans <- ifelse(all.data$Intrans == 1 & all.data$Error == 1, 
    1, 0)
sum(all.data$ErrAndIntrans)
```

```
## [1] 43
```


#### Which group made missed most trials?

The MTL group.


```r
aggregate(Missed ~ Group, data = Intransitive, sum)
```

```
##   Group Missed
## 1     C     22
## 2   ETP     45
## 3   MTL    139
```


Most people from all groups missed 0 or 1 trials (out of 190).
Nobody in the C group missed more than 3 trials.
Nobody in the ETP group missed more than 8 trials.
There is one participant in the MTL group who has missed 66 trials.
Without this participant MTL group still has most missed trials but not by this much.

(For table below: rows are number of missed trials NOT INTRANSITIVIES, cols are number of people from each group)


```r
table(Intransitive$Missed, Intransitive$Group)
```

```
##     
##       C ETP MTL
##   0  14  14   6
##   1  11   8  10
##   2   4   2   3
##   3   1   1   5
##   4   0   1   2
##   5   0   1   2
##   6   0   1   0
##   7   0   1   0
##   8   0   1   0
##   12  0   0   2
##   66  0   0   1
```


PercentMissed: Trials out of 190 that have RT<0

PercentIntr: Triplets with intransitivity out of 1140 possible triplets

PercentMissedTrialsAffect: Triplets where 1 or more pair has RT<0

PIntrError: Intransitive triplets that have 1 or more pairs with RT<0


```r
aggregate(PercentIntr ~ Group, data = Intransitive, mean)
```

```
##   Group PercentIntr
## 1     C        2.81
## 2   ETP        4.45
## 3   MTL        6.80
```

```r
aggregate(PercentMissedTrialsAffect ~ Group, data = Intransitive, mean)
```

```
##   Group PercentMissedTrialsAffect
## 1     C                      1.15
## 2   ETP                      2.31
## 3   MTL                      5.74
```

```r
aggregate(PIntrError ~ Group, data = Intransitive, mean)
```

```
##   Group PIntrError
## 1     C       2.78
## 2   ETP       9.50
## 3   MTL      10.39
```


So for example:
One participant missed 2 trials.(2/190*100 = 1.05%)  

Without accounting for these and assuming 1140 "clean" triplets we counted 25 intransitive choices (2.2%).  

These two trials affected 36 triplets out of 1140 (36/1140*100 = 3.16%). Note, this number could have been less.  

4 of the triplets that we had counted as intransitive included at least one missed trial (4/25*100 = 16%).  

So for these trials we can't claim intransitivity.   

We also can't claim no intransitivity for the remaining 32 triplets.  

If we exclude all 36 affected triplets we would have 1104 clean triplets and 21 of these would be clean intransitives so the percentage of intransitive choices would be 21/1104*100 = 1.9 %.


```r
Intransitive$CleanTriplets <- 1140 - Intransitive$Err
Intransitive$CleanIntr <- Intransitive$Intrans - Intransitive$ErrIntrans
Intransitive$CleanPercentIntr <- Intransitive$CleanIntr/Intransitive$CleanTriplets * 
    100
```


Looking at the descriptives again based on this re-calculation:

***All values are slightly smaller but the significant difference between C and MTL holds***


```r
summary(Intransitive$CleanIntr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    21.0    37.0    49.2    57.0   267.0
```

```r
aggregate(CleanIntr ~ Group, data = Intransitive, mean)
```

```
##   Group CleanIntr
## 1     C      31.1
## 2   ETP      46.0
## 3   MTL      69.8
```

```r
aggregate(CleanIntr ~ Group, data = Intransitive, median)
```

```
##   Group CleanIntr
## 1     C      32.5
## 2   ETP      34.5
## 3   MTL      50.0
```

```r
aggregate(CleanPercentIntr ~ Group, data = Intransitive, mean)
```

```
##   Group CleanPercentIntr
## 1     C             2.76
## 2   ETP             4.12
## 3   MTL             6.37
```

```r
aggregate(CleanPercentIntr ~ Group, data = Intransitive, median)
```

```
##   Group CleanPercentIntr
## 1     C             2.94
## 2   ETP             3.03
## 3   MTL             4.57
```

```r
Aov.cleanint <- aov(CleanPercentIntr ~ Group, data = Intransitive)
summary(Aov.cleanint)
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)   
## Group        2    204   101.8    6.74 0.0019 **
## Residuals   88   1328    15.1                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(Aov.cleanint, conf.level = 0.95)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = CleanPercentIntr ~ Group, data = Intransitive)
## 
## $Group
##         diff    lwr  upr p adj
## ETP-C   1.36 -1.032 3.75 0.369
## MTL-C   3.61  1.241 5.98 0.001
## MTL-ETP 2.25 -0.119 4.62 0.066
```

```r
pairwise.t.test(Intransitive$CleanPercentIntr, Intransitive$Group, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Intransitive$CleanPercentIntr and Intransitive$Group 
## 
##     C      ETP   
## ETP 0.5363 -     
## MTL 0.0014 0.0780
## 
## P value adjustment method: bonferroni
```



```r
kruskal.test(CleanIntr ~ as.factor(Group), data = Intransitive)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  CleanIntr by as.factor(Group)
## Kruskal-Wallis chi-squared = 13.5, df = 2, p-value = 0.001183
```

```r
pairwise.wilcox.test(Intransitive$CleanIntr, Intransitive$Group, p.adj = "bonferroni", 
    exact = F)
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test 
## 
## data:  Intransitive$CleanIntr and Intransitive$Group 
## 
##     C       ETP    
## ETP 0.47303 -      
## MTL 0.00095 0.07733
## 
## P value adjustment method: bonferroni
```


Same as above.


```r
# Does RT predict number of intransitive choices? No (as expected).
Data.clean <- Data[Data$RT >= 0, ]
RT.sub.clean <- aggregate(RT ~ f.id, data = Data.clean, mean)
Intransitive <- Intransitive[order(Intransitive$f.id), ]
Intransitive$Mean.RT.c <- RT.sub.clean$RT
rm(RT.sub.clean)
m3.c <- glm(CleanIntr ~ Group + Mean.RT, data = Intransitive)
summary(m3.c)
```

```
## 
## Call:
## glm(formula = CleanIntr ~ Group + Mean.RT, data = Intransitive)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -62.5   -24.0   -10.5    14.2   196.1  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  46.9865    19.1356    2.46  0.01606 *  
## GroupETP     14.8421    11.3282    1.31  0.19358    
## GroupMTL     40.6991    11.4380    3.56  0.00061 ***
## Mean.RT      -0.0113     0.0123   -0.92  0.36216    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 1925)
## 
##     Null deviance: 192419  on 90  degrees of freedom
## Residual deviance: 167462  on 87  degrees of freedom
## AIC: 952.4
## 
## Number of Fisher Scoring iterations: 2
```



```r
# Correlation of RT and number of intransitive choices by group
cor(Intransitive$Mean.RT.c[Intransitive$Group == "C"], Intransitive$CleanIntr[Intransitive$Group == 
    "C"])
```

```
## [1] 0.118
```

```r
cor(Intransitive$Mean.RT.c[Intransitive$Group == "ETP"], Intransitive$CleanIntr[Intransitive$Group == 
    "ETP"])  #This decreases most.
```

```
## [1] 0.0979
```

```r
cor(Intransitive$Mean.RT.c[Intransitive$Group == "MTL"], Intransitive$CleanIntr[Intransitive$Group == 
    "MTL"])
```

```
## [1] -0.343
```





Notes and TO DO's since last meeting with Eric:
--------------------------------------

1. Timing out still could be an error that should be counted some other way.

2. Ways to add power to the current dataset:
  * Running more subjects
  * Error with the two missing participants: Data files were corrupt. Nothing recovered.

3. Brand information: How should this be included in an HLM? It is a source of variance.

4. What do Kruskell Wallace tests show?
  * [http://yatani.jp/HCIstats/KruskalWallis] (http://yatani.jp/HCIstats/KruskalWallis)
  * Kruskal-Wallis is basically a non-parametric version of ANOVA. Thus, if you have the data which contain more than two groups to compare, and your data are ordinal or your data cannot assume the normality, Kruskal-Wallis is the way to go. Fortunately, in R, the way to run a Kruskal-Wallis test is similar to the way to run an ANOVA test.
  * Similar to ANOVA, you need to do a post-hoc test after Kruskal-Wallis and Friedman if you find a significant effect. As we do multiple t tests with Bonferroni or Holm correction, we can do Mann-Whitney or Wilcoxon tests with the same corrections. If you have unpaired (factorial, or non-repeated-measure) data, you can do pairwise comparison with a Mann-Whitney test.
  * If you have any tied value in your data, you can only calculate an approximate p value. (You can try to calculate the exact p value by changing the option to "exact=T", but will get warnings if you have ties.) Unfortunately, it seems there is no way to get this around, but in many cases, this approximate p value is close enough to the exact p value, and will not cause a lot of troubles. But if you really want to be very precise, you can calculate the exact p value by using functions in coin package, and manually apply Bonferroni corrections to the p values you have gained by using p.adjust() function.

5. What was used in Camille et al. (2011):
  * Participants made made choices in 11 choice sets between bundles consisting of 3 to 7 options.
  * Each bundle was implied a restriction in price and income. (so each bundle falls on a single line defined by the number of bars and juice boxes. Making choices that have unique slopes - not matching the slope of any other choice constitutes an intransitivity).
  * Transitivy was calculated using methods from economics (possible due to the restrictions in the design of the choice sets) that I do not think can be applied to this design.
  * Overall VMF lesion patients made more violations to transitivity in number and severity compared to normal controls.

6. Order effects
  * Analysis of intransitivities from subject level to trial level (how many times was trial X involved in intransitivity?)
  * Test linear and quadratic order variables of trial in 190
  * Frequency of intransitivities over time
  * Give 1 to all the three trials that are involved in an intransitivity
  * Note: Each pair can be involved in max 18 intransitivities
  * There are no overall trial number effects. But there are large individual differences. (seen in individual plots).
  * For some people these are significant. But in different directions so they cancel out.
  * I did not find a pattern on who these are significant for (checked for group only) but if you can think of other interactions that I should look in to more carefully I can do that.






```r
# For the quadratic term the sign is positive when the model is convex and
# negative when the curve is concave.

# Multilevel model on trial level checking for effect of subject, group and
# trial

# Also note significant group effects. MTL differs significantly more from
# the control group but ETL does not m4 <- lmer(Intrans ~ (Group|f.id) +
# c.Trialnumber*c.TrialQuad*Group, data=all.data) #Wrong DV
m4 <- lmer(IntransFreq ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * Group, 
    data = all.data)
summary(m4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IntransFreq ~ (Group | f.id) + c.Trialnumber * c.TrialQuad *      Group 
##    Data: all.data 
## 
## REML criterion at convergence: 37702 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr     
##  f.id     (Intercept) 0.00477  0.0691            
##           GroupETP    0.03672  0.1916   0.40     
##           GroupMTL    0.07004  0.2647   0.54 0.18
##  Residual             0.50505  0.7107            
## Number of obs: 17290, groups: f.id, 91
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                         1.33e-01   1.89e-02    7.03
## c.Trialnumber                       3.31e-03   4.29e-04    7.72
## c.TrialQuad                         1.18e-05   3.50e-06    3.36
## GroupETP                            8.75e-02   4.79e-02    1.83
## GroupMTL                            1.65e-01   6.00e-02    2.74
## c.Trialnumber:c.TrialQuad          -1.15e-07   7.26e-08   -1.59
## c.Trialnumber:GroupETP              1.55e-03   6.07e-04    2.55
## c.Trialnumber:GroupMTL              3.40e-03   6.02e-04    5.66
## c.TrialQuad:GroupETP                3.73e-06   4.95e-06    0.75
## c.TrialQuad:GroupMTL                2.49e-05   4.91e-06    5.07
## c.Trialnumber:c.TrialQuad:GroupETP -1.96e-08   1.03e-07   -0.19
## c.Trialnumber:c.TrialQuad:GroupMTL  9.30e-08   1.02e-07    0.91
## 
## Correlation of Fixed Effects:
##             (Intr) c.Trln c.TrlQ GrpETP GrpMTL c.Tr:.TQ c.T:GE c.T:GM
## c.Trialnmbr  0.000                                                   
## c.TrialQuad -0.556  0.000                                            
## GroupETP    -0.395  0.000  0.220                                     
## GroupMTL    -0.315  0.000  0.175  0.125                              
## c.Trlnm:.TQ  0.000 -0.917  0.000  0.000  0.000                       
## c.Trln:GETP  0.000 -0.707  0.000  0.000  0.000  0.648                
## c.Trln:GMTL  0.000 -0.713  0.000  0.000  0.000  0.653    0.504       
## c.TrlQ:GETP  0.393  0.000 -0.707 -0.311 -0.124  0.000    0.000  0.000
## c.TrlQ:GMTL  0.396  0.000 -0.713 -0.157 -0.246  0.000    0.000  0.000
## c.T:.TQ:GET  0.000  0.648  0.000  0.000  0.000 -0.707   -0.917 -0.462
## c.T:.TQ:GMT  0.000  0.653  0.000  0.000  0.000 -0.713   -0.462 -0.917
##             c.TQ:GE c.TQ:GM c.T:.TQ:GE
## c.Trialnmbr                           
## c.TrialQuad                           
## GroupETP                              
## GroupMTL                              
## c.Trlnm:.TQ                           
## c.Trln:GETP                           
## c.Trln:GMTL                           
## c.TrlQ:GETP                           
## c.TrlQ:GMTL  0.504                    
## c.T:.TQ:GET  0.000   0.000            
## c.T:.TQ:GMT  0.000   0.000   0.504
```

```r

m5 <- lmer(Intrans ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * Group, data = all.data)
summary(m5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Intrans ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * Group 
##    Data: all.data 
## 
## REML criterion at convergence: 60428 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr     
##  f.id     (Intercept) 0.057    0.239             
##           GroupETP    0.208    0.456    1.00     
##           GroupMTL    0.774    0.880    0.08 0.10
##  Residual             1.873    1.369             
## Number of obs: 17290, groups: f.id, 91
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                         4.95e-01   5.14e-02    9.63
## c.Trialnumber                       6.33e-04   8.26e-04    0.77
## c.TrialQuad                         3.46e-06   6.74e-06    0.51
## GroupETP                            2.66e-01   1.40e-01    1.90
## GroupMTL                            6.83e-01   1.77e-01    3.86
## c.Trialnumber:c.TrialQuad          -1.98e-07   1.40e-07   -1.42
## c.Trialnumber:GroupETP              1.27e-03   1.17e-03    1.09
## c.Trialnumber:GroupMTL             -4.37e-04   1.16e-03   -0.38
## c.TrialQuad:GroupETP                1.02e-05   9.53e-06    1.07
## c.TrialQuad:GroupMTL                1.15e-05   9.45e-06    1.22
## c.Trialnumber:c.TrialQuad:GroupETP -2.96e-07   1.98e-07   -1.50
## c.Trialnumber:c.TrialQuad:GroupMTL  1.15e-07   1.96e-07    0.59
## 
## Correlation of Fixed Effects:
##             (Intr) c.Trln c.TrlQ GrpETP GrpMTL c.Tr:.TQ c.T:GE c.T:GM
## c.Trialnmbr  0.000                                                   
## c.TrialQuad -0.394  0.000                                            
## GroupETP    -0.368  0.000  0.145                                     
## GroupMTL    -0.290  0.000  0.115  0.107                              
## c.Trlnm:.TQ  0.000 -0.917  0.000  0.000  0.000                       
## c.Trln:GETP  0.000 -0.707  0.000  0.000  0.000  0.648                
## c.Trln:GMTL  0.000 -0.713  0.000  0.000  0.000  0.653    0.504       
## c.TrlQ:GETP  0.279  0.000 -0.707 -0.205 -0.081  0.000    0.000  0.000
## c.TrlQ:GMTL  0.281  0.000 -0.713 -0.104 -0.161  0.000    0.000  0.000
## c.T:.TQ:GET  0.000  0.648  0.000  0.000  0.000 -0.707   -0.917 -0.462
## c.T:.TQ:GMT  0.000  0.653  0.000  0.000  0.000 -0.713   -0.462 -0.917
##             c.TQ:GE c.TQ:GM c.T:.TQ:GE
## c.Trialnmbr                           
## c.TrialQuad                           
## GroupETP                              
## GroupMTL                              
## c.Trlnm:.TQ                           
## c.Trln:GETP                           
## c.Trln:GMTL                           
## c.TrlQ:GETP                           
## c.TrlQ:GMTL  0.504                    
## c.T:.TQ:GET  0.000   0.000            
## c.T:.TQ:GMT  0.000   0.000   0.504
```

```r

all.data <- ddply(all.data, .(f.id), mutate, PofIntrans = Intrans/sum(Intrans))

m6 <- lmer(PofIntrans ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * Group, 
    data = all.data)
summary(m6)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: PofIntrans ~ (Group | f.id) + c.Trialnumber * c.TrialQuad * Group 
##    Data: all.data 
## 
## REML criterion at convergence: -104553 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr       
##  f.id     (Intercept) 1.36e-15 3.69e-08            
##           GroupETP    7.81e-15 8.84e-08 -1.00      
##           GroupMTL    1.70e-14 1.30e-07 -0.31  0.31
##  Residual             1.36e-04 1.17e-02            
## Number of obs: 17290, groups: f.id, 91
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                         5.05e-03   2.32e-04   21.80
## c.Trialnumber                       1.13e-06   7.04e-06    0.16
## c.TrialQuad                         7.08e-08   5.74e-08    1.23
## GroupETP                           -4.19e-05   3.28e-04   -0.13
## GroupMTL                            1.07e-04   3.25e-04    0.33
## c.Trialnumber:c.TrialQuad          -6.01e-10   1.19e-09   -0.50
## c.Trialnumber:GroupETP              1.40e-05   9.96e-06    1.41
## c.Trialnumber:GroupMTL             -6.00e-06   9.87e-06   -0.61
## c.TrialQuad:GroupETP                1.39e-08   8.12e-08    0.17
## c.TrialQuad:GroupMTL               -3.54e-08   8.05e-08   -0.44
## c.Trialnumber:c.TrialQuad:GroupETP -2.98e-09   1.69e-09   -1.77
## c.Trialnumber:c.TrialQuad:GroupMTL  8.43e-10   1.67e-09    0.50
## 
## Correlation of Fixed Effects:
##             (Intr) c.Trln c.TrlQ GrpETP GrpMTL c.Tr:.TQ c.T:GE c.T:GM
## c.Trialnmbr  0.000                                                   
## c.TrialQuad -0.745  0.000                                            
## GroupETP    -0.707  0.000  0.527                                     
## GroupMTL    -0.713  0.000  0.531  0.504                              
## c.Trlnm:.TQ  0.000 -0.917  0.000  0.000  0.000                       
## c.Trln:GETP  0.000 -0.707  0.000  0.000  0.000  0.648                
## c.Trln:GMTL  0.000 -0.713  0.000  0.000  0.000  0.653    0.504       
## c.TrlQ:GETP  0.527  0.000 -0.707 -0.745 -0.376  0.000    0.000  0.000
## c.TrlQ:GMTL  0.531  0.000 -0.713 -0.376 -0.745  0.000    0.000  0.000
## c.T:.TQ:GET  0.000  0.648  0.000  0.000  0.000 -0.707   -0.917 -0.462
## c.T:.TQ:GMT  0.000  0.653  0.000  0.000  0.000 -0.713   -0.462 -0.917
##             c.TQ:GE c.TQ:GM c.T:.TQ:GE
## c.Trialnmbr                           
## c.TrialQuad                           
## GroupETP                              
## GroupMTL                              
## c.Trlnm:.TQ                           
## c.Trln:GETP                           
## c.Trln:GMTL                           
## c.TrlQ:GETP                           
## c.TrlQ:GMTL  0.504                    
## c.T:.TQ:GET  0.000   0.000            
## c.T:.TQ:GMT  0.000   0.000   0.504
```

```r

ggplot(data = all.data, aes(x = Trialnumber, y = Intrans, group = Group, col = Group)) + 
    geom_smooth() + theme_classic() + ylab("Number of Times involved in Intransitivity") + 
    xlab("Trial Number") + scale_color_discrete(breaks = c("C", "ETP", "MTL"), 
    labels = c("Control", "ETL", "MTL"))
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-331.png) 

```r

plotLMER.fnc(m5, pred = "c.Trialnumber", intr = list("Group", c("C", "ETP", 
    "MTL"), "end"), addToExistingPlot = F)
```

```
## Error: could not find function "plotLMER.fnc"
```

```r

# pred.m5 <- predict(m5) all.data2 <- cbind(all.data, pred.m5)

# Plot random effects
dotplot(ranef(m5, condVar = TRUE))
```

```
## $f.id
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-332.png) 


7. Testing Luce choice model
  * A model with subject + 190 pairs + brand
  * If there aren't any intransitivities this should fit perfectly
  * I don't think I'm doing the right thing here..  
  * According to these slides [http://www.r-project.org/conferences/useR-2008/slides/Wickelmaier.pdf] (http://www.r-project.org/conferences/useR-2008/slides/Wickelmaier.pdf)




### Prepare data for eba package


```r
Data$Choice.left0.right1 <- ifelse(Data$Choice_1left_4right == 1, 0, 1)
Data2 <- Data
Data2$chose1 <- ifelse(Data2$Choice.image == 1, 1, 0)
Data2$chose2 <- ifelse(Data2$Choice.image == 2, 1, 0)
Data2$chose3 <- ifelse(Data2$Choice.image == 3, 1, 0)
Data2$chose4 <- ifelse(Data2$Choice.image == 4, 1, 0)
Data2$chose5 <- ifelse(Data2$Choice.image == 5, 1, 0)
Data2$chose6 <- ifelse(Data2$Choice.image == 6, 1, 0)
Data2$chose7 <- ifelse(Data2$Choice.image == 7, 1, 0)
Data2$chose8 <- ifelse(Data2$Choice.image == 8, 1, 0)
Data2$chose9 <- ifelse(Data2$Choice.image == 9, 1, 0)
Data2$chose10 <- ifelse(Data2$Choice.image == 10, 1, 0)
Data2$chose11 <- ifelse(Data2$Choice.image == 11, 1, 0)
Data2$chose12 <- ifelse(Data2$Choice.image == 12, 1, 0)
Data2$chose13 <- ifelse(Data2$Choice.image == 13, 1, 0)
Data2$chose14 <- ifelse(Data2$Choice.image == 14, 1, 0)
Data2$chose15 <- ifelse(Data2$Choice.image == 15, 1, 0)
Data2$chose16 <- ifelse(Data2$Choice.image == 16, 1, 0)
Data2$chose17 <- ifelse(Data2$Choice.image == 17, 1, 0)
Data2$chose18 <- ifelse(Data2$Choice.image == 18, 1, 0)
Data2$chose19 <- ifelse(Data2$Choice.image == 19, 1, 0)
Data2$chose20 <- ifelse(Data2$Choice.image == 20, 1, 0)

# Separate choice data in to groups

Data.mtl <- Data2[Data2$Group == "MTL", ]
Data.c <- Data2[Data2$Group == "C", ]
Data.etp <- Data2[Data2$Group == "ETP", ]

# Create three matrices tabulating pairwise counts

test <- aggregate(Data.mtl[, 15:34], by = list(id = Data.mtl$Trial), FUN = "sum")
# test <- aggregate(Data.mtl[,15:33],by=list(id=Data.mtl$Trial),FUN='sum')
# library(reshape) #Done once TM.m <- melt(TrialNumber[,c('Trial',
# 'Paste')], id = c('Trial')) TM.m <- TM.m[order(TM.m$Trial),] TM.m$Ones <-
# rep(c(1,2),190) TM.m <- TM.m[TM.m$Ones == 1,] Add pair info to test
test <- merge(TM.m[, c("Trial", "value")], test, by.x = "Trial", by.y = "id")

matrix <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("chose1", 
    "chose2", "chose3", "chose4", "chose5", "chose6", "chose7", "chose8", "chose9", 
    "chose10", "chose11", "chose12", "chose13", "chose14", "chose15", "chose16", 
    "chose17", "chose18", "chose19", "chose20"), c("opp1", "opp2", "opp3", "opp4", 
    "opp5", "opp6", "opp7", "opp8", "opp9", "opp10", "opp11", "opp12", "opp13", 
    "opp14", "opp15", "opp16", "opp17", "opp18", "opp19", "opp20")))

for (i in 1:20) {
    matrix[i, i] <- 0
}

ref <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("01", "02", 
    "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", 
    "15", "16", "17", "18", "19", "20"), c("01", "02", "03", "04", "05", "06", 
    "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
    "19", "20")))

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

matrix.mtl <- matrix

test <- aggregate(Data.etp[, 15:34], by = list(id = Data.etp$Trial), FUN = "sum")

test <- merge(TM.m[, c("Trial", "value")], test, by.x = "Trial", by.y = "id")

matrix <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("chose1", 
    "chose2", "chose3", "chose4", "chose5", "chose6", "chose7", "chose8", "chose9", 
    "chose10", "chose11", "chose12", "chose13", "chose14", "chose15", "chose16", 
    "chose17", "chose18", "chose19", "chose20"), c("opp1", "opp2", "opp3", "opp4", 
    "opp5", "opp6", "opp7", "opp8", "opp9", "opp10", "opp11", "opp12", "opp13", 
    "opp14", "opp15", "opp16", "opp17", "opp18", "opp19", "opp20")))

for (i in 1:20) {
    matrix[i, i] <- 0
}

ref <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("01", "02", 
    "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", 
    "15", "16", "17", "18", "19", "20"), c("01", "02", "03", "04", "05", "06", 
    "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
    "19", "20")))

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

matrix.etp <- matrix

test <- aggregate(Data.c[, 15:34], by = list(id = Data.c$Trial), FUN = "sum")

test <- merge(TM.m[, c("Trial", "value")], test, by.x = "Trial", by.y = "id")

matrix <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("chose1", 
    "chose2", "chose3", "chose4", "chose5", "chose6", "chose7", "chose8", "chose9", 
    "chose10", "chose11", "chose12", "chose13", "chose14", "chose15", "chose16", 
    "chose17", "chose18", "chose19", "chose20"), c("opp1", "opp2", "opp3", "opp4", 
    "opp5", "opp6", "opp7", "opp8", "opp9", "opp10", "opp11", "opp12", "opp13", 
    "opp14", "opp15", "opp16", "opp17", "opp18", "opp19", "opp20")))

for (i in 1:20) {
    matrix[i, i] <- 0
}

ref <- matrix(data = NA, nrow = 20, ncol = 20, dimnames = list(c("01", "02", 
    "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", 
    "15", "16", "17", "18", "19", "20"), c("01", "02", "03", "04", "05", "06", 
    "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
    "19", "20")))

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

matrix.c <- matrix
```


### Fitting BTL models using the eba package  

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
## Weak             39     0.0342  0.0285  0.113      7.9 19    0.988
## Moderate        135     0.1184  0.0585  0.226       NA NA       NA
## Strong          456     0.4000  0.0813  0.258       NA NA       NA
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
## Weak              2    0.00175  0.0333 0.0333     4.69  5    0.454
## Moderate         54    0.04737  0.0500 0.2000       NA NA       NA
## Strong          286    0.25088  0.0697 0.2333       NA NA       NA
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
## Weak             13     0.0114  0.0359 0.0667     4.42  8    0.817
## Moderate         97     0.0851  0.0505 0.1333       NA NA       NA
## Strong          353     0.3096  0.0769 0.2667       NA NA       NA
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
## 1   0.04923    0.00395    12.4   <2e-16 ***
## 2   0.07593    0.00606    12.5   <2e-16 ***
## 3   0.02517    0.00219    11.5   <2e-16 ***
## 4   0.04394    0.00356    12.4   <2e-16 ***
## 5   0.03762    0.00309    12.2   <2e-16 ***
## 6   0.02574    0.00223    11.6   <2e-16 ***
## 7   0.04698    0.00378    12.4   <2e-16 ***
## 8   0.06483    0.00517    12.6   <2e-16 ***
## 9   0.05228    0.00419    12.5   <2e-16 ***
## 10  0.04108    0.00334    12.3   <2e-16 ***
## 11  0.04923    0.00395    12.4   <2e-16 ***
## 12  0.05978    0.00477    12.5   <2e-16 ***
## 13  0.07863    0.00628    12.5   <2e-16 ***
## 14  0.06662    0.00531    12.6   <2e-16 ***
## 15  0.06754    0.00538    12.6   <2e-16 ***
## 16  0.02788    0.00238    11.7   <2e-16 ***
## 17  0.05590    0.00447    12.5   <2e-16 ***
## 18  0.02788    0.00238    11.7   <2e-16 ***
## 19  0.09827    0.00791    12.4   <2e-16 ***
## 20  0.06225    0.00496    12.5   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2 logLik1 logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1117.7  -863.3    508.7  9.2e-06 ***
## EBA        19 190  -410.6  -362.0     97.1        1    
## Effect      0  19  -616.4  -410.6    411.6  < 2e-16 ***
## Imbalance   1 190  -501.3  -501.3      0.0        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  859.1 
## Pearson X2: 96.1
```

```r

btl.etp <- eba(matrix.etp)
summary(btl.etp)
```

```
## 
## Parameter estimates:
##    Estimate Std. Error z value Pr(>|z|)    
## 1   0.04988    0.00411    12.1   <2e-16 ***
## 2   0.08775    0.00724    12.1   <2e-16 ***
## 3   0.01778    0.00169    10.6   <2e-16 ***
## 4   0.02712    0.00237    11.4   <2e-16 ***
## 5   0.02631    0.00231    11.4   <2e-16 ***
## 6   0.02651    0.00233    11.4   <2e-16 ***
## 7   0.07723    0.00635    12.2   <2e-16 ***
## 8   0.09189    0.00760    12.1   <2e-16 ***
## 9   0.05951    0.00489    12.2   <2e-16 ***
## 10  0.03683    0.00310    11.9   <2e-16 ***
## 11  0.05785    0.00475    12.2   <2e-16 ***
## 12  0.04715    0.00390    12.1   <2e-16 ***
## 13  0.05623    0.00462    12.2   <2e-16 ***
## 14  0.06346    0.00521    12.2   <2e-16 ***
## 15  0.05023    0.00414    12.1   <2e-16 ***
## 16  0.02419    0.00216    11.2   <2e-16 ***
## 17  0.03953    0.00331    11.9   <2e-16 ***
## 18  0.03553    0.00300    11.8   <2e-16 ***
## 19  0.09478    0.00785    12.1   <2e-16 ***
## 20  0.06771    0.00556    12.2   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2 logLik1 logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1186.0  -853.9    664.2   <2e-16 ***
## EBA        19 190  -393.8  -355.7     76.3        1    
## Effect      0  19  -687.8  -393.8    587.9   <2e-16 ***
## Imbalance   1 190  -498.2  -498.2      0.0        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  825.6 
## Pearson X2: 75.7
```

```r

btl.c <- eba(matrix.c)
summary(btl.c)
```

```
## 
## Parameter estimates:
##    Estimate Std. Error z value Pr(>|z|)    
## 1   0.04792    0.00397    12.1   <2e-16 ***
## 2   0.06090    0.00500    12.2   <2e-16 ***
## 3   0.01861    0.00176    10.6   <2e-16 ***
## 4   0.03688    0.00312    11.8   <2e-16 ***
## 5   0.02341    0.00211    11.1   <2e-16 ***
## 6   0.02453    0.00220    11.2   <2e-16 ***
## 7   0.07340    0.00603    12.2   <2e-16 ***
## 8   0.05105    0.00422    12.1   <2e-16 ***
## 9   0.05634    0.00464    12.2   <2e-16 ***
## 10  0.04826    0.00400    12.1   <2e-16 ***
## 11  0.05962    0.00490    12.2   <2e-16 ***
## 12  0.06311    0.00518    12.2   <2e-16 ***
## 13  0.06090    0.00500    12.2   <2e-16 ***
## 14  0.07957    0.00654    12.2   <2e-16 ***
## 15  0.04282    0.00357    12.0   <2e-16 ***
## 16  0.03877    0.00326    11.9   <2e-16 ***
## 17  0.05401    0.00445    12.1   <2e-16 ***
## 18  0.02322    0.00210    11.1   <2e-16 ***
## 19  0.10558    0.00878    12.0   <2e-16 ***
## 20  0.10815    0.00901    12.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Model tests:
##           Df1 Df2 logLik1 logLik2 Deviance Pr(>Chi)    
## Overall     1 380 -1194.7  -853.5    682.4   <2e-16 ***
## EBA        19 190  -401.8  -355.2     93.1        1    
## Effect      0  19  -696.4  -401.8    589.3   <2e-16 ***
## Imbalance   1 190  -498.2  -498.2      0.0        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## AIC:  841.5 
## Pearson X2: 92.2
```

```r

anova(btl.mtl, btl.etp, btl.c)
```

```
## Analysis of deviance table for elimination-by-aspect models
## 
##     Model Resid. df Resid. Dev   Test    Df LR stat. Pr(>Chi)
## 1 btl.mtl       171       97.1                               
## 2 btl.etp       171       76.3 1 vs 2     0     20.9        0
## 3   btl.c       171       93.1 2 vs 3     0    -16.9        1
```

```r
anova(btl.c, btl.etp, btl.mtl)
```

```
## Analysis of deviance table for elimination-by-aspect models
## 
##     Model Resid. df Resid. Dev   Test    Df LR stat. Pr(>Chi)
## 1   btl.c       171       93.1                               
## 2 btl.etp       171       76.3 1 vs 2     0     16.9        0
## 3 btl.mtl       171       97.1 2 vs 3     0    -20.9        1
```

```r

array <- array(c(matrix.mtl, matrix.etp, matrix.c), c(20, 20, 3))

group.test(array)
```

```
## 
## Testing for group effects in EBA models:
## 
##            Df1  Df2  logLik1  logLik2 Deviance Pr(>Chi)    
## Overall      1 1140 -3500.44 -2570.69  1859.50   <2e-16 ***
## EBA.g       57  570 -1206.14 -1072.88   266.53        1    
## Group       19   57 -1306.13 -1206.14   199.98   <2e-16 ***
## Effect       0   19 -2000.55 -1306.13  1388.83   <2e-16 ***
## Imbalance    1  570 -1499.90 -1497.82     4.16        1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


### How many times was each bar chosen/ Effect on intransitivity



```r
agg.Data2 <- aggregate(Data2[, c("chose1", "chose2", "chose3", "chose4", "chose5", 
    "chose6", "chose7", "chose8", "chose9", "chose10", "chose11", "chose12", 
    "chose13", "chose14", "chose15", "chose16", "chose17", "chose18", "chose19", 
    "chose20")], by = list(f.id = Data2$SubjectID, Group = Data2$Group), FUN = "sum")

melt.Data2 <- melt(agg.Data2, id = c("f.id", "Group"))
# Significant differences in how often each bar is preferred
summary(aov(value ~ variable, data = melt.Data2))
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)    
## variable      19   6716     353    14.4 <2e-16 ***
## Residuals   1800  44029      24                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
print(model.tables(aov(value ~ variable, data = melt.Data2), digits = 3))
```

```
## Tables of effects
## 
##  variable 
## variable
##  chose1  chose2  chose3  chose4  chose5  chose6  chose7  chose8  chose9 
##   0.071   2.005  -3.896  -1.456  -2.423  -2.940   1.324   1.544   0.698 
## chose10 chose11 chose12 chose13 chose14 chose15 chose16 chose17 chose18 
##  -0.687   0.643   0.731   1.390   1.720   0.423  -2.269   0.104  -2.456 
## chose19 chose20 
##   3.324   2.148
```

```r

# But posthoc hard to say which is preffered most TukeyHSD(aov(value ~
# variable, data = melt.Data2), conf.level = 0.95)
pairwise.t.test(melt.Data2$value, melt.Data2$variable, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  melt.Data2$value and melt.Data2$variable 
## 
##         chose1  chose2  chose3  chose4  chose5  chose6  chose7  chose8 
## chose2  1.00000 -       -       -       -       -       -       -      
## chose3  1.4e-05 2.9e-13 -       -       -       -       -       -      
## chose4  1.00000 0.00048 0.17000 -       -       -       -       -      
## chose5  0.12979 3.5e-07 1.00000 1.00000 -       -       -       -      
## chose6  0.00797 3.9e-09 1.00000 1.00000 1.00000 -       -       -      
## chose7  1.00000 1.00000 3.0e-10 0.02935 6.7e-05 1.4e-06 -       -      
## chose8  1.00000 1.00000 3.4e-11 0.00850 1.4e-05 2.2e-07 1.00000 -      
## chose9  1.00000 1.00000 8.9e-08 0.63649 0.00415 0.00015 1.00000 1.00000
## chose10 1.00000 0.04707 0.00242 1.00000 1.00000 0.40942 1.00000 0.45225
## chose11 1.00000 1.00000 1.4e-07 0.80749 0.00577 0.00021 1.00000 1.00000
## chose12 1.00000 1.00000 6.7e-08 0.55045 0.00340 0.00012 1.00000 1.00000
## chose13 1.00000 1.00000 1.6e-10 0.02041 4.2e-05 8.0e-07 1.00000 1.00000
## chose14 1.00000 1.00000 5.8e-12 0.00297 3.5e-06 5.0e-08 1.00000 1.00000
## chose15 1.00000 1.00000 8.7e-07 1.00000 0.02041 0.00092 1.00000 1.00000
## chose16 0.27275 1.2e-06 1.00000 1.00000 1.00000 1.00000 0.00020 4.2e-05
## chose17 1.00000 1.00000 1.1e-05 1.00000 0.11012 0.00657 1.00000 1.00000
## chose18 0.11012 2.7e-07 1.00000 1.00000 1.00000 1.00000 5.3e-05 1.1e-05
## chose19 0.00184 1.00000 < 2e-16 1.7e-08 1.5e-12 5.2e-15 1.00000 1.00000
## chose20 0.88685 1.00000 6.1e-14 0.00018 1.1e-07 1.0e-09 1.00000 1.00000
##         chose9  chose10 chose11 chose12 chose13 chose14 chose15 chose16
## chose2  -       -       -       -       -       -       -       -      
## chose3  -       -       -       -       -       -       -       -      
## chose4  -       -       -       -       -       -       -       -      
## chose5  -       -       -       -       -       -       -       -      
## chose6  -       -       -       -       -       -       -       -      
## chose7  -       -       -       -       -       -       -       -      
## chose8  -       -       -       -       -       -       -       -      
## chose9  -       -       -       -       -       -       -       -      
## chose10 1.00000 -       -       -       -       -       -       -      
## chose11 1.00000 1.00000 -       -       -       -       -       -      
## chose12 1.00000 1.00000 1.00000 -       -       -       -       -      
## chose13 1.00000 0.88685 1.00000 1.00000 -       -       -       -      
## chose14 1.00000 0.19938 1.00000 1.00000 1.00000 -       -       -      
## chose15 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -      
## chose16 0.01029 1.00000 0.01409 0.00850 0.00013 1.1e-05 0.04707 -      
## chose17 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 0.23341
## chose18 0.00340 1.00000 0.00474 0.00278 3.3e-05 2.7e-06 0.01698 1.00000
## chose19 0.06650 9.7e-06 0.04988 0.07882 1.00000 1.00000 0.01500 7.3e-12
## chose20 1.00000 0.02170 1.00000 1.00000 1.00000 1.00000 1.00000 3.9e-07
##         chose17 chose18 chose19
## chose2  -       -       -      
## chose3  -       -       -      
## chose4  -       -       -      
## chose5  -       -       -      
## chose6  -       -       -      
## chose7  -       -       -      
## chose8  -       -       -      
## chose9  -       -       -      
## chose10 -       -       -      
## chose11 -       -       -      
## chose12 -       -       -      
## chose13 -       -       -      
## chose14 -       -       -      
## chose15 -       -       -      
## chose16 -       -       -      
## chose17 -       -       -      
## chose18 0.09325 -       -      
## chose19 0.00226 1.0e-12 -      
## chose20 1.00000 8.1e-08 1.00000
## 
## P value adjustment method: bonferroni
```

```r

# On trial level does the chosen bar predict how many times a trial is
# involved in intransitivities?
summary(lm(Intrans ~ as.factor(Choice.image) + Group, data = all.data))
```

```
## 
## Call:
## lm(formula = Intrans ~ as.factor(Choice.image) + Group, data = all.data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.740 -0.855 -0.441  0.327 15.970 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 0.5093     0.0540    9.43  < 2e-16 ***
## as.factor(Choice.image)2   -0.1958     0.0695   -2.82  0.00487 ** 
## as.factor(Choice.image)3    0.5137     0.0846    6.08  1.3e-09 ***
## as.factor(Choice.image)4    0.3519     0.0760    4.63  3.7e-06 ***
## as.factor(Choice.image)5    0.0949     0.0788    1.20  0.22847    
## as.factor(Choice.image)6    0.3458     0.0806    4.29  1.8e-05 ***
## as.factor(Choice.image)7   -0.0847     0.0705   -1.20  0.22974    
## as.factor(Choice.image)8   -0.1572     0.0702   -2.24  0.02517 *  
## as.factor(Choice.image)9    0.0528     0.0715    0.74  0.46070    
## as.factor(Choice.image)10   0.2063     0.0742    2.78  0.00544 ** 
## as.factor(Choice.image)11   0.0435     0.0716    0.61  0.54341    
## as.factor(Choice.image)12  -0.1589     0.0715   -2.22  0.02626 *  
## as.factor(Choice.image)13  -0.1521     0.0704   -2.16  0.03087 *  
## as.factor(Choice.image)14  -0.1380     0.0699   -1.97  0.04851 *  
## as.factor(Choice.image)15  -0.1188     0.0720   -1.65  0.09907 .  
## as.factor(Choice.image)16   0.2601     0.0783    3.32  0.00090 ***
## as.factor(Choice.image)17  -0.0242     0.0726   -0.33  0.73851    
## as.factor(Choice.image)18   0.1312     0.0789    1.66  0.09644 .  
## as.factor(Choice.image)19  -0.2588     0.0679   -3.81  0.00014 ***
## as.factor(Choice.image)20  -0.0682     0.0694   -0.98  0.32532    
## GroupETP                    0.3018     0.0284   10.61  < 2e-16 ***
## GroupMTL                    0.7166     0.0282   25.40  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.52 on 17268 degrees of freedom
## Multiple R-squared:  0.0512,	Adjusted R-squared:  0.05 
## F-statistic: 44.4 on 21 and 17268 DF,  p-value: <2e-16
```

```r
# Yes but hard to say what this could mean because the bars that apparently
# have an effect on the number of intransitivies are not those that are
# chosen most or least often or seem to have any other obvious property
# looking at how often they have been chosen
aggregate(value ~ variable, data = melt.Data2, mean)
```

```
##    variable value
## 1    chose1  9.57
## 2    chose2 11.51
## 3    chose3  5.60
## 4    chose4  8.04
## 5    chose5  7.08
## 6    chose6  6.56
## 7    chose7 10.82
## 8    chose8 11.04
## 9    chose9 10.20
## 10  chose10  8.81
## 11  chose11 10.14
## 12  chose12 10.23
## 13  chose13 10.89
## 14  chose14 11.22
## 15  chose15  9.92
## 16  chose16  7.23
## 17  chose17  9.60
## 18  chose18  7.04
## 19  chose19 12.82
## 20  chose20 11.65
```

```r
table(all.data$Choice.image)
```

```
## 
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
##  871 1047  510  732  644  597  985 1005  928  802  923  931  991 1021  903 
##   16   17   18   19   20 
##  658  874  641 1167 1060
```

