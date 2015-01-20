Transitivity SOM
========================================================




### Missed trials


```r
Intransitive$HasMissed <- ifelse(Intransitive$Missed > 0, 1, 0)
```


Is the MTL group more likely to time out?


```r
chisq.test(table(Intransitive$HasMissed, Intransitive$Group))
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table(Intransitive$HasMissed, Intransitive$Group)
## X-squared = 6.51, df = 2, p-value = 0.03849
```


Yes

Average number of missed trials:


```r
aggregate(Missed ~ Group, data = Intransitive, FUN = mean)
```

```
##   Group Missed
## 1     C  0.733
## 2   ETP  1.500
## 3   MTL  4.484
```


How much of the missed trials is 8146 responsible for?


```r
66/sum(Intransitive$Missed)
```

```
## [1] 0.32
```


Excluding 8146 are there group differences in number of timed out trials?


```r
summary(aov(Missed ~ Group, data = Intransitive[Intransitive$f.id != 8146, ]))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## Group        2     43    21.7    4.43  0.015 *
## Residuals   87    427     4.9                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Yes


```r
pairwise.t.test(Intransitive$Missed[Intransitive$f.id != 8146], Intransitive$Group[Intransitive$f.id != 
    8146], p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Intransitive$Missed[Intransitive$f.id != 8146] and Intransitive$Group[Intransitive$f.id != 8146] 
## 
##     C     ETP  
## ETP 0.551 -    
## MTL 0.011 0.319
## 
## P value adjustment method: bonferroni
```


Group average timed out trials with exclusion

```r
aggregate(Missed ~ Group, data = Intransitive[Intransitive$f.id != 8146, ], 
    FUN = mean)
```

```
##   Group Missed
## 1     C  0.733
## 2   ETP  1.500
## 3   MTL  2.433
```


### Task time


```r

# Total Task times and average reaction times excluding timed out trials
TaskTime <- ddply(all.data[all.data$RT > 0, ], .(f.id, Group), summarise, TaskTotal = sum(RT), 
    RTave = mean(RT))

TaskTime$TaskTotalMin <- TaskTime$TaskTotal/(1000 * 60)

# Exclude subject with 66 timed out trials
TaskTime <- TaskTime[TaskTime$f.id != 8146, ]
```


Average RT and TaskTime


```r
summary(all.data$RT[all.data$RT > 0])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      50     971    1310    1490    1810    8940
```

```r
sd(all.data$RT[all.data$RT > 0])
```

```
## [1] 721
```

```r

summary(TaskTime$TaskTotal)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  131000  221000  270000  279000  330000  482000
```

```r
summary(TaskTime$TaskTotalMin)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    2.18    3.69    4.50    4.65    5.50    8.04
```

```r
sd(TaskTime$TaskTotalMin)
```

```
## [1] 1.23
```


Difference in task completion total? (excluding time outs and 8146)
Group means


```r
aggregate(TaskTotal ~ Group, data = TaskTime, FUN = mean)
```

```
##   Group TaskTotal
## 1     C    268414
## 2   ETP    267483
## 3   MTL    301261
```

```r
aggregate(TaskTotalMin ~ Group, data = TaskTime, FUN = mean)
```

```
##   Group TaskTotalMin
## 1     C         4.47
## 2   ETP         4.46
## 3   MTL         5.02
```

```r

# Anova
summary(aov(TaskTotal ~ Group, data = TaskTime))
```

```
##             Df   Sum Sq  Mean Sq F value Pr(>F)
## Group        2 2.22e+10 1.11e+10     2.1   0.13
## Residuals   87 4.60e+11 5.28e+09
```

```r
summary(aov(TaskTotalMin ~ Group, data = TaskTime))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## Group        2    6.2    3.08     2.1   0.13
## Residuals   87  127.7    1.47
```


### RT on trial level

```r
# For lmer's note that each subject is nested within groups
isNested(all.data$f.id, all.data$Group)
```

```
## [1] TRUE
```

```r

#### ':' IS FOR NESTING '(1 + X | Y)' IS FOR RANDOM INTERCEPT FOR BY Y
#### ADJUSTMENTS TO THE INTERCEPTS DENOTED BY 1 AND BY Y ADJUSTMENTS TO X (X
#### BEING A FIXED EFFECT IN THE MODEL) I.E. BY Y RANDOM SLOPES FOR X

# Trial level RT group differences
t1 <- lmer(RT ~ (1 | Group:f.id) + Trialnumber, data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber and random intercept for subject (nesting specification does not add anything else since lme4 is aware of it)

t2 <- lmer(RT ~ (1 | Group:f.id) + Trialnumber * Group, data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber, group and trialnumber group interaction (ie slopes), random intercept for subject

t3 <- lmer(RT ~ (1 | Group:f.id) + Trialnumber + Group, data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber and group and random intercepts for subject

t4 <- lmer(RT ~ Trialnumber + Group + (Group | f.id), data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber and group , random intercepts for subjects and adjustments to group effect by subject (ie random slopes)

t5 <- lmer(RT ~ Trialnumber * Group + (1 | f.id), data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber, group and trialnumber group interaction (ie slopes), random intercept for subject

t6 <- lmer(RT ~ Trialnumber * Group + (Group | f.id), data = all.data[all.data$RT > 
    0 & all.data$f.id != 8146, ])  #Fixed effect for trialnumber, group and trialnumber group interaction (ie slopes), random intercept for subject and random slopes for subject on group 

anova(t1, t2, t3, t4, t5, t6)
```

```
## Warning: longer object length is not a multiple of shorter object length
```

```
## Data: [
## Data: all.data
## Data: all.data$RT > 0 & all.data$f.id != 8146
## Data: 
## Models:
## t1: RT ~ (1 | Group:f.id) + Trialnumber
## t3: RT ~ (1 | Group:f.id) + Trialnumber + Group
## t2: RT ~ (1 | Group:f.id) + Trialnumber * Group
## t5: RT ~ Trialnumber * Group + (1 | f.id)
## t4: RT ~ Trialnumber + Group + (Group | f.id)
## t6: RT ~ Trialnumber * Group + (Group | f.id)
##    Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
## t1  4 264252 264283 -132122   264244                            
## t3  6 264251 264298 -132120   264239  4.61      2        0.1 .  
## t2  8 264199 264261 -132091   264183 56.50      2    5.4e-13 ***
## t5  8 264199 264261 -132091   264183  0.00      0        1.0    
## t4 11 264261 264346 -132119   264239  0.00      3        1.0    
## t6 13 264208 264309 -132091   264182 56.51      2    5.4e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

anova(t2, t5, t6)  #T2 and t5 ARE THE SAME MODEL. LME4 UNDERSTANDS THE NESTING STRUCTURE WITHOUT SPECIFICATION.
```

```
## Warning: longer object length is not a multiple of shorter object length
```

```
## Data: [
## Data: all.data
## Data: all.data$RT > 0 & all.data$f.id != 8146
## Data: 
## Models:
## t2: RT ~ (1 | Group:f.id) + Trialnumber * Group
## t5: RT ~ Trialnumber * Group + (1 | f.id)
## t6: RT ~ Trialnumber * Group + (Group | f.id)
##    Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
## t2  8 264199 264261 -132091   264183                        
## t5  8 264199 264261 -132091   264183  0.00      0       1.00
## t6 13 264208 264309 -132091   264182  0.63      5       0.99
```

```r

# Conclusion: t2 and t5 are the same model and they have the lowest AIC and
# BIC (ie best fitting). Random slopes in t6 are not necessary. T6 is not
# significantly worse than t2/5 but is in that direction.

# Do we need the individual intercepts?
t7 <- lm(RT ~ Trialnumber * Group, data = all.data[all.data$RT > 0 & all.data$f.id != 
    8146, ])
extractAIC(t5)
```

```
## [1]      8 264199
```

```r
extractAIC(t7)
```

```
## [1]      6 221953
```

```r

# So using the t2/5: There is a fixed effect of Trialnumber (all groups get
# faster in later trials) There is a significant fixed effect for the MTL
# group (The MTL group is slower on average) The interaction for both groups
# is significant. The slope of speeding in later trials is steepest for the
# MTL group, followed by the control group and flattest for the ETL group
# There are relatively large individual differences captured by the random
# intercepts (and seen in the dotplot). Though it might actually be harder
# to determine whether it is necessary to use a multilevel model and capture
# these since the AIC for the lm (t7) is lower..

# Model based plot
plotLMER.fnc(t2, pred = "Trialnumber", intr = list("Group", c("C", "ETP", "MTL"), 
    "end", list(c("red", "green", "blue"), c(1, 1, 1))))
```

```
## effect sizes (ranges) for the interaction of  Trialnumber  and  Group :
##     Group  =  C :  479 
##     Group  =  ETP :  358 
##     Group  =  MTL :  639
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-121.png) 

```r

# Raw data plot
ggplot(data = all.data[all.data$RT > 0 & all.data$f.id != 8146, ], aes(x = Trialnumber, 
    y = RT, group = Group, col = Group)) + geom_smooth(method = "loess") + theme_classic()
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-122.png) 

```r

# Plot random effects
dotplot(ranef(t5, condVar = TRUE))
```

```
## $f.id
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-123.png) 


### Choice


```r

summary(all.data$Intrans[all.data$RT > 0])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00    0.83    1.00   17.00
```

```r
sd(all.data$Intrans[all.data$RT > 0])
```

```
## [1] 1.53
```

```r
summary(Intransitive$CleanIntr)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    21.0    37.0    49.2    57.0   267.0
```

```r
sd(Intransitive$CleanIntr)
```

```
## [1] 46.2
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
aggregate(Intrans ~ Group, data = Intransitive, sd)
```

```
##   Group Intrans
## 1     C    16.4
## 2   ETP    44.4
## 3   MTL    59.3
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
aggregate(CleanIntr ~ Group, data = Intransitive, sd)
```

```
##   Group CleanIntr
## 1     C      15.9
## 2   ETP      43.6
## 3   MTL      59.6
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


### Left


```r

left.test2 <- as.vector(rbind(rep(0, nrow(Data)/2), rep(1, nrow(Data)/2)))
t.test(Data$Choice.left1.right0[], left.test2)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data$Choice.left1.right0[] and left.test2
## t = -1.71, df = 34578, p-value = 0.08724
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.01974  0.00134
## sample estimates:
## mean of x mean of y 
##     0.491     0.500
```

```r
t.test(Data.c$Choice.left1.right0, left.test2[1:nrow(Data.c)])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.c$Choice.left1.right0 and left.test2[1:nrow(Data.c)]
## t = 1.05, df = 11398, p-value = 0.2942
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.00853  0.02818
## sample estimates:
## mean of x mean of y 
##      0.51      0.50
```

```r
t.test(Data.etp$Choice.left1.right0, left.test2[1:nrow(Data.etp)])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.etp$Choice.left1.right0 and left.test2[1:nrow(Data.etp)]
## t = -0.993, df = 11398, p-value = 0.3208
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.02766  0.00906
## sample estimates:
## mean of x mean of y 
##     0.491     0.500
```

```r
t.test(Data.mtl$Choice.left1.right0, left.test2[1:nrow(Data.mtl)])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Data.mtl$Choice.left1.right0 and left.test2[1:nrow(Data.mtl)]
## t = -2.99, df = 11778, p-value = 0.002821
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.04555 -0.00946
## sample estimates:
## mean of x mean of y 
##     0.472     0.500
```

```r

# All but one timed out trial is coded right
table(all.data$Choice.left1.right0, all.data$Error)
```

```
##    
##        0    1
##   0 8599  205
##   1 8485    1
```

```r

# Excluding time outs the effect disappears
t.test(all.data$Choice.left1.right0[all.data$Group == "MTL" & all.data$RT > 
    0], left.test2[1:nrow(all.data[all.data$Group == "MTL" & all.data$RT > 0, 
    ])])
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  all.data$Choice.left1.right0[all.data$Group == "MTL" & all.data$RT >  and left.test2[1:nrow(all.data[all.data$Group == "MTL" & all.data$RT >     0] and     0, ])]
## t = -1.72, df = 11500, p-value = 0.0862
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.03427  0.00228
## sample estimates:
## mean of x mean of y 
##     0.484     0.500
```


### Bars


```r

Bars <- as.data.frame(table(all.data$Choice.image, all.data$f.id))
names(Bars) <- c("Bars", "f.id", "Freq")
Bars$Group <- rep(NA, nrow(Bars))
for (i in 1:nrow(Bars)) {
    Bars$Group[i] <- as.character(Intransitive$Group[which(Intransitive$f.id == 
        Bars$f.id[i])])
}
Bars$Group <- as.factor(Bars$Group)
summary(lmer(Freq ~ (1 | Bars) + (1 | f.id) + Group, data = Bars))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Freq ~ (1 | Bars) + (1 | f.id) + Group 
##    Data: Bars 
## 
## REML criterion at convergence: 11037 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept)  0.00    0.00    
##  Bars     (Intercept)  3.62    1.90    
##  Residual             24.49    4.95    
## Number of obs: 1820, groups: f.id, 91; Bars, 20
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) 9.50e+00   4.71e-01    20.2
## GroupETP    4.55e-15   2.86e-01     0.0
## GroupMTL    3.66e-15   2.83e-01     0.0
## 
## Correlation of Fixed Effects:
##          (Intr) GrpETP
## GroupETP -0.303       
## GroupMTL -0.306  0.504
```

```r
summary(aov(Freq ~ Bars, data = Bars))
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)    
## Bars          19   6716     353    14.4 <2e-16 ***
## Residuals   1800  44029      24                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
pairwise.t.test(Bars$Freq, Bars$Bars, p.adjust = "bonferroni")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  Bars$Freq and Bars$Bars 
## 
##    1       2       3       4       5       6       7       8       9      
## 2  1.00000 -       -       -       -       -       -       -       -      
## 3  1.4e-05 2.9e-13 -       -       -       -       -       -       -      
## 4  1.00000 0.00048 0.17000 -       -       -       -       -       -      
## 5  0.12979 3.5e-07 1.00000 1.00000 -       -       -       -       -      
## 6  0.00797 3.9e-09 1.00000 1.00000 1.00000 -       -       -       -      
## 7  1.00000 1.00000 3.0e-10 0.02935 6.7e-05 1.4e-06 -       -       -      
## 8  1.00000 1.00000 3.4e-11 0.00850 1.4e-05 2.2e-07 1.00000 -       -      
## 9  1.00000 1.00000 8.9e-08 0.63649 0.00415 0.00015 1.00000 1.00000 -      
## 10 1.00000 0.04707 0.00242 1.00000 1.00000 0.40942 1.00000 0.45225 1.00000
## 11 1.00000 1.00000 1.4e-07 0.80749 0.00577 0.00021 1.00000 1.00000 1.00000
## 12 1.00000 1.00000 6.7e-08 0.55045 0.00340 0.00012 1.00000 1.00000 1.00000
## 13 1.00000 1.00000 1.6e-10 0.02041 4.2e-05 8.0e-07 1.00000 1.00000 1.00000
## 14 1.00000 1.00000 5.8e-12 0.00297 3.5e-06 5.0e-08 1.00000 1.00000 1.00000
## 15 1.00000 1.00000 8.7e-07 1.00000 0.02041 0.00092 1.00000 1.00000 1.00000
## 16 0.27275 1.2e-06 1.00000 1.00000 1.00000 1.00000 0.00020 4.2e-05 0.01029
## 17 1.00000 1.00000 1.1e-05 1.00000 0.11012 0.00657 1.00000 1.00000 1.00000
## 18 0.11012 2.7e-07 1.00000 1.00000 1.00000 1.00000 5.3e-05 1.1e-05 0.00340
## 19 0.00184 1.00000 < 2e-16 1.7e-08 1.5e-12 5.2e-15 1.00000 1.00000 0.06650
## 20 0.88685 1.00000 6.1e-14 0.00018 1.1e-07 1.0e-09 1.00000 1.00000 1.00000
##    10      11      12      13      14      15      16      17      18     
## 2  -       -       -       -       -       -       -       -       -      
## 3  -       -       -       -       -       -       -       -       -      
## 4  -       -       -       -       -       -       -       -       -      
## 5  -       -       -       -       -       -       -       -       -      
## 6  -       -       -       -       -       -       -       -       -      
## 7  -       -       -       -       -       -       -       -       -      
## 8  -       -       -       -       -       -       -       -       -      
## 9  -       -       -       -       -       -       -       -       -      
## 10 -       -       -       -       -       -       -       -       -      
## 11 1.00000 -       -       -       -       -       -       -       -      
## 12 1.00000 1.00000 -       -       -       -       -       -       -      
## 13 0.88685 1.00000 1.00000 -       -       -       -       -       -      
## 14 0.19938 1.00000 1.00000 1.00000 -       -       -       -       -      
## 15 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -      
## 16 1.00000 0.01409 0.00850 0.00013 1.1e-05 0.04707 -       -       -      
## 17 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 0.23341 -       -      
## 18 1.00000 0.00474 0.00278 3.3e-05 2.7e-06 0.01698 1.00000 0.09325 -      
## 19 9.7e-06 0.04988 0.07882 1.00000 1.00000 0.01500 7.3e-12 0.00226 1.0e-12
## 20 0.02170 1.00000 1.00000 1.00000 1.00000 1.00000 3.9e-07 1.00000 8.1e-08
##    19     
## 2  -      
## 3  -      
## 4  -      
## 5  -      
## 6  -      
## 7  -      
## 8  -      
## 9  -      
## 10 -      
## 11 -      
## 12 -      
## 13 -      
## 14 -      
## 15 -      
## 16 -      
## 17 -      
## 18 -      
## 19 -      
## 20 1.00000
## 
## P value adjustment method: bonferroni
```



### RT predicts intransitivities?


```r

a1 <- lmer(Intrans ~ RT * Group + (1 | f.id), data = all.data[all.data$RT > 
    0, ])
summary(a1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Intrans ~ RT * Group + (1 | f.id) 
##    Data: all.data[all.data$RT > 0, ] 
## 
## REML criterion at convergence: 58455 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.506    0.711   
##  Residual             1.749    1.322   
## Number of obs: 17084, groups: f.id, 91
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept) -5.26e-02   1.39e-01   -0.38
## RT           3.91e-04   3.18e-05   12.32
## GroupETP     3.65e-01   1.95e-01    1.87
## GroupMTL     6.76e-01   1.94e-01    3.48
## RT:GroupETP -6.17e-05   4.35e-05   -1.42
## RT:GroupMTL -4.57e-05   4.17e-05   -1.10
## 
## Correlation of Fixed Effects:
##             (Intr) RT     GrpETP GrpMTL RT:GET
## RT          -0.325                            
## GroupETP    -0.709  0.231                     
## GroupMTL    -0.713  0.232  0.506              
## RT:GroupETP  0.238 -0.731 -0.316 -0.169       
## RT:GroupMTL  0.248 -0.761 -0.176 -0.324  0.556
```

```r

a2 <- lmer(Intrans ~ Group + (1 | f.id), data = all.data[all.data$RT > 0, ])
summary(a2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Intrans ~ Group + (1 | f.id) 
##    Data: all.data[all.data$RT > 0, ] 
## 
## REML criterion at convergence: 58830 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.474    0.688   
##  Residual             1.794    1.339   
## Number of obs: 17084, groups: f.id, 91
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)    0.503      0.127    3.96
## GroupETP       0.279      0.179    1.55
## GroupMTL       0.686      0.178    3.85
## 
## Correlation of Fixed Effects:
##          (Intr) GrpETP
## GroupETP -0.707       
## GroupMTL -0.713  0.504
```

```r
anova(a1, a2)
```

```
## Data: [
## Data: all.data
## Data: all.data$RT > 0
## Data: 
## Models:
## a2: Intrans ~ Group + (1 | f.id)
## a1: Intrans ~ RT * Group + (1 | f.id)
##    Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
## a2  5 58833 58872 -29411    58823                            
## a1  8 58407 58469 -29196    58391   432      3     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

ggplot(data = all.data[all.data$RT > 300 & all.data$RT < 5500 & all.data$f.id != 
    8146, ], aes(x = RT, y = Intrans, group = Group, col = Group)) + geom_smooth() + 
    theme_classic()
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using
## gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the
## smoothing method.
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-161.png) 

```r

ggplot(data = all.data[all.data$RT > 600 & all.data$RT < 5500 & all.data$f.id != 
    8146, ], aes(x = RT, y = Intrans, group = Group, col = Group)) + geom_smooth() + 
    theme_classic()
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using
## gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the
## smoothing method.
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-162.png) 

```r

# Looing at this plot non-linear effects
all.data$RT.quad <- all.data$RT^2

all.data$RT.c <- all.data$RT - mean(all.data$RT[all.data$RT > 0])

all.data$RT.quad.c <- all.data$RT.quad - mean(all.data$RT.quad[all.data$RT > 
    0])

a3 <- lmer(Intrans ~ RT.c * RT.quad.c * Group + (1 | f.id), data = all.data[all.data$RT > 
    600 & all.data$RT < 5500 & all.data$f.id != 8146, ])

a4 <- lmer(Intrans ~ RT.c + RT.quad.c + Group + RT.c:Group + (1 | f.id), data = all.data[all.data$RT > 
    600 & all.data$RT < 5500 & all.data$f.id != 8146, ])

# anova(a3, a4)

summary(a4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Intrans ~ RT.c + RT.quad.c + Group + RT.c:Group + (1 | f.id) 
##    Data: all.data[all.data$RT > 600 & all.data$RT < 5500 & all.data$f.id !=      8146, ] 
## 
## REML criterion at convergence: 57006 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  f.id     (Intercept) 0.508    0.713   
##  Residual             1.724    1.313   
## Number of obs: 16719, groups: f.id, 90
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    5.28e-01   1.31e-01    4.02
## RT.c           8.27e-04   6.57e-05   12.59
## RT.quad.c     -1.08e-07   1.44e-08   -7.55
## GroupETP       2.75e-01   1.86e-01    1.48
## GroupMTL       6.11e-01   1.86e-01    3.29
## RT.c:GroupETP -3.20e-05   4.35e-05   -0.73
## RT.c:GroupMTL  2.70e-05   4.28e-05    0.63
## 
## Correlation of Fixed Effects:
##             (Intr) RT.c   RT.qd. GrpETP GrpMTL RT.:GE
## RT.c         0.007                                   
## RT.quad.c    0.001 -0.876                            
## GroupETP    -0.707 -0.003 -0.003                     
## GroupMTL    -0.707 -0.014  0.010  0.500              
## RT.c:GrpETP -0.011 -0.314 -0.044  0.014  0.007       
## RT.c:GrpMTL -0.011 -0.226 -0.151  0.008 -0.006  0.548
```

```r

a4b <- lmer(Intrans ~ RT.c + RT.quad.c + Group + RT.c:Group + (1 | f.id), data = all.data[all.data$RT > 
    0, ])
a2b <- lmer(Intrans ~ Group + (1 | f.id), data = all.data[all.data$RT > 0, ])

anova(a4b, a2b)
```

```
## Data: [
## Data: all.data
## Data: all.data$RT > 0
## Data: 
## Models:
## a2b: Intrans ~ Group + (1 | f.id)
## a4b: Intrans ~ RT.c + RT.quad.c + Group + RT.c:Group + (1 | f.id)
##     Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
## a2b  5 58833 58872 -29411    58823                            
## a4b  9 58361 58431 -29172    58343   480      4     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

# plotLMER.fnc (a1, pred = 'RT', intr = list('Group', c('C', 'ETP', 'MTL'),
# 'end', list(c('red', 'green', 'blue'), c(1,1,1))))

plotLMER.fnc(a4, pred = "RT.c", intr = list("Group", c("C", "ETP", "MTL"), "end", 
    list(c("red", "green", "blue"), c(1, 1, 1))))
```

```
## effect sizes (ranges) for the interaction of  RT.c  and  Group :
##     Group  =  C :  3.64 
##     Group  =  ETP :  3.5 
##     Group  =  MTL :  3.76
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-163.png) 






