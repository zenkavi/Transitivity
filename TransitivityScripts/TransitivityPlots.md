Visualizing group differences in intransitivity
========================================================




#### Problem: How to visualize the difference in intransitivities between groups. (i.e. how to visualize the Kruskal Wallis test). 

As a reminder the Kruskal Wallis test and the following Wilcoxon rank sum test show the significant group differences.

Note that this is still *not* a paired test (i.e. measures are not from same sample in different conditions)


```r
kruskal.test(CleanPercentIntr ~ as.factor(Group), data = Intransitive)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  CleanPercentIntr by as.factor(Group)
## Kruskal-Wallis chi-squared = 16.2, df = 2, p-value = 0.0003069
```

```r

pairwise.wilcox.test(Intransitive$CleanPercentIntr, Intransitive$Group)
```

```
## Warning: cannot compute exact p-value with ties Warning: cannot compute
## exact p-value with ties Warning: cannot compute exact p-value with ties
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test 
## 
## data:  Intransitive$CleanPercentIntr and Intransitive$Group 
## 
##     C       ETP    
## ETP 0.13531 -      
## MTL 0.00026 0.02413
## 
## P value adjustment method: holm
```


Typically this is done by using boxplots.

Trying a boxplot on our aggregate data using the (Clean - timed out trials removed) Percent of Intransitivies per person as DV.


```r
ggplot(data = Intransitive, aes(x = Group, y = CleanPercentIntr)) + geom_boxplot() + 
    theme_classic()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Even excluding the outliers:


```r
ggplot(data = Intransitive[Intransitive$CleanPercentIntr < 12, ], aes(x = Group, 
    y = CleanPercentIntr)) + geom_boxplot() + theme_classic()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


**The large overlaps in ranges make it difficult to detect the differences at first glance.**

#### Try 1: Does Wilcox rank sum test provide confidence intervals

The function wilcox.test has a "conf.int" argument. But it can only compare two groups at a time. So the data needs to be subsetted to pass to this function.


```r
# Compare MTL and C groups
wilcox.test(CleanPercentIntr ~ Group, data = Intransitive[Intransitive$Group != 
    "ETP", ], conf.int = T)
```

```
## Warning: cannot compute exact p-value with ties Warning: cannot compute
## exact confidence intervals with ties
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  CleanPercentIntr by Group
## W = 192, p-value = 8.702e-05
## alternative hypothesis: true location shift is not equal to 0
## 95 percent confidence interval:
##  -3.48 -1.11
## sample estimates:
## difference in location 
##                  -2.16
```

```r
# Compare ETL and C groups
wilcox.test(CleanPercentIntr ~ Group, data = Intransitive[Intransitive$Group != 
    "MTL", ], conf.int = T)
```

```
## Warning: cannot compute exact p-value with ties Warning: cannot compute
## exact confidence intervals with ties
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  CleanPercentIntr by Group
## W = 348, p-value = 0.1353
## alternative hypothesis: true location shift is not equal to 0
## 95 percent confidence interval:
##  -1.824  0.255
## sample estimates:
## difference in location 
##                 -0.608
```

```r
# Compare MTL and ETL groups
wilcox.test(CleanPercentIntr ~ Group, data = Intransitive[Intransitive$Group != 
    "C", ], conf.int = T)
```

```
## Warning: cannot compute exact p-value with ties Warning: cannot compute
## exact confidence intervals with ties
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  CleanPercentIntr by Group
## W = 290, p-value = 0.01206
## alternative hypothesis: true location shift is not equal to 0
## 95 percent confidence interval:
##  -2.900 -0.276
## sample estimates:
## difference in location 
##                  -1.56
```


As clarified in the help for wilcox.test function changing the conf.int argument to true "in the two-sample case the estimator for the difference in location parameters does not estimate the difference in medians (a common misconception) but rather the median of the difference between a sample from x and a sample from y."

So it is unclear (to me) how these can be converted in to CIs that we can use for plots.

#### Try 2: Bootstrapping the median and plotting standard errors (subject level)


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

# summary(b.mtl$medians) summary(b.etp$medians) summary(b.c$medians)

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
    axis.title = element_text(size = 14, face = "bold")) + ylim(0, 7)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


#### Try 3: Bootstrapping the median and creating confidence intervals (subject level)


```r

# Confidence intervals Order medians
medians.mtl <- b.mtl$medians
medians.mtl <- medians.mtl[order(medians.mtl)]
medians.etp <- b.etp$medians
medians.etp <- medians.etp[order(medians.etp)]
medians.c <- b.c$medians
medians.c <- medians.c[order(medians.c)]

# Add 2.5th and 97.5 percentile
median.intr$ci.low[median.intr$Group == "C"] <- medians.c[25]
median.intr$ci.low[median.intr$Group == "MTL"] <- medians.mtl[25]
median.intr$ci.low[median.intr$Group == "ETL"] <- medians.etp[25]

median.intr$ci.high[median.intr$Group == "C"] <- medians.c[975]
median.intr$ci.high[median.intr$Group == "MTL"] <- medians.mtl[975]
median.intr$ci.high[median.intr$Group == "ETL"] <- medians.etp[975]

ggplot(data = median.intr, aes(x = Group, y = PercentIntr, ymin = ci.low, ymax = ci.high)) + 
    geom_point() + theme_classic() + geom_errorbar() + xlab("") + ylab("Percentage of Intransitivities") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14, 
        face = "bold")) + ylim(0, 8)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


#### Try 4: Triplet level raw data 

Darker top right sqaure indicates more intransitive triplets for the MTL group but it is a rather ugly graph and hard to quantify anything on it.


```r
g1 <- ggplot(data = all[all$Err == 0, ], aes(x = Group, y = Intrans, group = Group)) + 
    geom_boxplot() + geom_jitter() + theme_classic()
g1
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


#### Try 5: Triplet level model based plot manually calculating CIs from coefficients

The simplest multilevel model using intransitivities as DV without inflating the number is on triplet level including random intercepts for subjects and slopes for groups (main effect).

DV: Probability of a triplet being involved in intransitivity (multiplied by 100 for ease of reading)

Problem: Though the ETL vs MTL difference is significant there is still overlap


```r
all$Group <- as.factor(all$Group)
# Subset triplets without any timed out trials to avoid problems later w the
# confint function
b1.data <- all[all$Err == 0, ]
b1 <- lmer(Intrans ~ (1 | f.id) + Group, data = b1.data, family = binomial(link = "logit"))
```

```
## Warning: calling lmer with 'family' is deprecated; please use glmer()
## instead
```

```r
summary(b1)
```

```
## Generalized linear mixed model fit by maximum likelihood ['glmerMod']
##  Family: binomial ( logit )
## Formula: Intrans ~ (1 | f.id) + Group 
##    Data: b1.data 
## 
##      AIC      BIC   logLik deviance 
##    34054    34092   -17023    34046 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  f.id   (Intercept) 0.477    0.691   
## Number of obs: 100529, groups: f.id, 91
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -3.701      0.131  -28.15  < 2e-16 ***
## GroupETP       0.268      0.186    1.45     0.15    
## GroupMTL       0.787      0.183    4.31  1.7e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##          (Intr) GrpETP
## GroupETP -0.709       
## GroupMTL -0.719  0.510
```

```r

# Manually calculate coefficients and confidence intervals for plot
b1.coef <- as.data.frame(cbind(fixef(b1), confint.merMod(b1, method = "Wald")))
names(b1.coef) <- c("fixef", "ci.low", "ci.high")
b1.coef$p <- rep(NA, 3)
b1.coef$ymin <- rep(NA, 3)
b1.coef$ymax <- rep(NA, 3)
b1.coef$p[1] <- exp(b1.coef$fixef[1])/(1 + exp(b1.coef$fixef[1]))
b1.coef$p[2] <- exp(b1.coef$fixef[1] + b1.coef$fixef[2])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$fixef[2]))
b1.coef$p[3] <- exp(b1.coef$fixef[1] + b1.coef$fixef[3])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$fixef[3]))
b1.coef$ymin[1] <- exp(b1.coef$ci.low[1])/(1 + exp(b1.coef$ci.low[1]))
b1.coef$ymax[1] <- exp(b1.coef$ci.high[1])/(1 + exp(b1.coef$ci.high[1]))
b1.coef$ymin[2] <- exp(b1.coef$fixef[1] + b1.coef$ci.low[2])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$ci.low[2]))
b1.coef$ymax[2] <- exp(b1.coef$fixef[1] + b1.coef$ci.high[2])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$ci.high[2]))
b1.coef$ymin[3] <- exp(b1.coef$fixef[1] + b1.coef$ci.low[3])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$ci.low[3]))
b1.coef$ymax[3] <- exp(b1.coef$fixef[1] + b1.coef$ci.high[3])/(1 + exp(b1.coef$fixef[1] + 
    b1.coef$ci.high[3]))

b1.coef$Group <- as.factor(c("C", "ETL", "MTL"))

# Model based plot
ggplot(data = b1.coef, aes(x = Group, y = p * 100, ymin = ymin * 100, ymax = ymax * 
    100)) + geom_point() + theme_classic() + geom_errorbar(ymin = b1.coef$ymin * 
    100, ymax = b1.coef$ymax * 100) + xlab("") + ylab("P of Intransitivity of a triplet") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14, 
        face = "bold")) + ylim(0, 8)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


#### Try 6: Triplet level model based plot manually calculating CIs from coefficients changing the baseline comparison group to ETL from C 

DV: Probability of intransitive triplet (multiplied by 100)

Problem: Why lmer estimates different standard errors for groups remains unclear.


```r

# Changing the group variables contrast to make baseline comparisons to the
# ETL group
contrast.etp <- matrix(data = c(1, 0, 0, 0, 0, 1), nrow = 3, ncol = 2, byrow = T, 
    dimnames = list(c("C", "ETP", "MTL"), c("C", "MTL")))

b1.data.2 <- b1.data
contrasts(b1.data.2$Group) <- contrast.etp
b1.2 <- lmer(Intrans ~ (1 | f.id) + Group, data = b1.data.2, family = binomial(link = "logit"))
```

```
## Warning: calling lmer with 'family' is deprecated; please use glmer()
## instead
```

```r
summary(b1.2)
```

```
## Generalized linear mixed model fit by maximum likelihood ['glmerMod']
##  Family: binomial ( logit )
## Formula: Intrans ~ (1 | f.id) + Group 
##    Data: b1.data.2 
## 
##      AIC      BIC   logLik deviance 
##    34054    34092   -17023    34046 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  f.id   (Intercept) 0.477    0.691   
## Number of obs: 100529, groups: f.id, 91
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -3.433      0.131  -26.23   <2e-16 ***
## GroupC        -0.268      0.186   -1.45   0.1481    
## GroupMTL       0.519      0.182    2.85   0.0044 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##          (Intr) GroupC
## GroupC   -0.705       
## GroupMTL -0.717  0.506
```

```r


b1.2.coef <- as.data.frame(cbind(fixef(b1.2), confint.merMod(b1.2, method = "Wald")))
names(b1.2.coef) <- c("fixef", "ci.low", "ci.high")
b1.2.coef$p <- rep(NA, 3)
b1.2.coef$ymin <- rep(NA, 3)
b1.2.coef$ymax <- rep(NA, 3)
b1.2.coef$p[1] <- exp(b1.2.coef$fixef[1])/(1 + exp(b1.2.coef$fixef[1]))
b1.2.coef$p[2] <- exp(b1.2.coef$fixef[1] + b1.2.coef$fixef[2])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$fixef[2]))
b1.2.coef$p[3] <- exp(b1.2.coef$fixef[1] + b1.2.coef$fixef[3])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$fixef[3]))
b1.2.coef$ymin[1] <- exp(b1.2.coef$ci.low[1])/(1 + exp(b1.2.coef$ci.low[1]))
b1.2.coef$ymax[1] <- exp(b1.2.coef$ci.high[1])/(1 + exp(b1.2.coef$ci.high[1]))
b1.2.coef$ymin[2] <- exp(b1.2.coef$fixef[1] + b1.2.coef$ci.low[2])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$ci.low[2]))
b1.2.coef$ymax[2] <- exp(b1.2.coef$fixef[1] + b1.2.coef$ci.high[2])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$ci.high[2]))
b1.2.coef$ymin[3] <- exp(b1.2.coef$fixef[1] + b1.2.coef$ci.low[3])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$ci.low[3]))
b1.2.coef$ymax[3] <- exp(b1.2.coef$fixef[1] + b1.2.coef$ci.high[3])/(1 + exp(b1.2.coef$fixef[1] + 
    b1.2.coef$ci.high[3]))

b1.2.coef$Group <- as.factor(c("ETL", "C", "MTL"))

# Model based
ggplot(data = b1.2.coef, aes(x = Group, y = p * 100, ymin = ymin * 100, ymax = ymax * 
    100)) + geom_point() + theme_classic() + geom_errorbar(ymin = b1.2.coef$ymin * 
    100, ymax = b1.2.coef$ymax * 100) + xlab("") + ylab("Probability of intransitive triplet (%)") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 12, 
        face = "bold")) + ylim(0, 8)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


#### Try 7: Triplet level model based plot using built in function and C as baseline comparison group

DV: Probability of intransitive triplet (converted coefficients).

Problem: Can't add CIs or standard errors.


```r
plotLMER.fnc(b1, pred = "Group", fun = plogis, ylimit = c(0, 0.07), cex = 0.9, 
    xlabel = "", addlines = T)
```

```
## effect size (range) for  Group is  0.0274
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


#### Try 8: Triplet level means and standard errors

DV: Mean probability of intransitive triplet

Problem: Difference between the C and ETL groups appears very large too


```r
Group <- as.factor(c("C", "ETL", "MTL"))
se <- tapply(all$Intrans[all$Err == 0], all$Group[all$Err == 0], sd)/sqrt(tapply(all$Intrans[all$Err == 
    0], all$Group[all$Err == 0], length))
mn <- tapply(all$Intrans[all$Err == 0], all$Group[all$Err == 0], mean)
semax <- mn + se
semin <- mn - se
dat <- data.frame(cbind(Group, mn, semax, semin))
dat$Group <- c("C", "ETL", "MTL")

ggplot(dat, aes(as.factor(Group), mn)) + geom_point() + geom_errorbar(aes(ymin = semin, 
    ymax = semax, width = 0.25)) + xlab("") + ylab("Mean probability of intransitive triplet") + 
    theme_classic() + ylim(0, 0.1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


#### Try 9: Subject level means and standard errors 

DV: Mean percentage of intransitivities

Problem: Difference between the C and ETL groups appears very large too


```r

Group <- factor(c("C", "ETL", "MTL"))
se <- tapply(Intransitive$CleanPercentIntr, Intransitive$Group, sd)/tapply(Intransitive$CleanPercentIntr, 
    Intransitive$Group, length)
mn <- tapply(Intransitive$CleanPercentIntr, Intransitive$Group, mean)
semax <- mn + se
semin <- mn - se
dat <- data.frame(cbind(Group, mn, semax, semin))
dat$Group <- c("C", "ETL", "MTL")

ggplot(dat, aes(Group, mn)) + geom_point() + geom_errorbar(aes(ymin = semin, 
    ymax = semax, width = 0.25)) + xlab("") + ylab("Mean Percent of Intransitivities") + 
    theme_classic() + ylim(0, 7)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


#### Try 10: Correct way to manually estimate CIs from multilevel model (using C as baseline comparison group)

DV: Probability of intransitive triplet. Exponentiated coefficients for ease of reading.

Using the fixed effects only doesn't look bad. 


```r

newdat <- expand.grid(Group = c("C", "ETL", "MTL"), Intrans = 0)

mm <- model.matrix(terms(b1), newdat)

newdat$Intrans <- mm %*% fixef(b1)

pvar1 <- diag(mm %*% tcrossprod(vcov(b1), mm))

tvar1 <- pvar1 + VarCorr(b1)$f.id[1]  ## must be adapted for more complex models

newdat <- data.frame(newdat, plo = newdat$Intrans - 2 * sqrt(pvar1), phi = newdat$Intrans + 
    2 * sqrt(pvar1), tlo = newdat$Intrans - 2 * sqrt(tvar1), thi = newdat$Intrans + 
    2 * sqrt(tvar1))

# Converted coefs (remove exp otherwise)

# plot confidence
g0 <- ggplot(newdat, aes(x = Group, y = exp(Intrans)))

g0 + geom_point() + geom_errorbar(aes(ymin = exp(plo), ymax = exp(phi), width = 0.25)) + 
    labs(title = "CI based on fixed-effects uncertainty ONLY") + theme_classic()
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


Because there are large individual differences too adding the random effects makes it harder to read.

I believe this is related to the design question on how to analyze repeated measures between subjects data. 


```r
# plot prediction
g0 + geom_errorbar(aes(ymin = exp(tlo), ymax = exp(thi), width = 0.25)) + labs(title = "CI based on FE uncertainty + RE variance") + 
    theme_classic()
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


#### Try 11: Correct way to manually estimate CIs from multilevel model (using ETL as baseline comparison group)

Again shows the difference in the standard errors when the baseline group comparison is changed.


```r
newdat <- expand.grid(Group = c("C", "ETL", "MTL"), Intrans = 0)

mm <- model.matrix(terms(b1.2), newdat)

newdat$Intrans <- mm %*% fixef(b1.2)

pvar1 <- diag(mm %*% tcrossprod(vcov(b1.2), mm))

tvar1 <- pvar1 + VarCorr(b1.2)$f.id[1]  ## must be adapted for more complex models

newdat <- data.frame(newdat, plo = newdat$Intrans - 2 * sqrt(pvar1), phi = newdat$Intrans + 
    2 * sqrt(pvar1), tlo = newdat$Intrans - 2 * sqrt(tvar1), thi = newdat$Intrans + 
    2 * sqrt(tvar1))

# Converted coefs (remove exp otherwise)

# plot confidence
g0 <- ggplot(newdat, aes(x = Group, y = exp(Intrans)))

g0 + geom_point() + geom_errorbar(aes(ymin = exp(plo), ymax = exp(phi), width = 0.25)) + 
    labs(title = "CI based on fixed-effects uncertainty ONLY") + theme_classic()
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 


Same issues with including random effect variance.


```r
# plot prediction
g0 + geom_errorbar(aes(ymin = exp(tlo), ymax = exp(thi), width = 0.25)) + labs(title = "CI based on FE uncertainty + RE variance") + 
    theme_classic()
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 
