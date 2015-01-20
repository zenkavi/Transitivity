
### Organization code

# Scroll below to descriptive if not interested.

# Read Data in:
  
######### Read Data in
# setwd("~/Dropbox/CDS/Transitivity/grid files")
Data <- read.csv("complete_data.csv")
TrialNumber <- read.csv("TrialNumbers.csv")
options(digits=3)
# names(Data)

install.packages(c("lme4", "ggplot2", "reshape"))
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
Intransitive <- as.data.frame(matrix(NA, nrow=length(unique(Data$f.id)), ncol = 5))
#Columns are: subject, number of missed trials, number of intransitive trials, number of triplets with error, number of triplets with intransitivity and error
names(Intransitive) <- c("Subject", "Missed", "NumIntr", "Err1140", "IntrErr")
#Assign subject numbers to subject column
Intransitive$Subject <- unique(Data$f.id)
#Tabulate the number of trials where trial timed out
#Creates df with subject id in col 1 and number of timed out trials in col 2
Error <- as.data.frame(table(Data[which(Data$RT<0), "SubjectID"]))
#Assign number of missed trials from the Error df to the correct subject in Intrans df
for (i in 1:nrow(Error)){
  j <- which(as.character(Intransitive$Subject) == as.character(Error$Var1[i]))
  Intransitive$Missed[j] <- Error$Freq[i]
}
#Or give 0 if there are no missed trials
Intransitive$Missed <- ifelse(is.na(Intransitive$Missed) == T, 0, Intransitive$Missed)

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

for (x in 1:length(unique(Data$f.id))){
  #Cut data for each participant
  Data.cut <- Data[which(Data$f.id == unique(Data$f.id)[x]),]
  for (i in 1:nrow(comb)){
    #Find trial row in Data.cut that has the A vs B choice in comb
    j <- which((Data.cut$Image_left == comb$A[i] | Data.cut$Image_right == comb$A[i]) & (Data.cut$Image_left == comb$B[i] | Data.cut$Image_right == comb$B[i]))
    #Record choice for A vs B
    comb$ApreftoB[i] <- ifelse(Data.cut$Choice.image[j] == comb$A[i], 1, 0)
    #Record if the trial was error (i.e. timed out)
    comb$ErrApreftoB[i] <- ifelse(Data.cut$RT[j] <0 , 1, 0)
    #Find trial row in Data.cut that has the B vs C choice in comb
    k <- which((Data.cut$Image_left == comb$B[i] | Data.cut$Image_right == comb$B[i]) & (Data.cut$Image_left == comb$C[i] | Data.cut$Image_right == comb$C[i]))
    #Record choice for B vs C
    comb$BpreftoC[i] <- ifelse(Data.cut$Choice.image[k] == comb$B[i], 1, 0)
    #Record if the trial was error (i.e. timed out)
    comb$ErrBpreftoC[i] <- ifelse(Data.cut$RT[k] <0 , 1, 0)
    #Find trial row in Data.cut that has the A vs C choice in comb
    l <- which((Data.cut$Image_left == comb$A[i] | Data.cut$Image_right == comb$A[i]) & (Data.cut$Image_left == comb$C[i] | Data.cut$Image_right == comb$C[i]))
    #Record choice for A vs C
    comb$CpreftoA[i] <- ifelse(Data.cut$Choice.image[l] == comb$C[i], 1, 0)
    #Record if the trial was error (i.e. timed out)
    comb$ErrCpreftoA[i] <- ifelse(Data.cut$RT[l] <0 , 1, 0)
    #A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A (the second is the reverse)
    comb$Intrans[i] <- ifelse (comb$ApreftoB[i] == 1 & comb$BpreftoC[i] == 1 & comb$CpreftoA[i] == 1, 1, 
                               ifelse(comb$ApreftoB[i] == 0 & comb$BpreftoC[i] == 0 & comb$CpreftoA[i] == 0, 1, 0))
    #A triplet involves and Error if any of the three trials involved has timed out
    comb$Err[i] <- ifelse(comb$ErrApreftoB[i] == 1 | comb$ErrBpreftoC[i] == 1 | comb$ErrCpreftoA[i] == 1, 1, 0)
    #Mark triplets where there is both an error and an intransitivity
    comb$ErrIntrans[i] <- ifelse((comb$ErrApreftoB[i] == 1 | comb$ErrBpreftoC[i] == 1 | comb$ErrCpreftoA[i] == 1) & comb$Intrans[i]==1, 1, 0)
  }
  #Sum number of intransitivities, errors and triplets with both on subject level
  Intransitive[x,3] <- sum(comb$Intrans)
  Intransitive[x,4] <- sum(comb$Err)
  Intransitive[x,5] <- sum(comb$ErrIntrans)
}

Intransitive$PercentMissed <- Intransitive$Missed/190*100
Intransitive$PercentIntr <- Intransitive$NumIntr/1140*100
Intransitive$PercentMissedTrialsAffect <- Intransitive$Err1140/1140*100
#Percent of intransitivies that include an error
Intransitive$PIntrError<- Intransitive$IntrErr/ Intransitive$NumIntr *100

Intransitive$Group <- rep(NA, nrow(Intransitive))
Group <- aggregate(Data[,c("Group")],by=list(id=Data$f.id),FUN="unique")
for (i in 1:nrow(Group)){
  j <- which(as.character(Intransitive$Subject) == as.character(Group$id[i]))
  Intransitive$Group[j] <- as.character(Group$x[i])
}
rm(Group)


# Descriptives
  
  ### Number of participants
  
#   We are missing two ETP patients. They might be crucial for the difference between MTL-ETL group.

# table(Intransitive$Group) 

### Number of intransitive choices

# MTL differs from C but not from ETP (inconsistent result) and ETP does not differ from C

######### Descriptives

# summary(Intransitive$NumIntr)
# aggregate(NumIntr ~ Group, data=Intransitive, mean)
# aggregate(NumIntr ~ Group, data=Intransitive, median)
# aggregate(PercentIntr ~ Group, data=Intransitive, mean)
# aggregate(PercentIntr ~ Group, data=Intransitive, median)
# Aov.pint <- aov(PercentIntr ~ Group, data = Intransitive)
# summary(Aov.pint) # The group means are different
# TukeyHSD(Aov.pint, conf.level = 0.95)
# pairwise.t.test(Intransitive$PercentIntr, Intransitive$Group, p.adjust = "bonferroni") 

### Kruskal-Wallis-Test

# kruskal.test(NumIntr ~ Group, data=Intransitive)
# kruskal.test(NumIntr ~ Group=="MTL", data=Intransitive)

# Question: The graph in the paper includes a CI.
# How is this CI calculated? What model is it based on?

# ggplot()+
#   geom_boxplot(data=Intransitive, aes(x=Group, y=PercentIntr))+
#   theme_classic()

### Percent left

# No group differences. Near 50% overall.

# Percent left = No group differences
# mean(Data$Choice.left1.right0) #0.49
# aggregate(Choice.left1.right0 ~ Group, data=Data, mean)
#Just to make sure: bring to subject level
# Left <- aggregate(Data[,c("Choice.left1.right0")],by=list(id=Data$f.id),FUN="mean")
# Left$Group <- Intransitive$Group
# ggplot(data=Left, aes(x=Group, y=x, group = Group))+
#   geom_boxplot()+
#   theme_classic()
# summary(aov(x ~ Group, data = Left))
# TukeyHSD(aov(x ~ Group, data = Left), conf.level = 0.95)
# pairwise.t.test(Left$x, Left$Group, p.adjust = "bonferroni") 
# rm(Left)

### RT

# No group differences.

# RT = No group differences
# mean(Data$RT)
# aggregate(RT ~ Group, data=Data, mean)
# RT <- aggregate(Data[,c("RT")],by=list(id=Data$f.id),FUN="mean")
# Group <- aggregate(Data[,c("Group")],by=list(id=Data$f.id),FUN="unique")
# RT$Group <- Group$x
# plot(x ~ Group, data = RT)
# summary(aov(x ~ Group, data = RT))
# TukeyHSD(aov(x ~ Group, data = RT), conf.level = 0.95)
# pairwise.t.test(RT$x, RT$Group, p.adjust = "bonferroni") 
# rm(RT, Group)

### Any of the bars chosen more often?

# Doesn't seem so.

# Any of the bars chosen more? Doesn't seem so.
# Bar <- as.data.frame(prop.table(table(Data$Choice.image)))
# names(Bar) <- c("Bar", "Prop")
# plot(Prop ~ Bar, data=Bar, ylim = c(0,1))
# rm(Bar)

### Group differences in RT controlling for individual differences?

# Not significant.

#Group differences in RT controlling for individual differences: not significant
# m1 <- lmer(RT ~ Group + (1 | f.id), data=Data)
# summary(m1)

### Trial number has  a significant fixed effect on RT 

# People get faster in later trials

#Trial number has  a significant fixed effect on RT: People get faster in later trials
# m2 <- lmer(RT ~ Trialnumber + Group + (1 | f.id), data= Data)
# summary(m2)
# par(mfrow=c(1,2))
#Intercept = a
# log.a <- coef(summary(lm(log(RT) ~ log(Trialnumber), data= Data[Data$RT>0,])))[1]
# a <- exp(log.a)
#Slope = k
# k <-coef(summary(lm(log(RT) ~ log(Trialnumber), data= Data[Data$RT>0,])))[2]
# plot(aggregate(log(RT) ~ log(Trialnumber), data= Data[Data$RT>0,], mean), col = "red",pch=16, cex=0.5)
# abline(log.a,k)
# plot(Data$Trialnumber, a*(Data$Trialnumber)^k, pch=16, cex=0.5, xlab = "Trialnumber", ylab="RT")
# points(aggregate(RT ~ Trialnumber, data= Data[Data$RT>0,], mean), col = "red", pch=16, cex=0.5)

### Does RT predict number of intransitive choices? 

# No (as expected).

#Does RT predict number of intransitive choices? No (as expected).
# RT.sub <- aggregate(RT ~ f.id, data=Data, mean)
# Intransitive <- Intransitive[order(Intransitive$Subject),]
# Intransitive$Mean.RT <- RT.sub$RT
# rm(RT.sub)
# m3 <- glm(NumIntr ~ Group + Mean.RT, data=Intransitive)
# summary(m3)

### Correlation of RT and number of intransitive choices by group

#Correlation of RT and number of intransitive choices by group
# cor(Intransitive$Mean.RT[Intransitive$Group == "C"],
#     Intransitive$NumIntr[Intransitive$Group == "C"])
# cor(Intransitive$Mean.RT[Intransitive$Group == "ETP"],
#     Intransitive$NumIntr[Intransitive$Group == "ETP"])
# cor(Intransitive$Mean.RT[Intransitive$Group == "MTL"],
#     Intransitive$NumIntr[Intransitive$Group == "MTL"])


# par(mfrow=c(1,1))
# ggplot(data=Intransitive, aes(Mean.RT, NumIntr))+
#   geom_point()+
#   geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
#   facet_grid(.~Group)+
#   theme_classic()

# NOTE: Problem with data
# --------------------------------------
#   
#   There are trials where RT<0.
# 
# ```{r}
# sum(Data$RT<0)
# ```
# 
# ***Excluding these trials completely throws off the intransitivity calculation.***
#   
#   Participants from all groups missed at least one trial like this. 
# But the MTL is group is responsible for 68% of it.
# In fact, one participant (8146) is responsible for 32%. (This participant does not have RT>0 for 66/190 - 34% of trials)
# 
# ```{r eval=FALSE, echo=FALSE}
# ### Subject numbers (there are 190 trials for everyone)
# sort(unique(Data$Subject))
# length(unique(Data$Subject))
# ```
# 
# ### 56 participants with trials where RT<0
# 
# ```{r}
# table(Data[which(Data$RT<0), "SubjectID"])
# # table(Data$Group, Data$SubjectID)
# # prop.table(table(Data[which(Data$RT<0), "SubjectID"]))
# prop.table(table(Data[which(Data$RT<0), "Group"]))
# ```
# 
# ### How many of these trials are involved in intransitives?
# 
# #### Which group made missed most trials?
# 
# The MTL group.
# 
# ```{r}
# aggregate(Missed ~ Group, data=Intransitive, sum)
# ```
# 
# Most people from all groups missed 0 or 1 trials (out of 190).
# Nobody in the C group missed more than 3 trials.
# Nobody in the ETP group missed more than 8 trials.
# There is one participant in the MTL group who has missed 66 trials.
# Without this participant MTL group still has most missed trials but not by this much.
# 
# (For table below: rows are number of missed trials, cols are number of people from each group)
# 
# ```{r}
# table(Intransitive$Missed, Intransitive$Group)
# ```
# 
# PercentMissed: Trials out of 190 that have RT<0
# 
# PercentIntr: Triplets with intransitivity out of 1140 possible triplets
# 
# PercentMissedTrialsAffect: Triplets where 1 or more pair has RT<0
# 
# PIntrError: Intransitive triplets that have 1 or more pairs with RT<0
# 
# ```{r}
# aggregate(PercentIntr ~ Group, data=Intransitive, mean)
# aggregate(PercentMissedTrialsAffect ~ Group, data=Intransitive, mean)
# aggregate(PIntrError ~ Group, data=Intransitive, mean)
# ```
# 
# So for example:
#   One participant missed 2 trials.(2/190*100 = 1.05%)
# Without accounting for these and assuming 1140 "clean" triplets we counted 25 intransitive choices (2.2%).
# These two trials affected 36 triplets out of 1140 (36/1140*100 = 3.16%). Note, this number could have been less.
# 4 of the triplets that we had counted as intransitive included at least one missed trial (4/25*100 = 16%).
# So for these trials we can't claim intransitivity. 
# We also can't claim no intransitivity for the remaining 32 triplets.
# If we exclude all 36 affected triplets we would have 1104 clean triplets and 21 of these would be clean intransitives so the percentage of intransitive choices would be 21/1104*100 = 1.9 %.
# 

Intransitive$CleanTriplets <- 1140 - Intransitive$Err1140
Intransitive$CleanIntr <- Intransitive$NumIntr - Intransitive$IntrErr
Intransitive$CleanPercentIntr <- Intransitive$CleanIntr/Intransitive$CleanTriplets *100

# Looking at the descriptives again based on this re-calculation:
  
#   ***All values are slightly smaller but the significant difference between C and MTL holds***
  
#   ETP is not significantly different than either of the groups but this might be due to the two missing participants (i.e. power).

# ```{r}
# summary(Intransitive$CleanIntr)
# aggregate(CleanIntr ~ Group, data=Intransitive, mean)
# aggregate(CleanIntr ~ Group, data=Intransitive, median)
# aggregate(CleanPercentIntr ~ Group, data=Intransitive, mean)
# aggregate(CleanPercentIntr ~ Group, data=Intransitive, median)
# Aov.cleanint <- aov(CleanPercentIntr ~ Group, data = Intransitive)
# summary(Aov.cleanint) # The group means are different
# TukeyHSD(Aov.cleanint, conf.level = 0.95)
# pairwise.t.test(Intransitive$CleanPercentIntr, Intransitive$Group, p.adjust = "bonferroni") 
# ```
# 
# ```{r}
# kruskal.test(CleanIntr ~ as.factor(Group), data=Intransitive)
# kruskal.test(CleanIntr ~ Group=="MTL", data=Intransitive)
# kruskal.test(CleanIntr ~ Group=="C", data=Intransitive)
# kruskal.test(CleanIntr ~ Group=="ETP", data=Intransitive)
# ```
# 
# 
# Question: The graph in the paper includes a CI.
# How is this CI calculated? What model is it based on?
# 
# ```{r}
# ggplot()+
#   geom_boxplot(data=Intransitive, aes(x=Group, y=CleanPercentIntr))+
#   theme_classic()
# ```
# 
# Same as above.
# 
# ```{r}
# #Does RT predict number of intransitive choices? No (as expected).
# Data.clean <- Data[Data$RT>=0,]
# RT.sub.clean <- aggregate(RT ~ f.id, data=Data.clean, mean)
# Intransitive <- Intransitive[order(Intransitive$Subject),]
# Intransitive$Mean.RT.c <- RT.sub.clean$RT
# rm(RT.sub.clean)
# m3.c <- glm(CleanIntr ~ Group + Mean.RT, data=Intransitive)
# summary(m3.c)
# ```
# 
# ```{r}
# #Correlation of RT and number of intransitive choices by group
# cor(Intransitive$Mean.RT.c[Intransitive$Group == "C"],
#     Intransitive$CleanIntr[Intransitive$Group == "C"])
# cor(Intransitive$Mean.RT.c[Intransitive$Group == "ETP"],
#     Intransitive$CleanIntr[Intransitive$Group == "ETP"]) #This decreases most.
# cor(Intransitive$Mean.RT.c[Intransitive$Group == "MTL"],
#     Intransitive$CleanIntr[Intransitive$Group == "MTL"])
# ```
# 
# ```{r warning=FALSE, echo=FALSE, message=FALSE}
# par(mfrow=c(1,1))
# ggplot(data=Intransitive, aes(Mean.RT.c, CleanIntr))+
#   geom_point()+
#   geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
#   facet_grid(.~Group)+
#   theme_classic()
# ```


# Notes and TO DO's:

# 1. Timing out still could be an error that should be counted some other way.

# 2. Ways to add power to the current dataset:
# - Running more subjects
# - Error with the two missing participants: Data files were corrupt. Nothing recovered.

# 3. Brand information: How should this be included in an HLM? It is a source of variance.

# 4. What does Kruskell Wallace tests show?

# 5. What was used in Camille et al. (2011):
# Participants made made choices in 11 choice sets between bundles consisting of 3 to 7 options.
# Each bundle was implied a restriction in price and income. (so each bundle falls on a single line defined by the number of bars and juice boxes. Making choices that have unique slopes - not matching the slope of any other choice constitutes an intransitivity).
# Transitivy was calculated using methods from economics (possible due to the restrictions in the design of the choice sets) that I do not think can be applied to this design.
# Overall VMF lesion patients made more violations to transitivity in number and severity compared to normal controls.

# 6. Order effects

# Create larger matrix (89*1140 rows - all triplets for all subject)
# Store max(j,k,l) i.e. the last trial that makes a triplet intransitive 

# ```{r eval=FALSE}
#Count intransitive choices on trial level

#Create df where intransitive choices will be counted by creating the 1140 triplets possible out of 20 options (C(20,3) = 1140) for 89 subjects - 101460 columns

comb2 <- as.data.frame(matrix(NA, nrow=length(unique(Data$f.id))*nrow(t(combn(unique(Data$Image_left),3))), ncol = 21))

names(comb2) <- c("Subject", "Group", "A", "B", "C", "ApreftoB", "BpreftoC", "CpreftoA", "Intrans", "ErrApreftoB", "ErrBpreftoC", "ErrCpreftoA", "Err", "ErrIntrans", "TrialA", "TrialB", "TrialC", "ThirdTrial", "PasteA", "PasteB", "PasteC")

#Assign subject numbers
for (i in 0:(length(unique(Data$f.id)) - 1)){
comb2[(i*1140+1):(i*1140+1140), 1] <- sort(unique(Data$Subject))[i+1]
}
comb2$Subject <- as.factor(comb2$Subject)

#Assign group numbers
for (i in 0:(length(unique(Data$f.id)) - 1)){
comb2[(i*1140+1):(i*1140+1140), 2] <- aggregate(Data[,c("Group")],by=list(id=Data$f.id),FUN="unique")[(i+1),2] 
}
comb2$Group <- ifelse(comb2$Group == 1, "C", ifelse(comb2$Group == 2, "ETP", "MTL"))
comb2$Group <- as.factor(comb2$Group)

#Create triplets
comb2[1:1140, 3:5] <- t(combn(unique(Data$Image_left),3))

#Copy paste the combination of triplets for all subjects
for (i in 1:(length(unique(Data$f.id)) - 1)){
comb2[(i*1140+1):(i*1140+1140), 3:5] <- comb2[1:1140, 3:5] 
}


for (i in 1:nrow(comb2)){
#Find trial row in Data that has the A vs B choice in comb2
j <- which((Data$Image_left == comb2$A[i] | Data$Image_right == comb2$A[i]) & (Data$Image_left == comb2$B[i] | Data$Image_right == comb2$B[i]) & Data$Subject == comb2$Subject[i])
#Record choice for A vs B
comb2$ApreftoB[i] <- ifelse(Data$Choice.image[j] == comb2$A[i], 1, 0)
#Record if the trial was error (i.e. timed out)
comb2$ErrApreftoB[i] <- ifelse(Data$RT[j] <0 , 1, 0)
#Find trial row in Data that has the B vs C choice in comb2
k <- which((Data$Image_left == comb2$B[i] | Data$Image_right == comb2$B[i]) & (Data$Image_left == comb2$C[i] | Data$Image_right == comb2$C[i]) & Data$Subject == comb2$Subject[i])
#Record choice for B vs C
comb2$BpreftoC[i] <- ifelse(Data$Choice.image[k] == comb2$B[i], 1, 0)
#Record if the trial was error (i.e. timed out)
comb2$ErrBpreftoC[i] <- ifelse(Data$RT[k] <0 , 1, 0)
#Find trial row in Data that has the A vs C choice in comb2
l <- which((Data$Image_left == comb2$A[i] | Data$Image_right == comb2$A[i]) & (Data$Image_left == comb2$C[i] | Data$Image_right == comb2$C[i]) & Data$Subject == comb2$Subject[i])
#Record choice for A vs C
comb2$CpreftoA[i] <- ifelse(Data$Choice.image[l] == comb2$C[i], 1, 0)
#Record if the trial was error (i.e. timed out)
comb2$ErrCpreftoA[i] <- ifelse(Data$RT[l] <0 , 1, 0)
#A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A (the second is the reverse)
comb2$Intrans[i] <- ifelse (comb2$ApreftoB[i] == 1 & comb2$BpreftoC[i] == 1 & comb2$CpreftoA[i] == 1, 1, 
ifelse(comb2$ApreftoB[i] == 0 & comb2$BpreftoC[i] == 0 & comb2$CpreftoA[i] == 0, 1, 0))
#A triplet involves and Error if any of the three trials involved has timed out
comb2$Err[i] <- ifelse(comb2$ErrApreftoB[i] == 1 | comb2$ErrBpreftoC[i] == 1 | comb2$ErrCpreftoA[i] == 1, 1, 0)
#Mark triplets where there is both an error and an intransitivity
comb2$ErrIntrans[i] <- ifelse((comb2$ErrApreftoB[i] == 1 | comb2$ErrBpreftoC[i] == 1 | comb2$ErrCpreftoA[i] == 1) & comb2$Intrans[i]==1, 1, 0)
#Store trial numbers and last trial
comb2$TrialA[i] <- Data$Trialnumber[j]
comb2$TrialB[i] <- Data$Trialnumber[k]
comb2$TrialC[i] <- Data$Trialnumber[l]
comb2$ThirdTrial[i] <- max(comb2$TrialA[i], comb2$TrialB[i], comb2$TrialC[i])
comb2$PasteA[i] <- Data$Paste[j]
comb2$PasteB[i] <- Data$Paste[k]
comb2$PasteC[i] <- Data$Paste[l]
}

# ```

# - Analysis of intransitivities from subject level to trial level (how many times was trial X involved in 
# intransitivity?)
# - Test linear and quadratic order variables of trial in 190
# - Frequency of intransitivities over time
# - Give 1 to all the three trials that are involved in an intransitivity

# Note: Each pair can be involved in max 18 intransitivities

# ```{r}

# test <- comb2[1:4560,]

# test.data <- Data[Data$Subject %in% c(119, 306, 3256, 3574),]

# test.m <- melt(test[, c("Subject", "Group", "Intrans", "Err", "ErrIntrans","PasteA", "PasteB", "PasteC")], id = c("Subject", "Group", "Intrans", "Err", "ErrIntrans"))

#Temp df before aggregating. Has all three "Paste"s (ie trials in one column)
comb2.m <- melt(comb2[, c("Subject", "Group", "Intrans", "Err", "ErrIntrans","PasteA", "PasteB", "PasteC")], id = c("Subject", "Group", "Intrans", "Err", "ErrIntrans"))

#Aggregate df to sum (I think Err and ErrIntrans are less usefull here. Using only Intrans so far.)
#Note: Total intransitivities will be triple counted this way (because all three pairs that are involved in an intransitivity take the dummy variable 1)

# test.ag <- aggregate(test.m[,c("Intrans","Err", "ErrIntrans")], by = list(Subject = test.m$Subject, Group = test.m$Group, Paste = test.m$value), FUN = "sum")

comb2.ag <- aggregate(comb2.m[,c("Intrans","Err", "ErrIntrans")], by = list(Subject = comb2.m$Subject, Group = comb2.m$Group, Paste = comb2.m$value), FUN = "sum")

#Merge Intransitivity info with rest of trial level data
# test.data <- merge(test.data, test.ag, by.x = c("SubjectID", "Group", "Paste"), by.y = c("Subject", "Group", "Paste"))

comb2.data <- merge(Data, comb2.ag, by.x = c("SubjectID", "Group", "Paste"), by.y = c("Subject", "Group", "Paste"))

#Create centered trial number and quadratic trial term
# test.data$c.Trialnumber <- test.data$Trialnumber - mean(test.data$Trialnumber)
# test.data$c.TrialQuad <- test.data$c.Trialnumber^2

comb2.data$c.Trialnumber <- comb2.data$Trialnumber - mean(comb2.data$Trialnumber)
comb2.data$c.TrialQuad <- comb2.data$c.Trialnumber^2

# For one person model
# summary(lm(Intrans ~ Trialnumber + TrialQuad, data=test.data))
# summary(lm(Intrans ~ c.Trialnumber + c.TrialQuad, data=test.data))

# Plot intransitivities by trial number (convex trend)
# ggplot(data=comb2.data, aes(x=Trialnumber, y=Intrans))+
# geom_smooth()+
# theme_classic()

# For the quadratic term the sign is positive when the model is convex and negative when the curve is concave.

# Multilevel model on trial level checking for effect of subject, group and trial
# Significant trial effects. Earlier trials are involved in more intransitivities (a recency effect instead of a primacy)
# Also note significant group effects. MTL differs significantly more from the control group but ETL does not 
# summary(lmer(Intrans ~ (Group|f.id) + c.Trialnumber + c.TrialQuad + Group, data=comb2.data))

# ```


# 7. Testing Luce choice model
# - A model with subject + 190 pairs + brand
# - If there aren't any intransitivities this should fit perfectly
# 
# Note: I don't think I'm doing the right thing here..
# 
# ```{r}
# summary(lmer(Intrans ~ (Group|f.id) + c.Trialnumber + c.TrialQuad + Group + as.factor(Choice.image), data=test.data))
# ```

save.image("./TransitivityWorkSpace013014Grid.RData")
