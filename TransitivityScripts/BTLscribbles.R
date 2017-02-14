#What I want to say in the end: choice difficulty is not processed the same way for each group. The MTL group does not speed up its choices as quickly as the control groups as decisions get easier. We first quantified this by taking individual differences into account and creating ordinal value scales for each subject. That is, if a bounty is chosen 16 times out of the 19 potential times it could have been it would have an ordinal value of 16. Decision difficulty was then quantified as the difference between the ordinal values of the two candy bars in a given choice depending on the aggregate preferences of each subject. The larger this difference the easier and therefore presumably faster the decision. This pattern is observed for all participants (negative slopes for all groups) but is significantly less pronounced for the MTL group. Though we cannot rule out alternative explanations of this pattern due to other physical limitations posed by the MTL lesions, due to lack of additional neuropsychological data we can provide additional support to this pattern being due to noisy value representations by fitting Bradly-Terry-Luce models to the binary comparisons. These models can tell us how well the preference data follow choice axioms. Doing so separately for the three groups shows that these models fit worst for the MTL group (AIC comparison). Comparing the MTL group to both of the lesion groups reveals that the MTL group has aan estimated odds-multiplier of 0.512 against its favour in making consistent preferences.

#Fitting the BTL models to transitivity data

###################################
### Using the BradleyTerry2 package
###################################

# Firth, D. (2005). Bradley-Terry models in R. Journal of Statistical Software, 12(1), 1-12.
# Firth, D., & Turner, H. L. (2012). Bradley-Terry models in R: the BradleyTerry2 package. Journal of Statistical Software, 48(9).
library("BradleyTerry2")

#Create df for each group with 3 cols: winner, loser, freq
#Should have 190*2 + 20 rows
tmp <- choice2.trial.data.w.val

#Separate each groups data
tmp.mtl <- tmp[tmp$Group == "MTL", ]
tmp.c <- tmp[tmp$Group == "C", ]
tmp.etl <- tmp[tmp$Group == "ETL", ]

prep.btl.data <- function(raw.data){
  #Tabulate pref freq data
  tmp.tbl <- with(raw.data, table(Trial, Choice.image))
  #Create df to organize the pref freq data
  tmp.df <- data.frame(bar1 = rep(NA, 190), bar2 = rep(NA, 190), win1 = rep(NA, 190), win2 = rep(NA, 190))
  
  #Organize pref freq data for model
  for(r in 1:190){
    col.index.1 <- as.numeric(which(tmp.tbl[r,] != 0)[1])
    col.index.2 <- as.numeric(which(tmp.tbl[r,] != 0)[2])
    
    tmp.df$bar1[r] <- col.index.1
    tmp.df$bar2[r] <- col.index.2
    tmp.df$win1[r] <- tmp.tbl[r, col.index.1]
    tmp.df$win2[r] <- tmp.tbl[r, col.index.2]
  }
  tmp.df$bar1 <- factor(tmp.df$bar1, levels = c(1:20))
  tmp.df$bar2 <- factor(tmp.df$bar2, levels = c(1:20))
  
  return(tmp.df)
}

### Everyone
all.btl.df <- prep.btl.data(tmp)
prefModel.all <- BTm(cbind(win1, win2), bar1, bar2, data = all.btl.df)
utils <- data.frame(bar = c(1:20), util = c(0, coef(prefModel.all)))

choice2.trial.data.w.val <- merge(choice2.trial.data.w.val, utils, by.x="Image_left", by.y="bar", all.x = T)
choice2.trial.data.w.val <- merge(choice2.trial.data.w.val, utils, by.x="Image_right", by.y="bar", all.x = T)
names(choice2.trial.data.w.val)[which(names(choice2.trial.data.w.val) %in% c("util.x", "util.y"))] <- c("util.left", "util.right")

choice2.trial.data.w.val$util.diff <- with(choice2.trial.data.w.val, util.left - util.right)

summary(lmer(IntransFreq ~  (1|f.id) + Group*util.diff, choice2.trial.data.w.val))
summary(lm(IntransFreq ~ Group*util.diff, choice2.trial.data.w.val))

### MTL group
mtl.btl.df <- prep.btl.data(tmp.mtl)
prefModel.mtl <- BTm(cbind(win1, win2), bar1, bar2, data = mtl.btl.df)
prefModel.mtl

### ETL group
etl.btl.df <- prep.btl.data(tmp.etl)
prefModel.etl <- BTm(cbind(win1, win2), bar1, bar2, data = etl.btl.df)
prefModel.etl

# C group
c.btl.df <- prep.btl.data(tmp.c)
prefModel.c <- BTm(cbind(win1, win2), bar1, bar2, data = c.btl.df)
prefModel.c

#Compare models
prefModel.mtl #highest AIC (worst fit)
prefModel.etl #best fit
prefModel.c # second best fit
#summary(prefModel.c)

#Do the groups have the same preferences?
sort(prefModel.mtl$coefficients)
sort(prefModel.etl$coefficients)
sort(prefModel.c$coefficients)
#BTabilities(prefModel.c) #also gives coefficientsbut with se's

#It's hard to compare fits of the same model on different groups data
#How about treating the groups as an "order effect, i.e. similar to home game advantage in baseball games?
#Following example from paper above and ?BTm. Results kind of make sense but not sure what exactly is going on

tmp.lesion <- tmp[tmp$Group == "MTL", ]
tmp.control <- tmp[tmp$Group != "MTL", ]

prep.btl.data2 <- function(raw.data){
  #Tabulate pref freq data
  tmp.tbl <- with(raw.data, table(Trial, Choice.image))
  #Create df to organize the pref freq data
  tmp.df <- data.frame(bar1 = rep(NA, 380), bar2 = rep(NA, 380), win = rep(NA, 380))
  
  #Organize pref freq data for model
  for(r in 1:190){
    col.index.1 <- as.numeric(which(tmp.tbl[r,] != 0)[1])
    col.index.2 <- as.numeric(which(tmp.tbl[r,] != 0)[2])
    
    tmp.df$bar1[r] <- col.index.1
    tmp.df$bar2[r] <- col.index.2
    tmp.df$win[r] <- tmp.tbl[r, col.index.1]
  }
  
  for(r in 1:190){
    col.index.1 <- as.numeric(which(tmp.tbl[r,] != 0)[1])
    col.index.2 <- as.numeric(which(tmp.tbl[r,] != 0)[2])
    
    tmp.df$bar1[r+190] <- col.index.2
    tmp.df$bar2[r+190] <- col.index.1
    tmp.df$win[r+190] <- tmp.tbl[r, col.index.2]
  }
  
  tmp.df$bar1 <- factor(tmp.df$bar1, levels = c(1:20))
  tmp.df$bar2 <- factor(tmp.df$bar2, levels = c(1:20))
  
  return(tmp.df)
}

lesion.btl.df <- prep.btl.data2(tmp.lesion)
control.btl.df <- prep.btl.data2(tmp.control)

btl.df <- merge(lesion.btl.df, control.btl.df, by = c("bar1", "bar2"))
names(btl.df) <- c("bar1", "bar2", "lesion.win", "control.win")

##  Simple Bradley-Terry model, ignoring lesion disadvantage:
prefModel <- BTm(cbind(lesion.win, control.win), bar1, bar2, data = btl.df, id = "bar")

##  Now incorporate the "lesion disadvantage" effect
btl.df$bar1 <- data.frame(bar = btl.df$bar1, lesion = 1)
btl.df$bar2 <- data.frame(bar = btl.df$bar2, lesion = 0)
prefModel2 <- update(prefModel, formula = ~ bar + lesion)
summary(prefModel2)
coefficients(prefModel2)[20]
#the (MTL)lesion group has an estimated odds-multiplier of exp(-0.66925) = 0.512 against its favour.
exp(coefficients(prefModel2)[20])

#########################
### Using the prefmod package
#########################
#Hatzinger, R., & Dittrich, R. (2012). Prefmod: An R package for modeling preferences based on paired comparisons, rankings, or ratings. Journal of Statistical Software, 48(10), 1-31.

library(prefmod)

#data should have 91 rows, 191 columns: 190 for each paired comparison and 1 for group
with(choice2.trial.data.w.val[choice2.trial.data.w.val$SubjectID == "119",], table(Trial, Choice.image))

mod.tr.ltype <- llbtPC.fit(..., nitems = 20, obj.names = factor(c(1:20)), formel = ~ group, elim = ~ group)

#########################
### Using the eba package
#########################
library("eba")

balanced.pcdesign(20)

tmp <- choice2.trial.data.w.val[choice2.trial.data.w.val$f.id == "119",]

View(tmp)

get.ordinal.value(tmp)

with(tmp, table(Paste, Choice.image))

sort(unique(tmp$Paste))

str(tmp$Image_left)

View(choice2.trial.data.w.val[choice2.trial.data.w.val$Trial == 2,])


tmp <- choice2.trial.data.w.val

# Separate choice data in to groups

tmp.mtl <- tmp[tmp$Group == "MTL", ]
tmp.c <- tmp[tmp$Group == "C", ]
tmp.etp <- tmp[tmp$Group == "ETP", ]

tmp.tbl <- with(tmp.mtl, table(Trial, Choice.image))

tmp.m <- matrix(0, nrow = 20, ncol = 20)

for(r in 1:190){
  col.index.1 <- as.numeric(which(tmp.tbl[r,] != 0)[1])
  col.index.2 <- as.numeric(which(tmp.tbl[r,] != 0)[2])
  
  tmp.m[col.index.1 , col.index.2] <- tmp.tbl[r, col.index.1]
  tmp.m[col.index.2 , col.index.1] <- tmp.tbl[r, col.index.2]
}

str(strans(tmp.m)$violdf)

btl.mtl <- eba(tmp.m)
summary(btl.mtl)
