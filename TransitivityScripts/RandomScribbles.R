#how many intransitivities was a bar involved in?

#each bar was in 19 trials and 171 triplets
sum(1:18)

#function to count how many intransitive triplets a given bar was involved in for each subject
#data take triplet data for one subject (if fed data for more subjects counts for all)
#sum of sumIntrans col for one subject gives triple counted Intrans data
countBarIntrans <- function(data){
  
  bar.df <- data.frame(bar = c(1:20), sumIntrans = rep(0, 20))
  
  for (i in 1:nrow(bar.df)){
    for (j in 1:nrow(data)){
      if ((data$A[j] == bar.df$bar[i] | data$B[j] == bar.df$bar[i] | data$C[j] == bar.df$bar[i])&is.na(data$Intrans[j]) == F) {
        bar.df$sumIntrans[i] = bar.df$sumIntrans[i]+data$Intrans[j]
      }
    }
  }
  return(bar.df)
}

#Cut out choice data to apply the function on
choice.triplet.data <- both.triplet.data[both.triplet.data$Task == "choice",]

#Apply function to all subjects (loopy function might take a bit) 
bar.df <- ddply(choice.triplet.data, c("f.id"), countBarIntrans)

#Get means for each bar
mean.bar.df <- data.frame(bar = c(1:20), meanIntrans = aggregate(sumIntrans ~ bar, bar.df, mean)$sumIntrans, semIntrans = aggregate(sumIntrans ~ bar, bar.df, se)$sumIntrans)

#Plot means for each bar
ggplot(mean.bar.df, aes(x = bar, y = meanIntrans))+
  geom_bar(stat = "identity", fill = "grey")+
  geom_errorbar(aes(ymin = meanIntrans - semIntrans, ymax = meanIntrans+semIntrans))+
  theme_bw()+
  xlab("Candy Bar")+
  ylab("Average Number of Intransitive Triplets")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(1, 9, 3))

ggsave("./TransitivityFigures/CleanFigure4.png", width=6, height=4, dpi=300)

#Are the bars significantly different from each other? No.
summary(aov(meanIntrans ~ bar, data = mean.bar.df))
  
#from aggregate preferences estimate util of bar 1 and util of bar 2 
#then Intrans ~ util of bar 1 + util of bar 2 + util diff * Group
#based on elke's comment: is util diff sig and the interaction btw util diff and group sig here
#even if the interaction is sig this assumes everyone had the same preferences
#so to look at how each group reacted to difficult choices on a more fine grained level we ranked 
#if there is more noise in the MTL groups value representations then the variance of the util differences would be smaller

#when we normalize choice difficulty (ie difference in rank ordered utilities) we find that the MTL group is not only overall more slower (difference in intercept) which was noted before but also slower in responding to easier trials (difference in slope)
summary(lmer(val.diff ~ Group + (1|f.id), data = choice2.trial.data.w.val))

#PLOT RT over normalized val diff for choice vs numbers side by side to show that the intercept and slope difference holds for the choice task but not the control task

#The control group appears faster compared to both lesion groups in the control task BUT this doesn't appear statistically significant in the regression. 
p1 <- ggplot(numbers.trial.data, aes(scale(num.diff), RT, group = Group, linetype = Group))+
  geom_smooth(method = "lm", color = "black")+
  theme_bw()+
  xlab("Ease")+
  ylab("Response times")+
  expand_limits(y=c(500, 1800), x = c(-1, 3))+
  ggtitle("Numbers task")


p2 <- ggplot(choice2.trial.data.w.val, aes(c.val.diff, RT, group = Group, linetype = Group))+
  geom_smooth(method = "lm", color = "black")+
  theme_bw()+
  xlab("Ease")+
  ylab("Response times")+
  expand_limits(y=c(500, 1800))+
  ggtitle("Choice task")

library(gridExtra)

g <- ggplotGrob(p1)$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=1, widths=c(11,1))

ggsave("./TransitivityFigures/CleanFigure5.png", width=8, height=3, dpi=300)

#No significant interactions or group differences. All groups are faster the larger the difference between the numbers (i.e. the easier the decisions)
numbers.trial.data <- ddply(numbers.trial.data, "f.id", mutate, c.num.diff = scale(num.diff))

numbers.trial.data$Group <- as.factor(numbers.trial.data$Group)

contrasts(numbers.trial.data$Group) <- cbind(c(-1,1, 0), c(-1, -1, 2))

summary(lmer(scale(RT) ~ c.num.diff * Group + (1|f.id), data = numbers.trial.data))

choice2.trial.data.w.val$Group <- as.factor(choice2.trial.data.w.val$Group)

contrasts(choice2.trial.data.w.val$Group) <- cbind(c(-1,1, 0), c(-1, -1, 2))

summary(lmer(scale(RT) ~ c.val.diff * Group + (1|f.id), data = choice2.trial.data.w.val))

### post-hoc power analysis on difference between intransitivies in the two tasks
choice.Intransitive <- both.Intransitive[both.Intransitive$Task == "choice",]
numbers.Intransitive <- both.Intransitive[both.Intransitive$Task == "numbers",]
tmp.Intransitive <- merge(choice.Intransitive[,c("f.id", "CleanPercentIntr", "Group")], numbers.Intransitive[,c("f.id", "CleanPercentIntr")], by = "f.id")
tmp.Intransitive$DiffPercentIntr <- tmp.Intransitive$CleanPercentIntr.x - tmp.Intransitive$CleanPercentIntr.y
summary(aov(DiffPercentIntr~Group, tmp.Intransitive))
summary(lm(DiffPercentIntr~Group, tmp.Intransitive))
#post hoc power on the anova according to gpower with partial eta sqared (i.e. r sqaured for the regression) = 0.1603 is 0.96

summary(aov(log(CleanPercentIntr+1)~(Task*Group)+Error(f.id/(Task))+(Group),data=both.Intransitive.contrast))

library(BayesFactor)
tmp.Intransitive$DiffPercentIntr.log <- log(tmp.Intransitive$DiffPercentIntr+10)
anovaBF(DiffPercentIntr~Group, tmp.Intransitive)
tmp.Intransitive$Group.con <- ifelse(tmp.Intransitive$Group == "MTL", "MTL", "control")
tmp.Intransitive$Group.con <- as.factor(tmp.Intransitive$Group.con)
anovaBF(DiffPercentIntr~Group.con, tmp.Intransitive)
