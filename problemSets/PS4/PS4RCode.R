# Question 1 a

?table

SuperTroopers = matrix(c(14, 6, 7, 7, 7, 1), ncol = 3, byrow=TRUE)
colnames(SuperTroopers) = c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(SuperTroopers) = c("Upper class", "Lower class")
SuperTroopers
SuperTroopersExpected = matrix(c((27/42)*21, (27/42)*13, (27/42)*8, (15/42)*21, (15/42)*13, (15/42)*8), ncol = 3, byrow = TRUE)
SuperTroopersExpected
ResidualST = SuperTroopers - SuperTroopersExpected
ChiSqST = sum((ResidualST^2)/SuperTroopersExpected)
# x2 = 3.791

# Question 1 b
chisq.test(SuperTroopers)

# p = .1502
# At this level of significance, we fail to reject the null hypothesis that officers are more or less likely to 
# solicit bribes or pull drivers over based on their class.

StandardizedResiduals = (SuperTroopers - SuperTroopersExpected)/sqrt(SuperTroopersExpected)
colnames(StandardizedResiduals) = c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(StandardizedResiduals) = c("Upper class", "Lower class")
StandardizedResiduals

# Sources on making tables, chi-square
# https://www.youtube.com/watch?v=53kYOOr5Yhk
# https://www.cyclismo.org/tutorial/R/types.html#tables
# https://www.statisticshowto.datasciencecentral.com/what-is-a-standardized-residuals/
# Question 2 a
r2 = .094
k = 2
n = 30
df = n - 2
stderror = .016
coeff = .042
t = .042/.016
p = dt(t, df)
# p-value .016
# Question 2 b
n = 76
df = n - 2
stderror = .013
coeff = .042
t = .042/.013
p = dt(t, df)
# p-value: .0028
# Source: 
# https://stattrek.com/regression/slope-test.aspx
# https://www.researchgate.net/publication/288056551_The_Effects_of_Lawn_Signs_on_Vote_Outcomes_Results_from_Four_Randomized_Field_Experiments
# Question 3
WomenLeaders = read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
reserved = WomenLeaders$reserved
water = WomenLeaders$water
RegressionWomenLeaders = lm(water ~ reserved)
summary(RegressionWomenLeaders)
plot(WomenLeaders$reserved, WomenLeaders$water)
abline(RegressionWomenLeaders, col="red")
# p = 0.0197, coefficient: 9.252

# Question 4 

install.packages(car)
library(car)
data(Prestige)
help(Prestige)
View(Prestige)

Prestige$professional = NA # make empty column
Prestige$professional = Prestige$type # make the column 
Prestige$professional = as.character(Prestige$professional) # First part of recoding, essentially telling R you must
# read the code like characters rather than a factor (just makes R not freak out)
Prestige$professional[ Prestige$professional=="prof" ] <- "1" # recoding
Prestige$professional[ Prestige$professional=="bc" ] <- "0" # recoding
Prestige$professional[ Prestige$professional=="wc" ] <- "0" # recoding

PrestigeLM = lm(prestige ~ income + professional + professional:income, data=Prestige)
summary(PrestigeLM)


answer.e1 = 21.142 + .0032*(0) + 37.78*(1) - 0.0023*(0)
answer.e2 = 21.142 + .0032*(1000) + 37.78 - 0.0023*(1000)
answer.e = answer.e2 - answer.e1
answer.e # 0.9

answer.f1 = 21.142 + .0032*(6000) + 37.78*(0) - 0.0023*(6000)*0
answer.f2 = 21.142 + .0032*(6000) + 37.78*(1) - 0.0023*(6000)*1
answer.f = answer.f2 - answer.f1
answer.f # 23.98
# Sources: https://stackoverflow.com/questions/25729700/invalid-factor-level-na-generated-warning?lq=1
# https://stackoverflow.com/questions/26147558/what-does-the-error-arguments-imply-differing-number-of-rows-x-y-mean
# http://rstudio-pubs-static.s3.amazonaws.com/2551_979c0abc1bde4a90a95313f8f54fc516.html
# https://stattrek.com/multiple-regression/interaction.aspx

# Question 5 

library("faraway")
data("newhamp")
colnames(newhamp)
View(NewHamp)
NewHamp = newhamp
NewHamp = as.data.frame(NewHamp)
NewHamp$votesys = as.character(NewHamp$votesys)
NewHamp$votetsys[NewHamp$votesys=="D"] = 1
NewHamp$votetsys[NewHamp$votesys=="H"] = 0
votesysonly = lm(pObama ~ votesys, data=NewHamp)

summary(votesysonly)
VSPR = lm(pObama ~ votesys + povrate, data=NewHamp)
summary(VSPR)
VSPRPCI = lm(pObama ~ votesys + povrate + pci, data=NewHamp)
summary(VSPRPCI)
VSPRPCID = lm(pObama ~ votesys + povrate + pci + Dean, data=NewHamp)
summary(VSPRPCID)
VSPRCIDW = lm(pObama ~ votesys + povrate + pci + Dean + white, data=NewHamp)
summary(VSPRCIDW)
Deanonly = lm(pObama ~ Dean, data=NewHamp)
summary(Deanonly)

# Question 6 a
setwd("C:/Users/lehre/Desktop/QPM/PS4")
incumbents_subset = read.csv("incumbents_subset.csv")
View(incumbents_subset)
VSdifflog.lm = lm(voteshare ~ difflog, data=incumbents_subset)
summary(VSdifflog.lm)
VSdifflog.rs = summary(VSdifflog.lm)$residuals
VSdifflog.fv = VSdifflog.lm$fitted.values
pdf("Question6a.pdf")
plot(incumbents_subset$difflog, incumbents_subset$voteshare, xlab = "difflog", ylab = "voteshare",main = "Relationship between difflog and voteshare")
abline(VSdifflog.lm, col = "red")
dev.off()

# Question 6 b
PVdifflog.lm = lm(presvote ~ difflog, data=incumbents_subset)
summary(PVdifflog.lm)
PVdifflog.rs = summary(PVdifflog.lm)$residuals
pdf("Question6b.pdf")
plot(incumbents_subset$difflog, incumbents_subset$presvote, xlab = "difflog", ylab = "presvote",main = "Relationship between difflog and presvote")
abline(PVdifflog.lm, col = "red")
dev.off()


# Question 6 c
VSpresvote.lm = lm(voteshare ~ presvote, data=incumbents_subset)
summary(VSpresvote.lm)
pdf("Question6c.pdf")
plot(incumbents_subset$presvote, incumbents_subset$voteshare, xlab = "presvote", ylab = "voteshare",main = "Relationship between presvote and voteshare")
abline(VSpresvote.lm, col = "red")
dev.off()

# Question 6 d
Residual.lm= lm(VSdifflog.rs ~ PVdifflog.rs, data=incumbents_subset)
summary(Residual.lm)
pdf("Question6d.pdf")
plot(PVdifflog.rs, VSdifflog.rs, xlab = "Residuals B", ylab = "Residuals A",main = "Relationship between Residuals from Questions A and B")
abline(Residual.lm, col = "red")
dev.off()

# Question 6 e
VSdifflogpresvote.lm = lm(voteshare ~ difflog + presvote, data=incumbents_subset)
summary(VSdifflogpresvote.lm)
