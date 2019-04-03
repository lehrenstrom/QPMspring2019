# Question 1
install.packages("faraway")
library("faraway")
data("newhamp")
help("newhamp")
data(newhamp)


ObamaPerHand = newhamp[newhamp$pObama & newhamp$votesys=="H",]
ObamaPerMach = newhamp[newhamp$pObama & newhamp$votesys=="D",]
DeanPerHand = newhamp[newhamp$Dean & newhamp$votesys=="H",]
DeanPerMach = newhamp[newhamp$Dean & newhamp$votesys=="D",]

plot(ObamaPerHand$pObama, DeanPerHand$Dean, pch = 6, xlab = "Percentage of Obama Voters", ylab = "Percentage of Dean Voters",
     main = "Relationship Between Percentage that Voted for Dean and Obama", col="blue")
points(ObamaPerMach$pObama, DeanPerMach$Dean, pch = 1, col="orange")
legend("topleft", legend = c("Hand-Counted Votes", "Machine-Counted Votes"), pch = c(6, 1), col = c("blue", "orange"))


# Question 2
?dnorm
x = seq(-4, 4, length=1000)
norm = dnorm(x)
setwd("C:/Users/lehre/Desktop/QPM")
getwd()
pdf("PS3Question2Graph.pdf")
plot(x, norm, type = "l", main = "Normal and t distributions", ylab = "Density of Probability", xlab = "Standard Deviations from Mean")
t1 = dt(x, 1)
t3 = dt(x, 3)
t20 = dt(x, 20)
lines(x, t1, type = "l", lwd = 1, col="red")
lines(x, t3, type = "l", lwd = 1, col="blue")
lines(x, t20, type = "l", lwd = 1, col="green")
legend("topleft", legend = c("Normal", "20 Df", "3 Df", "1 Df"), text.col = c("black", "green", "blue", "red"))
# https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
# https://stackoverflow.com/questions/10543443/how-to-draw-a-standard-normal-distribution-in-r/10543555
# https://www.r-graph-gallery.com/119-add-a-legend-to-a-plot/ 
dev.off()
# Question 3
install.packages("Zelig")
library("Zelig")
data("voteincome")
?voteincome
data(voteincome)
View(voteincome)
View(Voters)
# Ho: mu = 50
# Ha: mu =/= 50
# level of significance: a = 0.05
Voters = voteincome[voteincome$vote==1,]
S = sd(Voters$age)
xbar = mean(Voters$age)
n = length(Voters$age)
StandardError = S/sqrt(n)
mu = 50
zscoretest = (xbar-mu)/StandardError
p = pnorm(zscoretest)*2
# Because p > 0.05, we fail to reject the null hypothesis and thus there is some evidence to suggest that the
# null hypothesis of the mean voting age being 50 is true.
zscore = 1.96
xbar = mean(Voters$age)
leftCI = xbar - zscore*StandardError
rightCI = xbar + zscore*StandardError
CI = c(leftCI, rightCI)


# Question 4
ybar = 9.5
s = 1.2
n = 16
df = n - 1
mu = 10
# n < 30, use t-statistic

StandardError = s/sqrt(n)
tscoretest = (ybar - mu)/StandardError
p4 = pt(tscoretest, df)
# p exceeds 0.05
ybar = 9.5
pops = 1.2
n = 16
mu = 10 
StandardError = pops/sqrt(n)
zscoretest4 = (ybar-mu)/StandardError
p = pnorm(zscoretest4)





# Question 5

# The population distribution would be a binomial distribution, while the sample distribution would be normally distributed.

# The value of pi-hat would be equal to 0.4885. 
pihat = 341/698

# The standard error of pi-hat would be 0.0189.

SEPiHat = sqrt((pihat*(1-pihat))/698)

# The 95% confidence interval is (0.451, 0.526).

zscore = 1.96
CILeft = pihat - zscore*SEPiHat
CIRight = pihat + zscore*SEPiHat
CI = c(CILeft, CIRight)
# Question 6
# The causal claim being made by the authors is that face-to-face voter mobilization leads to an increase in voter
# voter turnout in local elections.

# Question 7

# Ho: mu1 - mu2 = 0
# Ho: mu1 - mu2 =/= 0
# a = 0.05
# CLT: n1 > 30, n2 > 30
n1 = 1117
n2 = 870
mu1 = 2.99
mu2 = 2.86
samplemudist = mu1 - mu2
stddev1 = 2.34
stddev2 = 2.22
SEMean1 = .070
SEMean2 = .075
stderror = sqrt(((stddev1^2)/n1)+((stddev2^2)/n2))
p = pnorm(samplemudist, 0, sd = stddevdist, lower.tail = FALSE)*2
z = (samplemudist)/stderror
# Question 8 


# Ho: mu1 - mu2 = 0
# Ho: mu1 - mu2 =/= 0
# a = 0.05
# CLT: n1 < 30, n2 < 30; cannot use z-test
n1 = 11
n2 = 16
df = n1 + n2 - 2
mu1 = 2.99
mu2 = 2.86
samplemudist = mu1 - mu2
stddev1 = 2.34
stddev2 = 2.22
SEMean1 = .070
SEMean2 = .075

stddevhat = sqrt((((n1 - 1)*(stddev1^2))+((n2 - 1)*(stddev2^2)))/((n1+n2-2)))
SEttest = stddevhat*sqrt((1/n1)+(1/n2))
t = (samplemudist)/SEttest
p2 = pt(t, df=df, lower.tail=FALSE)*2

