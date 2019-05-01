# Question 1

R2 = 0.24
n = 100
k = 2
df1 = k # number of predictors
df2 = n-(k+1) 

FStatistic = (R2/(1-R2))*(df2/df1)
p = (1 - pf(FStatistic, df1, df2))

# Question 2 
R2 = 0.35
n = 100
k = 2
df1 = k # number of predictors
df2 = n-(k+1) 

FStatistic = (R2/(1-R2))*(df2/df1)
p = (1 - pf(FStatistic, df1, df2))

# Question 3

# Question 4
n = 11
SampStdDev = 9.50
mean = 93.87
t = qt(.95, n-1)
MoE = SampStdDev/sqrt(n)
CILeft = mean - t*MoE
CIRight = mean + t*MoE
CI = c(CILeft, CIRight)
CI
# 88.67, 99.06

# Question 5
n = 781
TableACAObserved = matrix(c(221, 225, 446, 360-221, 335-(360-221), 335, 360, 225+(335-(360-221)), 781), byrow = T, ncol = 3)
colnames(TableACAObserved) = c("Yes", "No", "Total")
rownames(TableACAObserved) = c("Democrats", "Republicans", "Total")
TableACAObserved

RepublicanNoObs = 196
RepublicanNoExp = 180.58
RepublicanNoX = ((RepublicanNoObs- RepublicanNoExp)^2)/RepublicanNoExp

TableACANoTotal = matrix(c(221, 225, 360-221, 335-(360-221)), byrow = T, ncol = 2)
colnames(TableACANoTotal) = c("Yes", "No")
rownames(TableACANoTotal) = c("Democrats", "Republicans")
TableACANoTotal
chisq.test(TableACANoTotal)

# Question 6
n1 = 288
n2 = 242
stddev1 = 2.4
stddev2 = 2.2
samplemudist = 3.8 - 3.5
stderror = sqrt(((stddev1^2)/n1)+((stddev2^2)/n2))
# both n1 and n2 exceed 30, can test normality
zstatistic = 1.96
CILeft = samplemudist - stderror*zstatistic
CIRight = samplemudist + stderror*zstatistic
CI = c(CILeft, CIRight)
CI
# -0.092, 0.692
p = pnorm(samplemudist, 0, sd = stderror, lower.tail = FALSE)*2  
