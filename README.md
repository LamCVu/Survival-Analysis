# Survival-Analysis
Code Appendix

dat <- read.delim("C:/Users/Lam/Desktop/sta104 project dat.txt")
#1
hist(dat$x)
qqnorm(dat$x)
qqline(dat$x)
n = length(dat$x) 
binom_data = as.numeric(dat$x > 0) 
x = sum(binom_data) 
binom.test(x, n, p = .5, alternative = "greater") 
#2a
y1=dat$y1
y2=dat$y2
hist(y1)
hist(y2)
qqnorm(y1)
qqline(y1)
qqnorm(y2)
qqline(y2)
library(exactRankTests)
wilcox.exact(y1, y2, alternative = "less")
t.test(y1,y2,alternative="less")
#2b
ansari.test(y1,y2,alternative = "two.sided")
var.test(y1,y2,alternative="two.sided")
#2c
library(ggplot2)
ks.test(y1,y2,alternative="less")
cdf1=ecdf(y1)
cdf2=ecdf(y2)
plot(cdf1,verticals=TRUE,do.points=FALSE,col="blue")
lines(cdf2,verticals=TRUE,do.points=FALSE, col="green") 
minMax= seq(min(y1,y2), max(y1,y2), length.out=length(y1)) 
x0=minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y10 <- cdf2(x0) 
points(c(x0, x0), c(y0, y10), pch=16, col="red") 
segments(x0, y0, x0, y10, col="red", lty="dotted") 
#3
hist(z1)
hist(z2)
qqnorm(z1)
qqline(z1)
qqnorm(z2)
qqline(z2)
z1=dat$z1
z2=dat$z2
ks.test(z1,z2,alternative="two.sided",exact=0.95)
hist(a)
hist(b)
qqnorm(a)
qqline(a)
qqnorm(b)
qqline(b)
sqrt(25+25/(25*25))*1.36
#4
hist(u1)
qqnorm(u1)
qqline(u1)
hist(u2)
qqnorm(u2)
qqline(u2)
hist(u3)
qqnorm(u3)
qqline(u3)
u1=dat$u1
u2=dat$u2
u3=dat$u3
kruskal.test(list(u1,u2,u3))  
data = c(u1,u2,u3)
type = as.factor(c(rep("u1", 25), rep("u2",25), rep("u3",25)))
anova1 = aov(data~type)
summary(anova1)
