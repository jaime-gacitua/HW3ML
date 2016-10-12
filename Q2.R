#ISLR Chapter 5 question 6.

names(city)
city
require(ISLR)
require(boot)

summary(Default)
names(Default)
set.seed(1)

## Part a
glm.fit=glm(default~balance+income,data=Default,family=binomial)
summary(glm.fit)

Default[c(1,2,3,4),]

## Part b
boot.fn <- function(def.data, obs.index) {
  data2 = def.data[obs.index,]
  glm.fit = glm(default~balance+income, data=data2, family=binomial)
  coefs = c(glm.fit$coefficients[2], glm.fit$coefficients[3])
  return(coefs)
}
boot.fn(Default, c(1,2,3,4,5,6,7,8,9,10))

## Part c
boot(Default, boot.fn, R = 999)
