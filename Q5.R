require(MASS)
require(boot)

## checking there are no NA values
Boston$medv[is.na(Boston$medv)]

## Part a
medv.mean <- mean(Boston$medv)

## Part b
medv.se <- sd(Boston$medv) / sqrt(length(Boston$medv))

## Part c
# Define the function to call with boot
se.function = function(data,index){     # INput args are: (i) the data and (ii) an index vector defining what observations to use
  data2 = data[index]
  return(mean(data2))
}
se.function(Boston$medv, sample(length(Boston$medv), 500)) # test for one realization
se.boot <- boot(Boston$medv, se.function, R=100000)
se.boot

## Part d
se.boot$t0[1] + 2*sd(se.boot$t)
se.boot$t0[1] - 2*sd(se.boot$t)

t.test(Boston$medv)

## Part e
summary(Boston$medv)[3]

## Part f
# Define the function to call with boot
med.function = function(data,index){     # INput args are: (i) the data and (ii) an index vector defining what observations to use
  data2 = data[index]
  return(summary(data2)[3])
}
med.boot <- boot(Boston$medv, med.function, R=100000)
med.boot

## Part g
?quantile
med.percentile10 <- quantile(Boston$medv, 0.1)
med.percentile10

## Part h
# Define the function to call with boot
perc10.function = function(data,index){     # INput args are: (i) the data and (ii) an index vector defining what observations to use
  data2 = data[index]
  return(quantile(data2, 0.1))
}
med.boot.perc10 <- boot(Boston$medv, perc10.function, R=100000)
med.boot.perc10

