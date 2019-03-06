###############
# Random data #
###############
set.seed(2)
n = 1500
x <- runif(n, -2, 2)
y <- x + rnorm(n) + 2


################
# Linear model #
################
res <- lm( y ~ x )
print(res)
#Slope of 1 intercept of 3

# plot
plot(x,y, col="steelblue3", main='LM Result')
abline(res, col='black')


###############################
# Squared error cost function #
###############################
cost <- function(X, y, theta) {
  ly <- length(y)
  sum( (X %*% theta - y)^2 ) / (2*ly)
}


###############################
# learning rate and iteration #
###############################
alpha <- 0.01
num_iters <- n

#history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))


####################
# Gradient descent #
####################
ly <- length(y)

for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / ly
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

# plot with Gradient descent
plot(x,y, col="steelblue3", main='Linear regression / Squared error / Gradient descent')
for (i in c(1,5,10,15,20,seq(15,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col="snow3")
}
abline(coef=theta, col="black")

#Cost plot
plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')


#Ref.
#https://www.r-bloggers.com/gradient-descent-in-r/
#https://www.r-bloggers.com/linear-regression-by-gradient-descent/
#https://rpubs.com/VladimirKazan/139320
#https://rpubs.com/fhlgood/graddescentlr
#https://www.khanacademy.org/math/statistics-probability/describing-relationships-quantitative-data/more-on-regression/v/squared-error-of-regression-line
#https://towardsdatascience.com/machine-learning-fundamentals-via-linear-regression-41a5d11f5220
#https://medium.com/@lachlanmiller_52885/machine-learning-week-1-cost-function-gradient-descent-and-univariate-linear-regression-8f5fe69815fd
#https://medium.com/@ken90242/machine-learning%E5%AD%B8%E7%BF%92%E6%97%A5%E8%A8%98-coursera%E7%AF%87-linear-regression-with-one-variable-model-representation-cost-1c5ff295ac2c






