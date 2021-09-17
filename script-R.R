# Load the necessary library
rm(list = ls())
library(datarium)

x = marketing$youtube
y = marketing$sales

a = min(x)
b = max(x)
z = seq(a, b, by = 0.1)

par(mfrow = c(2,2))
par(oma = c(2,1,2,1))
par(mar = c(3,4,1,1))

# Plot the estimation by polynomial regression method with p = 0 and p = 1 (Gaussian kernel)
plot(x,y, main = "",col = grey(.7), xlab = "", ylab = "")
mtext ("Sales", side = 2, line = 3, cex = 1)
library(KernSmooth)
h = 2
colestim = c("red", "blue")
lines(locpoly(x,y, degree = 0, kernel = "normal ", bandwidth = h), col = colestim[1], lwd = 1.4)
lines(locpoly(x,y, degree = 1, kernel = "normal ", bandwidth = h), col = colestim[2], lwd = 1.4)
legend(15, 29, legend = c("p = 0", "p = 1"), col = colestim , lwd = 1.4, lty = 1, cex = .85)

# Plot the estimation by k-nearest neighbor method
plot(x, y, main = "", col = grey(.7), xlab = "", ylab = "")
mhatkNN = rep(0, length = length(z))
k = 12
for (r in (1:length(z)))
{
  indexneighbourshat = which(rank(abs(z[r]-x)) <= k) # Maximum number of its linearly independent column vectors
  mhatkNN[r] = mean(y[indexneighbourshat])
}
points(z, mhatkNN , typ = "l", lwd = 1.4, main = "", col = colestim[1], xlab = "", ylab = "")

# Plot the estimation by spline method
plot(x, y, main = "", col = grey(.7), xlab = "", ylab = "")
mtext("Youtube", side = 1, line = 3, cex = 1)
mtext("Sales", side = 2, line = 3, cex = 1)
lambda = 1
mhatspline = smooth.spline(x, y, spar = lambda )
lines(mhatspline, lwd = 1.4, col = colestim[1], xlab = "")

# Plot the estimation by projection method with trigonometric base
plot(x, y, main = "", col = grey(.7), xlab = "", ylab = "")
mtext("Youtube", side = 1, line = 3, cex = 1)
k = 5
xgrid = z
Xcovariate = c()
for(j in (1:k)){
  Xcovariate = cbind(Xcovariate, sqrt(2/(b-a))*cos(2*pi*j*(x-a)/(b-a)))
  Xcovariate = cbind(Xcovariate, sqrt(2/(b-a))*sin(2*pi*j*(x-a)/(b-a)))
}

tmp = data.frame(y, Xcovariate)
Xgridcovariate = c()
Xgridcovariate = cbind(Xgridcovariate, rep(1, length(xgrid)))
for(j in (1:k)){
  Xgridcovariate = cbind(Xgridcovariate, sqrt(2/(b-a))*cos(2*pi*j*(xgrid-a)/(b-a)))
  Xgridcovariate = cbind(Xgridcovariate, sqrt(2/(b-a))*sin(2*pi*j*(xgrid-a)/(b-a)))
}

ygridhat = Xgridcovariate %*% (lm(tmp)$coefficients)
points(z, ygridhat, typ = "l", lwd = 1.4, main = "", col = colestim[1], xlab = "", ylab = "")