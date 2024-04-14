# "Analysis of yield curves through principal component analysis (PCA)."
# 
# "PCA analyses allows understanding the typical shapes of yield curves,
# observed in the market."
# 
# "A yield curve is a line that plots yields, or interest rates,
# of bonds that have equal credit quality but differing maturity dates."


#A) Load the data in the file YieldCurvedata.RData. The data frame YCdf
#embedded in this file contains the daily Canadian yield curves applying to
#Canadian zero-coupon bonds for the last few years.

load("/Users/edinsonjimenezarita/Projects/STAT380 Statistical Learning/STAT380 Assignment3/YieldCurvedata.RData")
dim(YCdf)
#So basically we have 6630 observations (bond's yields at different dates), and at every single date, the
#yield of the bond is given for a different length of periods. There are 120 different periods (the extra column is the date)
#Basically, it's following the daily (price) yield of bonds at different maturities.

# "Note: A zero-coupon bond is a bond in which the face value is repaid at the time of maturity.
#Use the R function data.matrix to construct a matrix called YCmatrix which contains the whole data curve history
YCmatrix <- data.matrix(YCdf[ , !(names(YCdf) == "Date")])
class(YCdf)
class(YCmatrix)
YCmatrix[1:5,1:5]

YCmatrixdm <- scale(YCmatrix)
#We can observe that the mean of sum of a column in zero! 
round(mean(YCmatrixdm[, 1]))

#Now I will create vector of column sample averages for each predictor of your original data
ALLmeans <- colMeans(YCmatrix)
length(ALLmeans)
ALLmeans[1:10]

plot(ALLmeans, type = "b", xlab = "Maturity in periods", ylab = "Average Yield", main = "Yield Curve")
#Every value in ALLmeans is simply the average yield of all days for a specific maturity date.


# B) Perform a principal component decomposition (i.e. a PCA) of your dataset in YCmatrixdm


#Now we will start the decomposition using the function prcomp()
pca_result <- prcomp(YCmatrixdm)
summary(pca_result)
#Note we have 3 important things in pca_result:
#1) The rotation vectors pca_result$rotation, which are the eigenvectors of covariance
#matrix to make the matrix that represents the orthonormal basis
#2) The transformed data pca_result$x (PCA components), the already transformed data 
#using the rotation vectors (Eigenvectors of covariance matrix)
#3) The standard deviation after the PCA decomposition. Which is the (squared root of) diagonal 
#values of the covariance matrix of transformed dataset pca_result$x



#The first PC accounts for the most variation of the original data (The maturuty length, 
#accross different dates) Let's plot the % of variation of every 6 first PC component
pca.var <- pca_result$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,2)
pca.var.per
barplot(pca.var.per[1:6], main='Scree Plot', xlab="Principal Component", ylab="Percent Variation")
#Now for the cumulative
barplot(cumsum(pca.var.per[1:6]), main='Scree Plot', xlab="Principal Component", ylab="Percent Variation")
#We can observe that almost 98% if all the variance is happening in PC1!

# Determine the number of principal components to retain
num_pcs <- which(cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) > 0.998)[1]
print(paste("Retaining", num_pcs, "principal components."))

# Show the main first three principal component scores (first 10 rows)
pca_result$x[, 1:num_pcs][1:10,1:3]
# Visualize the first two principal components
plot(pca_result$x[, 1], pca_result$x[, 2], xlab = "PC1", ylab = "PC2")

# Part (c):
# Plot the first three loading vectors a1, a2, a3 (the eigenvectors of the
# covariance matrix) in a single figure. Provide an interpretation for each
# of the three first principal components; i.e., when the scores associated with
# each of these eigenvectors vary, how does the shape of the yield curve vary?

# First I will prove the eigenvectors of covariance matrix, are the same as the rotation vectors

# Using the cov() function
C <- cov(YCmatrixdm)
C[1:10,1:10]
# We can observe that variance have a standard deviation of 1. So they,re scaled. This
# is important when doing PCA decomposition to have a better relative comparison

library(ggplot2)
eig <- eig(C)
class(eig$vectors)
dim(eig$vectors)
dim(pca_result$rotation)
# Extract the first three loading vectors from prcomp() function!
a1 <- pca_result$rotation[, 1]
a2 <- pca_result$rotation[, 2]
a3 <- pca_result$rotation[, 3]


# Compare the vectors

#Is not completely exact, this might be because prcomp() probably calculated PCA differently
#It probably used singular value decomposition, instead of the eigenvectors. 
#You can still note that the rotation vectors, are the really close to the eigenvectors of the 
#Covariance matrix of our centered dataset!
eig$vectors[1:5,1]
a1[1:5]
eig$vectors[1:5,2]
a2[1:5]
t(eig$vectors)[1:5,3]
a3[1:5]
#I will continue using the rotation vectors from the function, as they might be more accurate


#We can observe that the cumulative variance on both cases, the original matrix and
#the PCA transformed matrix, have the same cumulative variances of factors
cumsum(round(diag(cov(pca_result$x))))
cumsum(round(diag(cov(YCmatrixdm))))
#But in PCA transformed matrix, variable 1 and 2 (PC1 & PC2), take almost all of variance explained
#For this reason, the first three column vectors of PCA transformed matrix, is more
#than enough to take all of variance explained.


#Just checking that pca_result$x = eivectors*YCmatrixdm
(t(t(eig$vectors) %*% t(YCmatrixdm)))[4,4]
(pca_result$x)[4,4]
#eig$vectors
#YCmatrixdm

round((t(eig$vectors) %*% t(YCmatrixdm) - t(pca_result$x))[1:10,1:21])
#I found that they're not exactly the same in all of variables and observation, 
#since this should give the zero matrix. I will assume there are differences in
#calculating using the function prcomp(). I will continue using the results I found 
#using the mentioned function




#PROBLEM: It is impossible to plot the loading vectors in a single figure, since they
#are multidimensional. The maximum plotting we can do in real life is 3D. Our vectors 
#have at the 120th dimension. I will simply plot the first two values of the 3 vectors
#Note: This will NOT give orthogonal vectors on the graph. 

#This is the plotting of transformed points of dataset YCmatrixdm
plot(YCmatrixdm[,1],YCmatrixdm[,2],main='Data points and basis',
     xlab = expression('X'[1]), 
     ylab = expression('X'[2]),
     xlim=c(-2,4),
     ylim=c(-2,4)
)
#Now I will plot the first three vectors
arrows(x0=0, y0=0, x1 = a1[1], y1 = a1[2], col = 'red',lwd=2)
arrows(x0=0, y0=0, x1 = a2[1], y1 = a2[2], col = 'red',lwd=2)
arrows(x0=0, y0=0, x1 = a3[1], y1 = a3[2], col = 'red',lwd=2)


#Since it is impossible to plot the vectors, I will simply do a POINTplot of the eigenvectors
#given different their different weigths. Pointplot the first three loading vectors:
par(mfrow = c(1, 3))

# Plot the first loading vector
plot(a1, type = "b", xlab = "Variable", ylab = "PC1 Loading", main = "PC1 Loadings")

# Plot the second loading vector
plot(a2, type = "b", xlab = "Variable", ylab = "PC2 Loading", main = "PC2 Loadings")

# Plot the third loading vector
plot(a3, type = "b", xlab = "Variable", ylab = "PC3 Loading", main = "PC3 Loadings")

# Reset the par() settings
par(mfrow = c(1, 1))

#For simplicity
A <- pca_result$rotation
Z <- pca_result$x
X <- YCmatrixdm

# Provide an interpretation for each of the three first principal component
#Our first three principal components: 



# i.e., when the scores associated with each
# of these eigenvectors vary, how does the shape of the yield curve vary?

#So basically we analyze changes in matrix Z, how do they affect approximation of X? 
#Remember, t(Z) = t(A) %*% t(X)
#         A %*% t(Z) = t(X)

dim(A[,1:3])
dim(Z[,1:3])

# Reconstruct the yield curve from PC1, PC2, and PC3
reconstructed_yields <- A[, 1:3] %*% t(Z[,1:3])
reconstructed_yields <- t(reconstructed_yields)

ALLmeans <- colMeans(YCmatrix)
ALLstdv <- sqrt(diag(cov(YCmatrix)))

#Checking if descaling works on YCmatrixdm. 
f <- sweep(t(YCmatrixdm), 1, ALLstdv, "*")
f <- sweep(f, 1, ALLmeans, "+")
f <- t(f)
f[1:5,1:5]
YCmatrix[1:5,1:5]
#It does work!

# Plot the AVERAGE reconstructed yield curves
par(mfrow = c(1, 2))
plot(colMeans(reconstructed_yields), type = "l", xlab = "Maturity", ylab = "Yield (average)", main = "Yield Curve")
plot(colMeans(X), type = "l", xlab = "Maturity", ylab = "Yield (average)", main = "Yield Curve")
#This doesn't show too much value, since both are centered at the mean, so the mean is really close to zero. 
#But even now, we can see that variation is happening everywhere equally on YCmatrixdm, compared to reconstructed matrix

#Now I will try descaling to compare. 
descaled_reconstructed_yields <- sweep(t(reconstructed_yields), 1, ALLstdv, "*")
descaled_reconstructed_yields <- sweep(descaled_reconstructed_yields, 1, ALLmeans, "+")
descaled_reconstructed_yields <- t(descaled_reconstructed_yields)
dim(descaled_reconstructed_yields)

plot(colMeans(f), type = "l", xlab = "Maturity", ylab = "Yield", main = "Yield Curve")
plot(colMeans(descaled_reconstructed_yields), type = "l", xlab = "Maturity", ylab = "Yield", main = "Yield Curve")
# we can observe that on average, it is pretty much the same! 
#We can still see that individual observationa. are close between constructed and original dataset: 
reconstructed_yields[1:5,1:10]
X[1:5,1:10]
#And on the descaled version too
descaled_reconstructed_yields[1:5,1:10]
YCmatrix[1:5,1:10]

#When the scores associated with each of these eigenvectors vary, how does the shape of the yield curve vary? 
#So to answer that, I will perform changes on the PCA components, and check on the 
#average yield curve, even when we change PCA components we won't see shifts in the yield curve
#I will prove here: 



Z0 <- Z
#I will start by changing the two most important PCA components, to see if it does
#something to the average of the yield curve
Z0[,2] <- -1*Z[,2]
Z0[,1] <- 2000*Z[,1]
Z0[1:5,1:5]
reconstructed_yields1 <- A[, 1:3] %*% t(Z0[,1:3])
reconstructed_yields1 <- t(reconstructed_yields1)
dim(reconstructed_yields1)
#descaling again
reconstructed_yields1 <- sweep(t(reconstructed_yields1), 1, ALLstdv, "*")
reconstructed_yields1 <- sweep(reconstructed_yields1, 1, ALLmeans, "+")
reconstructed_yields1 <- t(reconstructed_yields1)
dim(reconstructed_yields1)

plot(colMeans(reconstructed_yields1), type = "l", xlab = "Maturity", ylab = "Yield", main = "Yield Curve")
plot(colMeans(YCmatrix), type = "l", xlab = "Maturity", ylab = "Yield", main = "Yield Curve")
#So it doesn't matter if you scale the PCA score vectors. On average it will maintain the same Yield curve approximation. 

#Now, to properly see the effects of changes in the PCA components, I will illustrate it with 
#only the first observation. We would have a better idea of how it works 
#I will have the original PCA approximation (blue), compared to the tweaked approximation (red)
par(mfrow = c(1, 3))

#I multiply PC1 by negative one
Z0 <- Z
Z0[,1] <- -1*Z[,1]
Z0[,2] <- 1*Z[,2]
Z0[,3] <- 1*Z[,3]
Z0[1:5,1:5]
z1 <- as.matrix(Z[1,1:3])
x1 <- A[,1:3] %*% z1
z2 <- as.matrix(Z0[1,1:3])
x2 <- A[,1:3] %*% z2
plot(x2, type = "l", col = "blue", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 1991-01-02", xlim = c(-1, 121), ylim = c(-5, 5))
lines(x1, col = "red")

#I multiply PC2 by negative one
Z0 <- Z
Z0[,1] <- 1*Z[,1]
Z0[,2] <- -1*Z[,2]
Z0[,3] <- 1*Z[,3]
z2 <- as.matrix(Z0[1,1:3])
x2 <- A[,1:3] %*% z2
plot(x2, type = "l", col = "blue", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 1991-01-02", xlim = c(-1, 121), ylim = c(-5, 5))
lines(x1, col = "red")

#I multiply PC3 by negative one
Z0 <- Z
Z0[,1] <- 1*Z[,1]
Z0[,2] <- 1*Z[,2]
Z0[,3] <- -1*Z[,3]
z2 <- as.matrix(Z0[1,1:3])
x2 <- A[,1:3] %*% z2
plot(x2, type = "l", col = "blue", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 1991-01-02", xlim = c(-1, 121), ylim = c(-5, 5))
lines(x1, col = "red")

#CONCLUSION: 
#It seems PC1 does a parallel shift, it maintained its form but if just shifted updwards
#PC2 seems it rotated along the x-axis 
#PC3 seems it rotated along the y-axis

# Part (d):
# First, compute an approximation of the yield curve using the first three
# principal components on each of the following four dates: 
# 1991-01-02, 2000-05-31, 2003-03-26, 2006-06-27.

#Note these dates correspond to observation #: 
# 1, 2301, 3000, 3801
# Remember that: A %*% t(Z) = t(X)

par(mfrow = c(2, 2))

# 1991-01-02
z1 <- as.matrix(Z[1,1:3])
x1 <- A[,1:3] %*% z1
plot(X[1,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 1991-01-02")
lines(x1, col = "blue")

# 2000-05-31
z_2301 <- as.matrix(Z[2301,1:3])
x_2301 <- A[,1:3] %*% z_2301
plot(X[2301,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2000-05-31")
lines(x_2301, col = "blue")

# 2003-03-26
z_3000 <- as.matrix(Z[3000,1:3])
x_3000 <- A[,1:3] %*% z_3000
plot(X[3000,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2003-03-26")
lines(x_3000, col = "blue")

# 2006-06-27
z_3801 <- as.matrix(Z[3801,1:3])
x_3801 <- A[,1:3] %*% z_3801
plot(X[3801,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2006-06-27")
lines(x_3801, col = "blue")

par(mfrow = c(1, 1))


#I will do all the same steps, but with the 5 PCA components!

par(mfrow = c(2, 2))

# 1991-01-02
z1 <- as.matrix(Z[1,1:5])
x1 <- A[,1:5] %*% z1
plot(X[1,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 1991-01-02")
lines(x1, col = "blue")

# 2000-05-31
z_2301 <- as.matrix(Z[2301,1:5])
x_2301 <- A[,1:5] %*% z_2301
plot(X[2301,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2000-05-31")
lines(x_2301, col = "blue")

# 2003-03-26
z_3000 <- as.matrix(Z[3000,1:5])
x_3000 <- A[,1:5] %*% z_3000
plot(X[3000,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2003-03-26")
lines(x_3000, col = "blue")

# 2006-06-27
z_3801 <- as.matrix(Z[3801,1:5])
x_3801 <- A[,1:5] %*% z_3801
plot(X[3801,], type = "l", col = "black", xlab = "Maturity in periods", ylab = "Yield ", main = "Yield Curve 2006-06-27")
lines(x_3801, col = "blue")

par(mfrow = c(1, 1))

#CONCLUSION, it does seem the 5 first PCA components, make a more accurate representation of the real Yield curve! 

