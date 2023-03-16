# install.packages("spdep")
rm(list=ls())
library(sp)
library(spdep)

# Load the data
data(meuse)
# coordinates(meuse) <- c("x", "y")
colnames(meuse)
# Create a spatial weights matrix
knn <- knn2nb(knearneigh(meuse[,c("x", "y")], k=5))
w <- nb2listw(knn)

# Calculate Moran's I statistic
moran <- moran.test(meuse$copper, w)
moran <- moran.test(meuse$lead, w)
# Print the Moran's I statistic and p-value
cat("Moran's I:", round(moran$statistic, 2), "\n")
cat("p-value:", round(moran$p.value, 2), "\n")
