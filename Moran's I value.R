# https://r-spatial.github.io/spdep/reference/moran.plot.html

# Load necessary packages

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


 ##############################
data(afcon, package="spData")
colnames(afcon)
mp <- moran.plot(afcon$totcon, nb2listw(paper.nb),
                 labels=as.character(afcon$name), pch=19)

moran.plot(as.vector(scale(afcon$totcon)), nb2listw(paper.nb),
           labels=as.character(afcon$name), xlim=c(-2, 4), ylim=c(-2,4), pch=19)

if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") + 
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    xlab(xname) + ylab(paste0("Spatially lagged ", xname))
}

#################Python code 

import numpy as np
from esda.moran import Moran
from libpysal.weights import KNN
import matplotlib.pyplot as plt

# Example data (replace this with your data)
data = np.random.rand(50, 1)  # Reshape to 2 dimensions

# Create spatial weights matrix (k-nearest neighbors)
k = 5  # Adjust this value as needed
w = KNN.from_array(data, k=k)

# Standardize the data
data_std = (data - np.mean(data)) / np.std(data)

# Calculate Moran's I
moran_i = Moran(data_std, w)

# Print Moran's I statistic
print("Moran's I:", moran_i.I)
print("p-value:", moran_i.p_sim)

# Plot Moran's I statistic
plt.figure(figsize=(8, 6))
plt.plot(moran_i.I, 'bo', markersize=8, label="Moran's I")
plt.axhline(0, color='k', linestyle='--', linewidth=0.5)
plt.axhline(moran_i.EI_sim, color='r', linestyle='--', linewidth=0.5, label="Expected Moran's I")
plt.title("Moran's I Statistic")
plt.xlabel("Permutation")
plt.ylabel("Moran's I")
plt.legend()
plt.show()

# Plot p-values
plt.figure(figsize=(8, 6))
plt.plot(moran_i.p_sim, 'go', markersize=8, label="p-value")
plt.axhline(0.05, color='r', linestyle='--', linewidth=0.5, label="Significance Level")
plt.title("Moran's I p-value")
plt.xlabel("Permutation")
plt.ylabel("p-value")
plt.legend()
plt.show()





