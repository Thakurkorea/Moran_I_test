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







