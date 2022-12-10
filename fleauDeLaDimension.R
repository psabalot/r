### Illustration fleau de la dimension
set.seed(1234)
MAT <- matrix(runif(2000000),ncol=1000,nrow=2000)
head(MAT)
dim(MAT)
tmp <- MAT[,1]
tmp <- sqrt(tmp^2)
length(tmp)
m1 = min(tmp)
m2 = max(tmp)
m = mean(tmp)
################
tmp <- MAT[,1:2]
tmp <- tmp^2
tmp <- sqrt(rowSums(tmp))
min(tmp)
max(tmp)

### on va généraliser
resMin = 1:1000 + NA
resMax = 1:1000 + NA
resMoy = 1:1000 + NA
resMin[1] = m1
resMax[1] = m2
resMoy[1] = m
for(i in 2:1000)
{
  disTemp = sqrt(rowSums(MAT[,1:i]^2))
  resMin[i] = min(disTemp)
  resMax[i] = max(disTemp)
  resMoy[i] = mean(disTemp)
}

# et afficher
library(ggplot2)
ggplot(data = data.frame(id=1:1000, min=resMin, max=resMax, moy=resMoy)) +
  aes(x=id) +
  geom_line(aes(y=moy), color="black", ) +
  geom_line(aes(y=max), color="red") + 
  geom_line(aes(y=min), color="blue") +
  xlab("Nombre de dimensions") + 
  ylab("Distance à l'origine")
