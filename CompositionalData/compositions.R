library(aqp)
library(compositions)
library(sharpshootR)
library(soilDB)
library(soiltexture)

options(warn = -1)

# A horizons in Flanly
flanly.hz <- horizons(fetchKSSL(series='Flanly'))#[which(grepl(horizons(foo)$hzn_desgn,pattern='A')),]
flanly.hz.sub <- flanly.hz[,c("sand","silt","clay")]
flanly.aa <- acomp(flanly.hz.sub)
mean.acomp(flanly.aa)
## SOURCE: Analyzing Compositional Data with R, by K. Gerald van den Boogaart
#         Linear Models for Compositions (Ch 5.2 pp105-111)
#linear model of cec7 on the isometric log-ratio transformed (Aitchison Simplex) compositions (sand+silt+clay = 100%)
model <- lm(log(flanly.hz$cec7)~ilr(flanly.aa)) 
summary(model)
a <- coef(model)[1] # intercept
b <- ilrInv(coef(model)[-1], orig=flanly.aa) #back transform coefficients for the two ilr terms

plot(flanly.aa) #plot the compositions
straight(mean(flanly.aa), b, lwd=2, col="black", lty=2) #plot mean value in composition space
y <- pretty(log(flanly.hz$cec7))
refX <- mean(flanly.aa) + ((y - a)/norm(b)^2)*b
plot(refX, add=T, pch=19)

varb <- ilrvar2clr(vcov(model)[-1,-1]) #estimation variance of b
names(b) <- colnames(flanly.aa)
scaleB <- 1
plot(scaleB*b)
plot(0*b,add=T,pch=20)
alpha=0.1
rF=sqrt(qf(1-alpha, nrow(varb)-1, model$df.residual))
ellipses(scaleB*b, scaleB^2*varb, rF) 
#WOW! the clay component, and somewhat less so silt, are significant determinants of CEC7! 
#confidence ellipsoid does not overlap the neutral element of the simplex

# Bt1 horizons in loafercreek
data(loafercreek)
hz <- horizons(loafercreek)
hz.sub <- hz[which(hz$hzname == 'Bt1'),]
hz.sub.fineearth <- hz.sub[,which(names(hz.sub) %in% c("sand","silt","clay"))]
hz.sub.fineearth <- na.omit(hz.sub.fineearth[,c("sand","silt","clay")])
hz.sub
