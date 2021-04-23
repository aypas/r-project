# model 1 - BMI Alcohol
# model 2 - GDP Population
# model 3 - Income Schooling 
# model 4 - Schooling Alcohol

library("RColorBrewer")
library("GISTools")

colors <- brewer.pal(4, "Set1")
colorsT <- add.alpha(colors, .4)

data <- read.csv("data.csv")
data <- na.omit(data)
data$GDP <- log10(data$GDP)


#ProletarianBoi96( or ProletarianBoi96) or something...

par(mfcol=c(2,2))

# values can be grabbed with ["1"]/["2]/["3]...must unlist to subscript a vector
customSample <- function(len) {
	#returns three vectors: one vector is for training, the other for validation, and the next for testing
	localSample <- split(sample(1:len, len), 1:2)
	return(localSample)
}





randomSample <- customSample(dim(data)[1])
model1 <- lm(Life.expectancy ~ BMI + Alcohol, data[unlist(randomSample["1"]),])
pred <- predict(model1, data[unlist(randomSample["2"]),], type="response")
#corNum <- cor(pred,data[unlist(randomSample["2"]),]$Life.expectancy)

pY <- pretty(data[unlist(randomSample["2"]),]$Life.expectancy)
pX <- pretty(pred)
plot(x=pred, y=data[unlist(randomSample["2"]),]$Life.expectancy, ylim=c(pY[1], pY[length(pY)]), xlim=c(pX[1], pX[length(pX)]), ann=FALSE, axes=FALSE, pch=16, col=colorsT[1])
abline(model1, col=colorsT[1], lwd=2)
text(75,45, labels=paste("AIC=",floor(extractAIC(model1)[2])))
axis(1, at=pX, labels=pX, tck=.01, cex.axis=.7, mgp=c(3,.3,0))
axis(2, at=pY, labels=pY, tck=.01, las=2, cex.axis=.7, mgp=c(3,.3,0))
mtext("Predicted Life Expextancy", side=1, line=1.5, cex=.8)
mtext("Real Life Expectancy", side=2, line=1.5, cex=.8)
mtext("Alcohol and BMI as Predictor", side=3, line=1.2)

#second model
randomSample <- customSample(dim(data)[1])
model2 <- lm(Life.expectancy ~ GDP + Population, data[unlist(randomSample["1"]),])
pred <- predict(model2, data[unlist(randomSample["2"]),], type="response")
#corNum <- cor(pred,data[unlist(randomSample["2"]),]$Life.expectancy)

pY <- pretty(data[unlist(randomSample["2"]),]$Life.expectancy)
pX <- pretty(pred)
plot(x=pred, y=data[unlist(randomSample["2"]),]$Life.expectancy, ylim=c(pY[1], pY[length(pY)]), xlim=c(pX[1], pX[length(pX)]), ann=FALSE, axes=FALSE, pch=16, col=colorsT[2])
abline(model1, col=colorsT[2], lwd=2)
text(80,45, labels=paste("AIC=",floor(extractAIC(model2)[2])))
axis(1, at=pX, labels=pX, tck=.01, cex.axis=.7, mgp=c(3,.3,0))
axis(2, at=pY, labels=pY, tck=.01, las=2, cex.axis=.7, mgp=c(3,.3,0))
mtext("Predicted Life Expextancy", side=1, line=1.5, cex=.8)
mtext("Real Life Expectancy", side=2, line=1.5, cex=.8)
mtext("GDP and Population as Predictor", side=3, line=1.2)



randomSample <- customSample(dim(data)[1])
model3 <- lm(Life.expectancy ~ Income.composition.of.resources + Schooling, data[unlist(randomSample["1"]),])
pred <- predict(model2, data[unlist(randomSample["2"]),], type="response")

pY <- pretty(data[unlist(randomSample["2"]),]$Life.expectancy)
pX <- pretty(pred)
plot(x=pred, y=data[unlist(randomSample["2"]),]$Life.expectancy, ylim=c(pY[1], pY[length(pY)]), xlim=c(pX[1], pX[length(pX)]), ann=FALSE, axes=FALSE, pch=16, col=colorsT[3])
abline(model1, col=colorsT[3], lwd=2)
text(80,45, labels=paste("AIC=",floor(extractAIC(model3)[2])))
axis(1, at=pX, labels=pX, tck=.01, cex.axis=.7, mgp=c(3,.3,0))
axis(2, at=pY, labels=pY, tck=.01, las=2, cex.axis=.7, mgp=c(3,.3,0))
mtext("Predicted Life Expextancy", side=1, line=1.5, cex=.8)
mtext("Real Life Expectancy", side=2, line=1.5, cex=.8)
mtext("Income and Schooling as Predictor", side=3, line=1.2)





randomSample <- customSample(dim(data)[1])
model4 <- lm(Life.expectancy ~ Schooling + Alcohol, data[unlist(randomSample["1"]),])


pred <- predict(model2, data[unlist(randomSample["2"]),], type="response")

pY <- pretty(data[unlist(randomSample["2"]),]$Life.expectancy)
pX <- pretty(pred)
plot(x=pred, y=data[unlist(randomSample["2"]),]$Life.expectancy, ylim=c(pY[1], pY[length(pY)]), xlim=c(pX[1], pX[length(pX)]), ann=FALSE, axes=FALSE, pch=16, col=colorsT[4])
abline(model1, col=colorsT[4], lwd=2)
text(80,45, labels=paste("AIC=",floor(extractAIC(model4)[2])))
axis(1, at=pX, labels=pX, tck=.01, cex.axis=.7, mgp=c(3,.3,0))
axis(2, at=pY, labels=pY, tck=.01, las=2, cex.axis=.7, mgp=c(3,.3,0))
mtext("Predicted Life Expextancy", side=1, line=1.5, cex=.8)
mtext("Real Life Expectancy", side=2, line=1.5, cex=.8)
mtext("Schooling and Alcohol as Predictor", side=3, line=1.2)