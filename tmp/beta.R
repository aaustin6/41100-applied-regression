getwd()

corp <- read.csv("Beta.csv")
len<-dim(corp)[1]
RetSPX<-diff(log(corp$SPX))*52-corp$TBILL[2:(len)]/100       ## or log(corp$SPX[2:len])-log(corp$SPX[1:len-1])
RetAAPL<-diff(log(corp$AAPL))*52-corp$TBILL[2:(len)]/100
plot(RetSPX,RetAAPL,xlab="SPX Returns",ylab="AAPL Returns")
reg<-lm(RetAAPL~RetSPX)
xx <- seq(range(RetAAPL)[1],range(RetAAPL)[2],length=4)
lines(xx, reg$coeff[1] + reg$coeff[2]*xx, col=2)

Retcorp<-52*log(corp[2:len,2:(dim(corp)[2]-2)])-52*log(corp[1:len-1,2:(dim(corp)[2]-2)])-corp$TBILL[2:(len)]/100

## Run the regression for each of the 5 corps
print(CAPM <- lm(as.matrix(Retcorp) ~ RetSPX))
plot(CAPM$coeff[2,], CAPM$coeff[1,],
     ylab="alpha", xlab="beta", col=0)
text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(corp)[2:(dim(corp)[2]-2)], col=2)
