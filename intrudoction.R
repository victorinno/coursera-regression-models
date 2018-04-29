library(swirl)
#XboBqrRc7gqEWi0g
library(devtools)
library(Biobase)
library(UsingR)

data(galton)
#?formula

regrline <- lm(child ~ parent, galton)



plot(jitter(child,4) ~parent, galton)

abline(regrline, lwd=3, col='red')

summary(regrline)
