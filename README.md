# Easy-R_Jags example

R_Jags is an open_source library (package) for developing Bugs commands in R enviroment. In other words, it's a very important and  confident tool to implement MCMC equations in R.
In this example we have the airline.csv dataset and we want to estimate airplane fatals with some baysian models.
It is an easy way to getting started with the R-Jags Library.

## Getting Started...

[R_Jags](https://www.r-bloggers.com/getting-started-with-jags-rjags-and-bayesian-modelling/)

## Installing Jags in R.
```R
install.packages("rjags") #this will install the package
library(rjags) #this will load the package and make it usable
```

### Some things about the example

+ This example which specifies the estimation of airplain fatals, is based on statistical baysian theory. So we have to use statistical theory and imagination too!
Imagination because in baysian problems we need to imagine of which distribution the data have been generated. 

+ In this example we assume that the 'fatals' have been generated by [Poisson](https://en.wikipedia.org/wiki/Poisson_distribution) distribution only with one parameter 'mu' which is the mean of data observations (fatals).
So we have y = fatals , mu = mean(fatals) and in conclusion we can say that `y|mu ~ Poisson(mu)`.This is the first thing we must do when we have to solve a baysian problem.
Then we can say that the posterior distribution, that one which the mean fits, is [Gamma](https://en.wikipedia.org/wiki/Gamma_distribution).
So the posterior distribution for mu is Gamma(0 + Sum(y) , 0 + n), which 'y' is the fatals and 'n' is the number of observations.
*Now we can draw the posterior*

[![Posterior Curve](https://s11.postimg.org/olc31lu8z/Posterior.png)](https://postimg.org/image/nj1wj2bfj/)

*red line is mean and the blue line is median.*

+ Then we have to implement the jags model which we have to cat it in R and store it in a `file.jag` as it being in `example-rjags.R` script.
Follow the script to make this model you've already cat be ready to run. You have to initialize some values and set how much iterations you want to make. In the end you have the plots of what you have done.

[![Final](https://s22.postimg.org/737mu9ytt/Rplot.png)](https://postimg.org/image/c1v58t2ml/)

