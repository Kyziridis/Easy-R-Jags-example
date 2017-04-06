###################################
##~~ Rjags for airplain fatals ~~##
###################################

#load packs
library(rjags)
#fix the working directory and insert the airline data_set
wd <-getwd()
if (wd != "/media/NTFS1/5_Documents/BDA2012") {
  setwd("YOUR_PATH") #Use appropriate slashes
}
airline <- read.csv( "airline.csv" )

###########################################################

# take a look at the data_set
nrow( airline )
sum( airline$fatal )
# so we have 26 observations wich have sum = 634
# y|mu ~ Poisson(mu) and mu~Gamma(0,0) 
# Gamma(0,0) =  almost uniform(0,+inf)
# so the posterior for mu ~ G(0 + Sum(y) , 0 + n)
#'n' is the number of observetions, in this case n=26 and sum(y)=634

mn <-mean(xx<- rgamma(10000 , 634 , 26))
md <- median(xx)
#mean = 24.35343 and median = 24.34909

#we can draw the posterior 
curve( dgamma( x, 634, 26 ), from=20, to=30, lwd=2 )
abline( v=mn, col="red", lwd=3 )
abline( v=md, col="blue" )

# we make a list with the y variable(fatal) with an extra NA for the prediction
# and I=27
a.dat <- list(fatal = c(airline$fatal , NA) , 
              I =  1+length(airline[,1]))

# make the m1.jag 
cat ( "model 
        {
        for ( i in 1:I)
          { 
          fatal[i] ~ dpois(mu)
          }
        mu ~ dgamma(0.1 , 0.1)
        }",      
        file = "m1.jag")

#initial values to mu = 20 , 23 , 26
#we always have to initialize the parameters before start the MCMC
#we specify list of 3 parameters so we will roll three paraller chains 
a.ini <- list ( list(mu=20) ,
                list(mu=23),
                list(mu=26) )

#Compiling-adapting
#we ask rjags for 3 chains and 2000 cycles to burn-in
m <- jags.model(file = "m1.jag" , 
              data = a.dat ,
              n.chains = 3 , 
              inits = a.ini , 
              n.adapt = 2000)

#Parameters and simulated parameters
#we specify : nodes (variable names) , iterations(n.iter)
# and how often we sample and retain the result in memory (thin)
res <- coda.samples(m ,
                    var = "mu",
                 n.iter = 10000,
                   thin = 10)
class(res)
# the result of this is a mcmc.list with 3 elements each of them is 1000x1 matrix

str(res)
summary(res)
# If we decide that the set of samples from the 3 chains provides
# a reasonable representation of the posterior distribution, 
# we can get an overview of the three chains by using the function plot.mcmc.list:
par(mfrow=c(1,2))
plot(res)
curve( dgamma( x, sum(airline$fatal), nrow(airline) ), from=20, to=30, lwd=2, col="red", add=TRUE )

#collect the posterior samples from different chains in a matrix
rmat<- as.matrix(res)
head(rmat)
