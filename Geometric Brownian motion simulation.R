
library(tseries);
library(zoo);

APPLE_prices <- get.hist.quote(instrument="aapl", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(APPLE_prices) <- as.yearmon(index(APPLE_prices))

P <- APPLE_prices
dP <- diff(APPLE_prices)

#dP/P follows a brownian motion with drift of mean mu and variance sigma
#dP/P=mu*dt + sigma*dz, where dz is normal with variance dt

#Simulation of the stock price: by Ito's lemma, P_t=P_0 exp((mu-sigma^2/2)t)exp(sigma(z_t-z_0)), with z_t-z_0 is normal of mean zero sd sqrt(t)
#Simulation using Ito's lemma on del(log(P)) with del(t)=1

Del_t <- 1 #Actual variation of time in years
Del_log_P <- diff(log(APPLE_prices))

sigma <-sd(Del_log_P)
mu <- 0.5*sigma^2+mean(Del_log_P)

t <- seq(from=0, to=191, by=1)
Del_log_P_sim <- (mu-0.5*sigma^2)+ sigma*rnorm(191, mean=0, sd=sqrt(1))
Del_log_P_sim  <- as.zoo(Del_log_P_sim)
index(Del_log_P_sim) <- as.yearmon(index(Del_log_P))

plot(Del_log_P_sim, type="l", col="orange", ylab="P", main="Simulation of stock price for Apple")
lines(Del_log_P, type="l", col="blue")
legend(x="topleft", legend=c("Observed", "Simulation"), col=c("blue", "orange"), lwd=1, cex=0.7)



P_sim <- rep(0, times=192)
P_sim[1] <- P[1]

for(k in 1:191){
  
  P_sim[k+1] <- exp(sum(Del_log_P_sim[1:k]))*P[1]
}

P_sim  <- as.zoo(P_sim)
index(P_sim) <- as.yearmon(index(P))

plot(P_sim, type="l", col="orange", ylab="P", main="Simulation of stock price for Apple")
lines(P, type="l", col="blue")
legend(x="topleft", legend=c("Observed", "Simulation"), col=c("blue", "orange"), lwd=1, cex=0.7)





