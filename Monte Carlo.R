##call packages
library(tidyquant)

###BBCA
BBCA = tq_get("BBCA.JK",get = "stock.prices")

## using rate of change
BBCA$changes <- 1 + ((BBCA$close - lag(BBCA$close))/BBCA$close)

mean_BBCA_closing <- mean(BBCA$changes[!is.na(BBCA$changes)])

sd_BBCA_closing <- sd(BBCA$changes[!is.na(BBCA$changes)])

n_simulation <- 100000

# function to automate the simulation
generate.path <- function(){
  days <- 255
  changes <- rnorm(255,mean=mean_BBCA_closing,sd=sd_BBCA_closing)
  sample.path <- cumprod(c(BBCA$close[length(BBCA$close)],changes))
  return(sample.path)
}

simulation <- tibble(day=seq(1, 256))

for (x in 1:n_simulation) {
  simulation[[paste0("sim_",x)]] <- generate.path()
}

# Prepare the data
days <- simulation$day
simulations <- simulation[, -1]  # Exclude the 'day' column

mean_row <- rowMeans(simulations) # mean of each row
no_change_value <- simulations[1, 1] #no change value

# Plot the first time series
plot(days, simulations$sim_1, 
     type = "l", 
     col = "blue", 
     lwd = 2, 
     xlab = "Day", 
     ylab = "Price in Rupiah", 
     main = "Monte Carlo Simulation of BBCA Stock Price", 
     ylim = range(simulations))

# Add additional time series
colors <- rainbow(ncol(simulations))  # Generate a color for each simulation

for (i in 1:ncol(simulations)) {
  lines(days, simulations[[i]], 
        type = "l", 
        col = colors[i], 
        lwd = 2)
}

# Add the mean row to the plot
lines(days, mean_row, 
      type = "l", 
      col = "black", 
      lwd = 3, 
      lty = 1)

#add no change line
abline(h = no_change_value, 
       col = "black", 
       lwd = 3, a
       lty = 2)

print((mean_row[255] - no_change_value) / no_change_value *100)
