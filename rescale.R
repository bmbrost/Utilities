###
### Function to rescale values in x to the interval [a,b]
###

# x <-runif(100)
# z <- rescale(x,0,10)
# hist(z)

rescale <- function(x,a,b){  # rescale x to the interval [a,b]
	mx <- max(x)
	mn <- min(x)
	(b*(x-mn))/(mx-mn)
}
