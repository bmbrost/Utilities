# Violin plot for each column in z

# N <- 100
# z <- matrix(rnorm(N*3),N)
# violin(z,ylab="This is only a test",xlab=c("Violin 1", "Violin 2", "Violin 3"))

violin <- function(z,xlab=NULL,ylab=NULL,ylim=NULL,yat=NULL,bw="SJ",width=0.35,violin.col="gray75",
	x.buff=0.5,y.buff=0){

	# browser()
	
	# Create violins
	n <- ncol(z)  # number of violins
	den <- apply(z,2,function(x) density(x,bw=bw))  # kernel density
	poly <- lapply(den,function(x)  # rotate and close polygon to create violin
		cbind(c(x$y,0-rev(x$y))/max(x$y)*width,c(x$x,rev(x$x))))
	poly <- sapply(1:n,function(x)  # adjust x-axis values
		cbind(poly[[x]][,1]+x,poly[[x]][,2]),simplify=FALSE)

	# Determine plot axis limits
	if(is.null(ylim)){
		ylim <- range(unlist(lapply(poly,function(x) range(x[,2]))))+c(-y.buff,y.buff)
	}
	xlim <- c(1-x.buff,n+x.buff)

	# Setup plot
	if(is.null(yat)){  # use default y-axis 
		plot(1,pch="",xlim=xlim,ylim=ylim,xaxt="n",xpd=TRUE,ylab=ylab,xlab="",las=1)
	}
	if(!is.null(yat)){  # use user-specified y-axis tick marks and lables
		plot(1,pch="",xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xpd=TRUE,ylab=ylab,xlab="",las=1)
		axis(2,at=yat,labels=TRUE,cex.axis=1,las=1)
	}
	axis(1,at=1:n,labels=xlab,cex.axis=1)  # add x-axis labels
	
	# axis(4,at=seq(-10,10,0.5),labels=TRUE,las=1,cex.axis=cex)	
	# mtext(expression(hat(beta)[1]),side=4,line=3.1,cex=1,las=1)

	# Plot violins
	lapply(poly,polygon,col=violin.col)	# plot violin

	# Plot means
	avg <- apply(z,2,mean) # means
	points(1:n,avg,pch=19,col=1)  # polot means
}

