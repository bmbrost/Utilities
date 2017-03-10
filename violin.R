# Violin plot for each column in z

# N <- 100
# y <- matrix(rnorm(N*3),N)
# violin(y,ylim=c(-3,3))

violin <- function(z,bw="SJ",width=0.35,ylim=c(-1,1),cex=1.2,
	xlab=NULL,ylab=NULL){
	n <- ncol(z)
	avg <- apply(z,2,mean)
	plot(1,pch="",xlim=c(0.5,n+0.5),ylim=ylim,xaxt="n",yaxt="n",xpd=TRUE,ylab=ylab,xlab="")
	axis(1,at=1:n,labels=xlab,cex.axis=cex)
	axis(2,at=seq(0,1,0.2),labels=TRUE,las=1,cex.axis=cex)	
	# axis(4,at=seq(-10,10,0.5),labels=TRUE,las=1,cex.axis=cex)	
	# mtext(expression(hat(beta)[1]),side=4,line=3.1,cex=1,las=1)
	den <- apply(z,2,function(x) density(x,bw=bw))  # kernel density
	poly <- lapply(den,function(x)  # rotate and close polygon to create violin
		cbind(c(x$y,0-rev(x$y))/max(x$y)*width,c(x$x,rev(x$x))))
	poly <- sapply(1:n,function(x)  # adjust x-axis values
		cbind(poly[[x]][,1]+x,poly[[x]][,2]),simplify=FALSE)
	lapply(poly,polygon,col="gray75")	# plot violin
	points(1:n,avg,pch=19,col=1)  # polot means
}

