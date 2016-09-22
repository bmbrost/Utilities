# Create color ramp legend for continous values

# ramp.black <- colorRampPalette(c("white","black"),bias=1)(2000)  # raster color palette
# par(mar=c(5.1,4.1,2.1,5.1))
# colorramp(z.max=1,ramp.black,seq(0,1,0.1),"Distance (m)")

colorramp <- function(z.max,ramp,at,label){
	plot(c(0,1),c(0,z.max),type="n",axes=FALSE,xaxs="i",yaxs="i",xlab="",ylab="")	
	axis(4,at=at,las=1)
	mtext(label,4,line=3.5)	
	rasterImage(as.raster(matrix(rev(ramp.black)),ncol=1),0,0,1,z.max)
	box()
}






