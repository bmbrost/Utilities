# Function to create 'halo' around text

# shadowtext(-312000,1999500,"b.)",col=1,bg="white",cex=1.5)

shadowtext <- function(x, y=NULL, labels, col='white', bg='black', 
    theta= seq(0, 2*3.1459, length.out=50), r=0.3, ... ) {
    xy <- xy.coords(x,y)
    xo <- r*strwidth('A')
    yo <- r*strheight('A')

    # draw background text with small shift in x and y in background colour
    for (i in theta) {
        text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
    }
    # draw actual text in exact xy position in foreground colour
    text(xy$x, xy$y, labels, col=col, ... )
}
