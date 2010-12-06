
twoord.stackplot <- function (lx, rx, ldata, rdata, lcol, rcol, ltype, rtype, border, 
    rylab, lylab, xlab, ..., incrylim = NULL, halfwidth = 0.4, 
    leftfront = FALSE, mar = c(5, 4, 4, 4)) 
{
    ltype <- sapply(ltype, function(x) match.arg(x, c("p", "l", 
        "b", "c", "o", "bar")))
    rtype <- sapply(rtype, function(x) match.arg(x, c("p", "l", 
        "b", "c", "o", "bar")))
    incrylim <- ifelse(is.null(incrylim), 0, as.numeric(incrylim))
    xlimits <- range(lx, rx)
    oldmar <- par("mar")
    par(mar = mar)
    if (leftfront) {
        twoord.stackplot(rx, lx, rdata, ldata, rcol, lcol, rtype, 
            ltype, border, lylab, rylab, xlab, ..., incrylim = NULL, 
            halfwidth = 0.4, leftfront = FALSE, mar = c(5, 4, 
                4, 4))
        return(invisible())
    }
    if (NCOL(ldata) > 1) {
        lcol <- rep(lcol, length = NCOL(ldata))
        ltype <- rep(ltype, length = NCOL(ldata))
    }
    if (any(ltype == "bar")) {
        lylimits <- range(ifelse(ldata < 0, ldata, 0), rowSums(ldata))
        lylimits[1] <- ifelse(lylimits[1] > 0, lylimits[1] * 
            (1 - incrylim), lylimits[1] * (1 + incrylim))
        lylimits[2] <- ifelse(lylimits[2] > 0, lylimits[2] * 
            (1 + incrylim), lylimits[2] * (1 - incrylim))
            
        plot(0, type = "n", axes = FALSE, xlim = xlimits, ylim = lylimits, 
            ylab = "", xlab = xlab, ...)
        xbottom <- par("usr")[1]
        xylim <- par("usr")

		ldata_cumsum <- t(apply(ldata, 1, cumsum))
        ly <- ldata_cumsum[, 1]
		
        rect(lx - halfwidth, ifelse(ly < 0, ly, xbottom), lx + 
            halfwidth, ifelse(ly < 0, 0, ly), col = lcol[1], 
            border = border)
            
        for (i in 2:NCOL(ldata))
			rect(lx - halfwidth, ldata_cumsum[, i-1], 
				 lx + halfwidth, ldata_cumsum[, i], 
				 col = lcol[i], border = border)
		
    }
    else {
        lylimits <- range(ldata)
        lylimits[1] <- ifelse(lylimits[1] > 0, lylimits[1] * 
            (1 - incrylim), lylimits[1] * (1 + incrylim))
        lylimits[2] <- ifelse(lylimits[2] > 0, lylimits[2] * 
            (1 + incrylim), lylimits[2] * (1 - incrylim))
        plot(lx, ldata[, 1], xlim = xlimits, ylim = lylimits, 
            col = lcol[1], type = ltype[1], axes = FALSE, ylab = "", 
            xlab = xlab, ...)
        for (i in 2:NCOL(ldata)) lines(lx, ldata[, i], col = lcol[i], 
            type = ltype[i], ...)
    }
    xylim <- par("usr")
    mtext(lylab, 2, 2, col = lcol[1])
    axis(1)
    axat <- axis(2, col = lcol[1], labels = FALSE)
    abline(v = xylim[1], col = lcol[1])
    mtext(axat, 2, 1, at = axat, col = lcol[1])
    box()
    par(new = TRUE)
    if (NCOL(rdata) > 1) {
        rcol <- rep(rcol, length = NCOL(rdata))
        rtype <- rep(rtype, length = NCOL(rdata))
    }
    if (any(rtype == "bar")) {
        rylimits <- range(ifelse(rdata < 0, rdata, 0), rowSums(rdata))
        rylimits[1] <- ifelse(rylimits[1] > 0, rylimits[1] * 
            (1 - incrylim), rylimits[1] * (1 + incrylim))
        rylimits[2] <- ifelse(rylimits[2] > 0, rylimits[2] * 
            (1 + incrylim), rylimits[2] * (1 - incrylim))

        plot(0, type = "n", axes = FALSE, xlim = xlimits, ylim = rylimits, 
            ylab = "", xlab = "", ...)
        xbottom <- par("usr")[1]
        xylim <- par("usr")
		
		rdata_cumsum <- t(apply(rdata, 1, cumsum))
        ry <- rdata_cumsum[, 1]
		
        rect(rx - halfwidth, ifelse(ry < 0, ry, xbottom), rx + 
            halfwidth, ifelse(ry > 0, ry, 0), col = rcol[1], 
            border = border)
		
        for (i in 2:NCOL(rdata))
			rect(lx - halfwidth, rdata_cumsum[, i-1], 
				 lx + halfwidth, rdata_cumsum[, i], 
				 col = rcol[i], border = border)
    }
    else {
        rylimits <- range(rdata)
        rylimits[1] <- ifelse(rylimits[1] > 0, rylimits[1] * 
            (1 - incrylim), rylimits[1] * (1 + incrylim))
        rylimits[2] <- ifelse(rylimits[2] > 0, rylimits[2] * 
            (1 + incrylim), rylimits[2] * (1 - incrylim))
        plot(rx, rdata[, 1], xlim = xlimits, ylim = rylimits, 
            col = rcol[1], type = rtype[1], axes = FALSE, ylab = "", 
            xlab = "", ...)
        for (i in 2:NCOL(rdata)) lines(rx, rdata[, i], col = rcol[i], 
            type = rtype[i], ...)
    }
    axat <- axis(4, col = rcol[1], labels = FALSE)
    abline(v = xylim[1], col = rcol[1])
    mtext(axat, 4, 1, at = axat, col = rcol[1])
    mtext(rylab, 4, 2, col = rcol[1])
    par(mar = oldmar)
}


# 
# 
# time <- 0:25
# 
# A <- 1+1/2*sin(time/2)
# B <- A + rnorm(length(A), sd=1/10)
# B <- B + rnorm(length(A), sd=1/10)
# 
# sizeA <- floor(450*(1 + 1/4*sin(time/2+2))*(1+.1))
# sizeB <- 10*rpois(time, 100)
# sizeC <- 1000-sizeA
# 
# 
# C <- (A*sizeA + B*sizeB)/(sizeA+sizeB)
# D <- C + rnorm(time, sd=1/10)
# 
# #many things plotted. we change the line width with lwd for rdata
# #
# 
# twoord.stackplot(lx=time, rx=time, ldata=cbind(sizeA, sizeB, sizeC), 
# 	rdata=cbind(A, B, C, D),  lcol=c("grey80", "grey90", "white"), 
# 	rcol=c("blue", "red", "black", "green"), ltype="bar", 
# 	rtype=c("l","p","o","b"), border="grey80", lylab="Size", 
# 	rylab="A, B, C, D", xlab="Time", main="a plot", lwd=0.8)
	