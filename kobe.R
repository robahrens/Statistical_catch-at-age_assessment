require( KernSmooth)
kobe.egg=function(xx,yy)
{
    bw=25
    bwx=(max(xx,na.rm=T)-min(xx,na.rm=T))/bw; bwy=(max(yy,na.rm=T)-min(yy,na.rm=T))/bw
    est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy),gridsize=c(81, 81))
    est$fhat=est$fhat/max(est$fhat,na.rm=T)
    plot(xx,yy,xlim=c(0,2.5),ylim=c(0,2.5),xlab=NA,ylab=NA,type="n",xaxs="i",yaxs="i")
    lvs=c(0.01,0.1,0.8,0.99)
    maxct=max(lvs)
    nlvs=length(lvs)
    thelines=contourLines(est$x1,est$x2,est$fhat,levels=lvs)
    polygon(thelines[[nlvs-3]]$x,thelines[[nlvs-3]]$y,col="khaki",border="khaki",lwd=1)
    polygon(thelines[[nlvs-2]]$x,thelines[[nlvs-2]]$y,col="snow",border="snow1",lwd=2)
    polygon(thelines[[nlvs-1]]$x,thelines[[nlvs-1]]$y,col="yellow",border="yellow2",lwd=3)
    polygon(thelines[[nlvs]]$x,thelines[[nlvs]]$y,col="lightyellow",border="yellow",lwd=1)
    #contour(est$x1,est$x2,est$fhat,drawlabels=T,add=T,levels=lvs,lty=1,lwd=1,labcex= 0.7)
    #Add salt and pepper
    xi=sample(1:length(xx),100)
    points(xx[xi],yy[xi],pch=".",col=grey(0:10/10))
    lines(x=c(0.2,0.8),y=c(0,1),lwd=2,col="red")
    lines(x=c(0.8,2.5),y=c(1,1),lwd=2,col="red")
    mtext(expression(B["cur"]/B["msy"]),side=1,line=2.5,outer=F,cex=1.2)
    mtext(expression(F["cur"]/F["msy"]),side=2,line=2.5,outer=F,cex=1.2)
}
refpar=read.table("refpar.mcmc",header=TRUE)
kobe.egg(refpar[,4],refpar[,5])


