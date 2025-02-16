### Une fonction pour le choix des classes d'un histogramme régulier 
### (Birgé-Rozenholc)

HISTSELECT2 = function(x,A=min(x),B=max(x), col=NULL, border="black", freq=TRUE,  right=TRUE, xlab =NA, ylab=NA, main=NA, nmax=NA, ylim=NULL)
{ if(is.na(ylab)){if(freq==TRUE){ylab="Frequency"}else{ylab="Density"}}
  {if(is.na(main)){main=paste("Histogram of", deparse(substitute(x)))}}
  {if(is.na(xlab)){xlab=deparse(substitute(x))}}
  if(A>=B){cat("The limits of the chosen interval are not consistent")}
  else
  {
    n<-length(x)
    a<-min(x)
    b<-max(x)
    a2<-max(a,A)
    b2<-min(b,B)
    if(B<=a | A >=b){cat("There are no observations in the chosen interval")}
    else{
      y<-(x<a2|x>b2)
      n2<-table(y)[1]
      if(is.na(nmax)){nmax<-min(floor(n2/log(max(n2,exp(1)))), 250)}
      #if(is.na(nmax)){nmax<-min(floor(0.3+n2^(0.575)),150)}
      L <- vector(mode="numeric", length=nmax)
      for(D in 1:nmax)
      {
        bins<-a2+(b2-a2)*seq(0,1,1/D)
        N<-hist(x[y==FALSE], breaks=bins, right=right, plot=FALSE)$count
        #if(n2<150){pen<--D+1-(log(D))^(n*2.5/150)}else{pen<--D+1-(log(D))^(2.5)}
        pen<--D+1-(log(D))^(2.5)
        L[D]<-sum(N*log(pmax(N,1)))+n2*log(D)+pen
      }
      D1<-which.max(L)
      D2<-order(L)[2]
      D3<-order(L)[3]
      if(n2<15){Dstar<-D1}
      if(n2>14&n2<41){if(D1>1){Dstar<-D1}else{Dstar<-D2}}
      if(n2>40){if(D1>2){Dstar<-D1}else{if(D2>2){Dstar<-D2}else{Dstar<-D3}}}
      cat("Number of observations in the chosen interval:", n2, "\n")
      cat("Partition size:", Dstar, "\n")
      if(a2==a&b2==b)
      {
        hist(x,breaks=a2+(b2-a2)*seq(0,1,1/Dstar),freq=freq, right=right, xlim=c(a2,b2), col=col, border=border,xlab=xlab, ylab=ylab, main=main, ylim=ylim)
      }
      if(a2>a&b2==b)
      {
        a3<-a2-ceiling((a2-a)/((b2-a2)/Dstar))*(b2-a2)/Dstar
        hist(x,breaks=seq(a3,b2,(b2-a2)/Dstar),freq=freq, right=right, xlim=c(a2,b2),col=col, border=border, xlab=xlab, ylab=ylab, main=main, ylim=ylim)
        points(a2,0,col="black",pch=20)
      }
      if(a2==a&b2<b)
      {
        b3<-b2+ceiling((b-b2)/((b2-a2)/Dstar))*(b2-a2)/Dstar
        hist(x,breaks=seq(a2,b3,(b2-a2)/Dstar),freq=freq, right=right, xlim=c(a2,b2), col=col, border=border, xlab=xlab, ylab=ylab, main=main, ylim=ylim)
        points(b2,0,col="black",pch=20)
      } 
      if(a2>a&b2<b)
      {
        a3<-a2-ceiling((a2-a)/((b2-a2)/Dstar))*(b2-a2)/Dstar
        b3<-b2+ceiling((b-b2)/((b2-a2)/Dstar))*(b2-a2)/Dstar
        hist(x,breaks=seq(a3,b3,(b2-a2)/Dstar),freq=freq, right=right, xlim=c(a2,b2), col=col, border=border, xlab=xlab, ylab=ylab, main=main, ylim=ylim)
        points(a2,0,col="black",pch=20)
        points(b2,0,col="black",pch=20)
      } 
    }
  }
  S<-Dstar
}
