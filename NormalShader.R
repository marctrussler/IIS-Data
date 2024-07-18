
#Normal with shading function
#x <- seq(-20,30,.001)
#y <- dnorm(x, mean=5, sd=10)
#plot(x,y, type="l", main="Normal with mu=5, sigma=10")
#
#min=-20
#max=30
#mu=5
#sd=10
#value.one=-5
#value.two=15
#between=T
#color="gray80"

normal.shader <- function(min, max.val, mu, sd, value.one, value.two=NULL, greater=F, between=F, color="gray.val80"){
  
  x.val <- seq(min,max, .001)
  y.val <- dnorm(x.val, mean=mu, sd=sd)
  
  if(between == T){
    polygon(c(x.val[x.val>=value.one & x.val<=value.two], value.two, value.one), c(y.val[x.val>=value.one & x.val<=value.two], 0, 0), 
            col=color, border=NA)
    abline(v=value.one,lty=2, lwd=2)
    abline(v=value.two, lty=2,lwd=2)
  }else if(greater==T){
    polygon(c(x.val[x.val>=value.one], max.val(x.val), value.one), c(y.val[x.val>=value.one], 0, 0), col=color,
            border=NA)
    abline(v=value.one,lty=2, lwd=2)
    
  } else {
    polygon(c(rev(x.val[x.val<=value.one]), min(x.val), value.one), c(rev(y.val[x.val<=value.one]), 0, 0), col=color,
            border=NA)
    abline(v=value.one,lty=2, lwd=2)
  }
}


