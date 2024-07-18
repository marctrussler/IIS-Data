
#Normal with shading function
x <- seq(min,max, .001)
y <- dnorm(values)
plot(x, y, main="Standard Normal CDF", 
     type="l")
min <- -3
max <- 3
mu = 0
sd = 1
value.one = -1
value.two = 1
greater = F
between = F
color = "gray80"


normal.shader <- function(min, max, mu, sd, value.one, value.two=NULL, greater=F, between=F, color="gray80"){
  
  x <- seq(min,max, .001)
  y <- dnorm(values)
  
  if(between == T){
    polygon(c(x[x>=value.one & x<=value.two], value.two, value.one), c(y[x>=value.one & x<=value.two], 0, 0), 
            col=color, border=NA)
    abline(v=value.one,lty=2, lwd=2)
    abline(v=value.two, lty=2,lwd=2)
  }else if(greater==T){
    polygon(c(x[x>=value.one], max(x), value.one), c(y[x>=value.one], 0, 0), col=color,
            border=NA)
    abline(v=value.one,lty=2, lwd=2)
    
  } else {
    polygon(c(rev(x[x<=value.one]), min(x), value.one), c(rev(y[x<=value.one]), 0, 0), col=color,
            border=NA)
    abline(v=value.one,lty=2, lwd=2)
    area <- pnorm(value.one, mean=mu, sd=sd, lower.tail = T)
  }
}


