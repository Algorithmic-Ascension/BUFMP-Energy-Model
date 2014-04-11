#final model 
# if you want relative paths, open up R interpreter
# and use with source( 'final model.R', chdir=T )
# wonky I know, such is life

library(KernSmooth)
library(cvTools)

# use $y of the density function as input
peaks.valleys = function(d)
{
  d. = d[-1]-d[-length(d)] # first derivative
  return(which( d.[-1]*d.[-length(d.)] <0)+1)  #locations of turning points, derivative is 0, peaks and valleys
}# there will always be an odd number of them because its a pdf

#returns a vector of vectors of peaks and valleys
pvs = sapply( round(0:167/24, 3), function(hotw, m = ET.classroom)
  {
    d = density(m[round(m[,1]%%7,3)==round(hotw,3), 2], bw="SJ")
    pv = peaks.valleys(d$y)
    return(d[pv]$x)
  }
)
peaks = function(pv) { return(pv[seq(1,length(pv), 2)]) }
valleys = function(pv) { return(pv[seq(0,length(pv), 2)]) }
hotw.estimate = function(m, hotw, c=2)
{ # return the nth peak where n is 1+the number of 
  p = peaks(pvs[hotw])
  v = valleys(pvs[hotw])
  return (  p[1+sum(m[i,c]>v)] )
}

#CONVENTION (note, the dot has no syntactic meaning, it's part of the name)
# m for model, m.t is the predicted factor wrt that temperature
# f for factor
# for classical decomposition (global + seasonal + time series),
# hotw should be additive, maybe estimated from exponential 
# decaying historical data? ie, if we take the non-parametric route.

# dpill from library KernSmooth takes O(n^2)
# if it needs to be scaled up, use optimized kNN instead
m.t = function(target.v)
{
  xy = locpoly(ET[,3],ET[,2], bandwidth=dpill(ET[,3], ET[,2]))
  return(sapply( target.v, function(target){ return(xy$y[order(abs(xy$x-target))][1]) }))
  # return y s.t. its corresponding x is the smallest L1 distance to target
}

temperatures.temps = rep(c(2, 2.5, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 1.5), 365)+rnorm(8760)
ET = cbind(E.dates, E.hourly, temperatures.temps)

# predict 1st argument based on 2nd
predict = function(temp.new, ET)
{
  f.building.type = 1 # vector eventually for buildings
  f.occupancy = rep(0, 365) #frequency * building-specific estimate based on building type
  f.temp =  m.t(temp.new)*f.building.type
  hotw = c(0, sapply(1:167/24, function(h) {return(hotw.estimate(ET, h))}))
  
  # note all factors are mututally independent
  #plot(row.names(ET), (e.total - f.occuancy - f.temp)/hotw)
}  
  #data frame with year month day hour# hour format as row name
readable.matrix = function(m, col.date=1) { return(data.frame(m, row.names=sapply(m[,col.date], readable.date))) }
readable.date = function(d) { return(paste(format(as.Date(d,origin="1970-01-01"), "%Y %B %d"), "hour#", round(24*(d - floor(d))))) }


#pvs = sapply(round(0:167/24,3), function(hotw){ return( density(m[round(m[,1]%%7,3)==round(hotw,3), 2], bw="SJ") ) } )
# c is column of the matrix m we want to estimate
# hotw.estimate = function(hotw, c=2)
# {
#   n = nrow(m)
#   results = vector("numeric", n) #categorical, which cluster it belongs to
#   d = density(m[m[,1]%%7==round(hotw,3), c], bw="SJ")
#   pv = peaks.valleys(d$y)
#   peaks = pv[seq(1,length(pv), 2)]
#   valleys = pv[seq(0,length(pv), 2)]
#   #print(pv)
#   for( i in 1:n )
#   {
#     results[i] = 1+sum(m[i,c]>d$y[valleys])
#   }
#   return(d$x[peaks[results]])
# }

### Used for prototyping purposes
# temps = read.csv("Documents/Work/BUFMP-Energy-Model/KMACAMBR9.csv", stringsAsFactors=FALSE)
# temps[,1] = sapply(temps[,1], function(d){ return(as.Date(d)) })
# temps = data.matrix(temps)
# temps = cbind(as.numeric(as.Date(temps[,1], origin="1970-01-01"))+signif(temps[,2]/24, 3), temps[,3])
# 
# 
# #copied wholesale from the other code
# compressBy4 = function(x)
# {
#   result = vector("numeric", length(x)/4)
#   for( i in 0 : (length(x)/4 - 1) )
#   {
#     numbers.of.interest = x[(i*4+1):(i*4+4)]
#     result[i+1] = sum(numbers.of.interest) / (4-sum(is.na(numbers.of.interest)))
#   }
#   return(result)
# }
# 
# E = read.table("~/Documents/Work/BUFMP-Energy-Model/mystery building - 2012.csv", quote="\"", header=TRUE, sep=",", stringsAsFactors=FALSE)
# for(d in 1:nrow(E))
# {
#   E[d,1] = as.Date(E[d,1], "%B %d, %Y")
#   E[d,98] = as.numeric(gsub(",", "", E[d,98]))
# }
# E = data.matrix(E)
# E.dates = as.vector(t( matrix(E[-366,1], nrow=365, ncol=24) + t(matrix(round(0:23/24, 3), nrow=24, ncol=365))))
# E.hourly = compressBy4(as.vector(t(E[-366,2:97]))) # t(matrix(0:23/24, nrow=24, ncol=96))
# E = cbind(E.dates, E.hourly)
# # E = na.omit(E)
# 
# #matching electricity to temperature
# # O(n^2), replace with complete matrix, I only used it because
# # I didn't want to linearly interpolate data points
# ET = c()
# for(i in 1:nrow(E))
# {
#   if(sum(E[i,1]==temps[,1])!=0)
#   {
#     ET = rbind(ET, c(E[i,1], E[i,2] , temps[E[i,1]==temps[,1],2]))
#   }
# }
# col.names(ET, c("Date", "Electricity", "Temperature"))
###End prototype code