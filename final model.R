#final model 
# if you want relative paths, open up R interpreter
# and use with source( 'final model.R', chdir=T )
# wonky I know, such is life

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
# E = na.omit(E)
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

# use $y of the density function as input
peaks.valleys = function(d)
{
  d. = d[-1]-d[-length(d)] # first derivative
  return(which( d.[-1]*d.[-length(d.)] <0)+1)  #locations of turning points, derivative is 0, peaks and valleys
}# there will always be an odd number of them because its a pdf, 
#integrates to 1, choose every other as spliting point 
#then use peak as the 1-point approximation

approx = function(m,hotw=round(64/24, 3), c=6)
{
  n = nrow(m)
  results = vector("numeric", n) #categorical, which cluster it belongs to
  d = density(m[m[,4]==hotw,c], bw="SJ")
  pv = peaks.valleys(d$y)
  peaks = pv[seq(1,length(pv), 2)]
  valleys = pv[seq(0,length(pv), 2)]
  print(pv)
  for( i in 1:n )
  {
    results[i] = 1+sum(m[i,c]>d$y[valleys])
  }
  return(d$x[peaks[results]])
}

#CONVENTION (note, the dot has no syntactic meaning, it's part of the name)
# m for model, m.t is the predicted factor wrt that temperature
# f for factor
# for classical decomposition (global + seasonal + time series),
# hotw should be additive, maybe estimated from exponential 
# decaying historical data? ie, if we take the non-parametric route.

# dpill from library KernSmooth takes O(n^2)
# default method SJ from locpoly takes comparable time, I think
# if it needs to be scaled up, use optimized kNN instead
m.t = locpoly()
f.building.type = 1 # vector eventually for buildings
f.occupancy =  #frequency * hotw estimate * building-specific estimate based on building type
f.temp =  m.t[1]*f.building.type*() #building type * degrees away from baseload as determined by linear fit

# note all factors are 
plot(row.names(ET), (e.total - f.occuancy - f.temp)/hotw)

#data frame with year month day hour# hour format as row name
readable.matrix = function(m, col.date=1) { return(data.frame(m, row.names=sapply(m[,col.date], readable.date))) }
readable.date = function(d) { return(paste(format(as.Date(d,origin="1970-01-01"), "%Y %B %d"), "hour#", round(24*(d - floor(d))))) }
