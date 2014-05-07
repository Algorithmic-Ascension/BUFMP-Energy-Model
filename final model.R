#final model 
# if you want relative paths, open up R interpreter
# and use with source( 'final model.R', chdir=T )
# wonky I know, such is life

library(KernSmooth)
#library(cvTools)
#use crossvalidation to evaluate the model, soon to come

# use $y of the density function as input
peaks.valleys = function(d)
{
  d. = d[-1]-d[-length(d)] # first derivative
  return(which( d.[-1]*d.[-length(d.)] <0)+1)  #locations of turning points, derivative is 0, peaks and valleys
}# there will always be an odd number of them because its a pdf

#index of peaks and valleys in pv
peaks = function(pv) { return(pv[seq(1,length(pv), 2)]) }
valleys = function(pv) { return(pv[seq(0,length(pv), 2)]) }

train = function(ET, c=2)
{ # return the nth peak where n is 1+the number of valleys it's greater than
  pvs = sapply( round(0:167/24, 3), function(hotw, ET. = ET.classroom)
  {
    d = density(ET.[round(ET.[,1]%%7,3)==round(hotw,3), 5], bw="SJ")
    pv = peaks.valleys(d$y)
    return(d$x[pv])
  }
  )
  p = sapply(1:168, function(h) {return(peaks(pvs[[h]]))})
  v = sapply(1:168, function(h) {return(valleys(pvs[[h]]))})
  return (  sapply(1:nrow(ET), 
    function(i, ET.=ET, c.=c,p.=p,v.=v) 
    {
      #print(i)
      pv.i = round(ET[i,4])+1
      return(p.[[pv.i]][1+sum(ET.[i,c.]>v.[[pv.i]])])
    } ))
}

#CONVENTION (note, the dot has no syntactic meaning, it's part of the name)
# m for model, m.t is the predicted factor wrt that temperature
# f for factor
# for classical decomposition (global + seasonal + time series),
# hotw should be additive, maybe estimated from exponential 
# decaying historical data? ie, if we take the non-parametric route.

# dpill from library KernSmooth takes O(n^2)
# if it needs to be scaled up, use optimized kNN instead
m.t = function(temp.new, ET)
{
  xy = locpoly(ET[,2],ET[,3], bandwidth=dpill(ET[,2], ET[,3]))
  return(sapply( temp.new, function(target){ return(xy$y[order(abs(xy$x-target))][1]) }))
  # return y s.t. its corresponding x is the smallest L1 distance to target
}

# predict 1st argument based on 2nd, y on x
predict = function(ET.new, ET)
{
  f.building.type = 1 # vector eventually for buildings
  f.occupancy = rep(0, 365) #frequency * building-specific estimate based on building type
  f.temp =  m.t(ET.new[,2], ET=ET)*f.building.type
  f.hourly = hotw.estimate(ET.new[,1]%%7, ET)
  return( f.temp + f.hourly )
}  
  #data frame with year month day hour# hour format as row name
readable.matrix = function(m, col.date=1) { return(data.frame(m[-col.date], row.names=sapply(m[,col.date], readable.date))) }
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