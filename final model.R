#final model 
# if you want relative paths, open up R interpreter
# and use with source( 'final model.R', chdir=T )
# wonky I know, such is life

library(KernSmooth)
#library(cvTools)
#TODO use crossvalidation to evaluate the model

E.SMG = as.vector(t(read.csv("~/Documents/Work/BUFMP-Energy-Model/Electricity.SMG.2010.csv", header=F, na.strings="No Record")))
E.SMG = append(ET.SMG, as.vector(t(read.csv("~/Documents/Work/BUFMP-Energy-Model/Electricity.SMG.2011.csv", header=F, na.strings="No Record"))))
E.SMG = append(ET.SMG, as.vector(t(read.csv("~/Documents/Work/BUFMP-Energy-Model/Electricity.SMG.2012.csv", header=F, na.strings="No Record"))))
E.SMG = append(ET.SMG, as.vector(t(read.csv("~/Documents/Work/BUFMP-Energy-Model/Electricity.SMG.2013.csv", header=F, na.strings="No Record"))))
#ET.SMG = append(ET.SMG, as.vector(t(read.csv("~/Documents/Work/BUFMP-Energy-Model/Electricity.SMG.2014.csv", header=F, na.strings="No Record"))))
ET.SMG.dates = as.vector(t( matrix(as.numeric(as.Date("2010-01-01")):as.numeric(as.Date("2012-12-31")), nrow=730, ncol=24) + t(matrix(0:23/24, nrow=24, ncol=730))))

#works, but not fast enough
#lastDuplicates = function(m)
#{ # last copy of duplicates (most minutes are 54)
#  v = sapply(1:nrow(m), function(i) {return(paste(m[i,1], m[,2], m[i,3], m[i,4]))} )
#  return (m[rev(!duplicated(rev(v)))])
#}


# use $y of the density function as input
peaks.valleys = function(d)
{
  d. = d[-1]-d[-length(d)] # first derivative
  return(which( d.[-1]*d.[-length(d.)] <0)+1)  #locations of turning points, derivative is 0, peaks and valleys
}# there will always be an odd number of them because its a pdf

#parses out peaks and valleys separately from pv
peaks = function(pv) { return(pv[seq(1,length(pv), 2)]) }
valleys = function(pv) { return(pv[seq(0,length(pv), 2)]) }

train = function(ET.new, ET, pvs, c=2)
{ # return the nth peak where n is 1+the number of valleys it's greater than
#   pvs = sapply( round(0:167/24, 3), function(hotw, ET. = ET)
#   {
#     d = density(ET.[round(ET.[,1]%%7,3)==round(hotw,3), 3], bw="SJ")
#     pv = peaks.valleys(d$y)
#     return(d$x[pv])
#   })
   p = sapply(1:168, function(h) {return(peaks(pvs[[h]]))})
   v = sapply(1:168, function(h) {return(valleys(pvs[[h]]))})
  
  #training = sapply( 0:floor(nrow(ET)/8760),
  #  function(j){
  #    this.year = j*365*24 + 1
  #    next.year = (j+1)*365*24
  #    return(
  #      sapply(this.year:min(nrow(ET), next.year), 
    training = sapply(1:nrow(ET),
        function(i, ET.=ET, c.=c,p.=p,v.=v)
        {
          pv.i = round(round(ET.[i,1]%%7, 3)*24) + 1
          #print(pv.i)
          return(p.[[pv.i]][1+sum(ET.[i,c.]>v.[[pv.i]])])
        } )
  #  )}
      
  #)
  return(training)
  #return the mode of each of hoty
  #return (
    #unlist(
  #    sapply(training, function(x) { 
  #      return(as.numeric(names(sort(-table(x)))[1]))
  #    } )
    #)
  #)
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
  f.occupancy.new = rep(0, nrow(ET.new))
  f.building.type = 1 # vector eventually for buildings
  f.occupancy = rep(0, nrow(ET.new)) #frequency * building-specific estimate based on building type
  f.temp =  m.t(ET.new[,2], ET=ET)*f.building.type
  resid.ET = cbind(ET[,1:2], ET[,3] - f.temp - f.occupancy )
  resid.ET.new = cbind(ET.new[,1:2], ET.new[,3] - m.t(ET.new[,2], ET=ET)*f.building.type - f.occupancy.new )

  #TODO integrate into hotw.estimate
  pvs = sapply( round(0:167/24, 3), function(hotw, ET. = ET)
  {
    d = density(ET.[round(ET.[,1]%%7,3)==round(hotw,3), 3], bw="SJ")
    pv = peaks.valleys(d$y)
    return(d$x[pv])
  })
  
  f.hourly = hotw.estimate(resid.ET.new, resid.ET)
  #print(f.hourly)
  return( f.temp + f.hourly + f.occupancy )
}

#data frame with year month day hour# hour format as row name
readable.matrix = function(m, col.date=1) { return(data.frame(m[,-col.date], row.names=sapply(m[,col.date], readable.date))) }
readable.date = function(d) { return(paste(format(as.Date(d,origin="1970-01-01"), "%Y %B %d"), "hour#", round(24*(d - floor(d))))) }