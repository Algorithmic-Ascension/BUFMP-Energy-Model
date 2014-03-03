#final model 
temps = read.csv("Documents/Work/FMP/KMACAMBR9.csv", stringsAsFactors=FALSE)
for(d in 1:nrow(temps)){
  temps[d,1] = as.Date(temps[d,1])
}
temps = data.matrix(temps)
temps = cbind(temps[,1] + round(temps[,2]/24,3), temps[,3])


#copied wholesale from the other code
compressBy4 = function(x)
{
  result = vector("numeric", length(x)/4)
  for( i in 0 : (length(x)/4 - 1) )
  {
    numbers.of.interest = x[(i*4+1):(i*4+4)]
    result[i+1] = sum(numbers.of.interest) / (4-sum(is.na(numbers.of.interest)))
  }
  return(result)
}

E = read.table("~/Documents/Work/FMP/mystery building - 2012.csv", quote="\"", header=TRUE, sep=",", stringsAsFactors=FALSE)
for(d in 1:nrow(E))
{
  E[d,1] = as.Date(E[d,1], "%B %d, %Y")
  E[d,98] = as.numeric(gsub(",", "", E[d,98]))
}
E = data.matrix(E)
E.dates = as.vector(t( matrix(E[-366,1], nrow=365, ncol=24) + t(matrix(round(0:23/24, 3), nrow=24, ncol=365))))
E.hourly = compressBy4(as.vector(t(E[-366,2:97]))) # t(matrix(0:23/24, nrow=24, ncol=96))
E = cbind(E.dates, E.hourly)
E = na.omit(E)

ET = c()
for(i in 1:nrow(data))
{
  if(sum(E[E[,1]==data[i,1]])!=0)
  {
    ET = rbind(ET, c(temps[i,1], temps[i,2], E[E[,1]==temps[i,1]][2], temps[i,1] %% 7))
  }
}


m.t = lm(ET[,3] ~ ET[,2])$coef
f.building.type = 1 # vector eventually for buildings
f.occupancy =  #frequency * hotw estimate * building-specific estimate based on building type
f.temp =  m.t[1]*f.building.type*() #building type * degrees away from baseload as determined by linear fit

plot(e.time, (e.total - f.occuancy - f.temp)/hotw)