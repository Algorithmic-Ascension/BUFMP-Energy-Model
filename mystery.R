library(KernSmooth)

electricity = read.table("~/Documents/Work/FMP/mystery building - 2001.csv", quote="\"", header=TRUE, sep=",", stringsAsFactors=FALSE)
CDD22 = read.table("~/Documents/Work/FMP/KBOS_CDD_-22F.csv", quote="\"", header=TRUE, sep=";", stringsAsFactors=FALSE)


# for(d in 1:nrow(CDD22))
# {
#   CDD22[d,1] = as.Date(CDD22[d,1])
# }

for(d in 1:nrow(electricity))
{
 electricity[d,1] = as.Date(electricity[d,1], "%B %d, %Y")
 electricity[d,98] = as.numeric(gsub(",", "", electricity[d,98]))
}

#CDD = CDD22[electricity[,1] %in% CDD22[,1]][,1:2]
CDD = as.vector(data.matrix(CDD22[246:611,][,2]))
start = as.Date(electricity[1,1], "%B %d, %Y")

electricityDaily = sapply(electricity[,98], as.numeric, USE.NAMES=FALSE)
#fit = lm(CDD~electricityDaily, x=TRUE, y=TRUE)
#plot(fit$x[,2],fit$y)
#lines(fit$x[,2], fit$fitted.values)

m=cbind(CDD-22, electricityDaily)
fitDD = function(m, baseload=65)
{
  CDD_ = m[m[,1]>baseload,][,1]
  electricityDaily_ = m[m[,1]>baseload,][,2]
  return( lm( CDD_ ~ electricityDaily_ ) )
}

ISE = function(bl=65) { return ( sum(fitDD(m, baseload=bl)$residuals^2) ) }

correlation = function(baseload, matrix=m) {return ( cor(matrix[matrix[,1]>baseload,][,1],matrix[matrix[,1]>baseload,][,2]) ) }

#ISERs = vector("numeric", 40)
#for (t in (41:80))
#{
#  ISERs[t-40] = ISER(bl=t)
#}
#plot((41:80), ISERs)

#idea: kernels
plot(m[,1], m[,2], main="Daily Temperature and Electricity Usage", xlab="Temperature", ylab="Demand (KW)")
lines(locpoly(m[,1], m[,2], bandwidth=dpill(m[,1],m[,2])), col="red", lwd=2)
lines(m[,1], lm(m[,2]~m[,1])$fitted.values, col="blue", lwd=2)

plot( (20:80), sapply((20:80), correlation) , type="l", main="Correlation of Temperature Daily", xlab="Base Temperature", ylab="Correlation")

plot(m[m[,1]>41,][,1], m[m[,1]>41,][,2] , main="Daily Temperature and Electricity Usage\n on Days Above 41 degrees", xlab="Temperature", ylab="Demand (KW)")
lines(m[m[,1]>41,][,1], lm(m[m[,1]>41,][,2]~m[m[,1]>41,][,1])$fitted.values, col="blue", lwd=2)
lines(locpoly(m[m[,1]>41,][,1], m[m[,1]>41,][,2], bandwidth=dpill(m[m[,1]>41,][,1],m[m[,1]>41,][,2])), col="red", lwd=2)

temperatures = read.csv("~/Documents/Work/FMP/KBOS.csv", header=TRUE)
temperatures = temperatures[temperatures[,5]==54,]


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

electricity = data.matrix(electricity)
electricity.dates = as.vector(t( matrix(electricity[-366,1], nrow=365, ncol=24) + t(matrix(0:23/24, nrow=24, ncol=365))))
electricity.hourly = as.vector(t(electricity[-366,2:97])) # t(matrix(0:23/24, nrow=24, ncol=96))
electricity.hourly = compressBy4(electricity.hourly)
electricity = cbind(electricity.dates, electricity.hourly)

temperatures = data.matrix(temperatures)
for(d in 1:nrow(temperatures))
{
  temperatures[d,1] = as.Date(paste(c(temperatures[d,3], temperatures[d,1], temperatures[d,2]), collapse="-"))
}
temperatures.dates = temperatures[,1] + (temperatures[,4]/24)
temperatures.temps = temperatures[,6]
temperatures = cbind(temperatures.dates, temperatures.temps)

ET.athletics = c()
for(i in 1:nrow(temperatures))
{
  #print(c( temperatures[i,], electricity[electricity[,1]==temperatures[i,1]] ))
  if(sum(electricity[electricity[,1]==temperatures[i,1]])!=0)
  {
    ET.athletics = rbind(ET.athletics, c(temperatures[i,1], temperatures[i,2], electricity[electricity[,1]==temperatures[i,1]][2], floor(temperatures[i,1]) %% 7))
  }
}
ET.athletics = na.omit(ET.athletics)
plot(ET.athletics[,2], ET.athletics[,3], main="Hourly Temperature and Electricity Usage", xlab="Temperature", ylab="Demand (KW)")
lines(locpoly(ET.athletics[,2], ET.athletics[,3], bandwidth=dpill(ET.athletics[,2],ET.athletics[,3])), col="red", lwd=2)
lines(ET.athletics[,2], lm(ET.athletics[,3]~ET.athletics[,2])$fitted.values, col="blue", lwd=2)

correlation2=function(baseload=41)
{
  return(cor(ET.athletics[ET.athletics[,2]>baseload,][,2], ET.athletics[ET.athletics[,2]>baseload,][,1]))
}
plot( (0:98), sapply((0:98), correlation2) , type="l", main="Correlation of Temperature Hourly", xlab="Base Temperature", ylab="Correlation")
plot(ET.athletics[ET.athletics[,2]>81,][,2], ET.athletics[ET.athletics[,2]>81,][,3], main="Hourly Temperature and Electricity Usage\nAbove 81 Degrees", xlab="Temperature", ylab="Demand (KW)")
lines(locpoly(ET.athletics[ET.athletics[,2]>81,][,2], ET.athletics[ET.athletics[,2]>81,][,3], bandwidth=dpill(ET.athletics[ET.athletics[,2]>81,][,2],ET.athletics[ET.athletics[,2]>81,][,3])), col="red", lwd=2)
lines(ET.athletics[ET.athletics[,2]>81,][,2], lm(ET.athletics[ET.athletics[,2]>81,][,3]~ET.athletics[ET.athletics[,2]>81,][,2])$fitted.values, col="blue", lwd=2)

hist(ET.athletics[ET.athletics[,2]>81,][,1], breaks=100, main="Date-Hours above 81 degrees", xlab="Time (days since 1970-01-01)")

ET.athletics[,4] = floor(ET.athletics[,1] +3) %% 7
plot(0:6, sapply(0:6, function(dotw){ dotws = ET.athletics[ET.athletics[,4]==dotw,3]; return(quantile(dotws, 0.5)) }), ylim=c(25, 70), main="Electricity on Days of the Week", xlab="Day of the Week", ylab="Demand (KW)", type="l")
lines(0:6, sapply(0:6, function(dotw){ dotws = ET.athletics[ET.athletics[,4]==dotw,3]; return(quantile(dotws, 0.25)) }), col="green")
lines(0:6, sapply(0:6, function(dotw){ dotws = ET.athletics[ET.athletics[,4]==dotw,3]; return(quantile(dotws, 0.75)) }), col="green")

ET.athletics[,4] = (ET.athletics[,1]+3) %% 7
plot(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.5)); }), ylim=c(0, 100), main="Electricity on Days of the Week", xlab="Day of the Week", ylab="Demand (KW)", type="l", col="seagreen4", lwd=2)
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(mean(dotws)); }), col="tomato4", lwd=2)
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(mean(dotws) + 0.68*sd(dotws)); }), col="tomato1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(mean(dotws) - 0.68*sd(dotws)); }), col="tomato1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.25)); }), col="seagreen1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.75)); }), col="seagreen1")

#Controlling for temperature, assuming base temperature is 65
plot(ET.athletics[ET.athletics[,2]>65,2], ET.athletics[ET.athletics[,2]>65,3], main="Temperature vs Demand", xlab="Temperature", ylab="Demand")
model.controlled_for_weather = lm(ET.athletics[ET.athletics[,2]>65,3] ~ ET.athletics[ET.athletics[,2]>65,2])
lines(ET.athletics[ET.athletics[,2]>65,2], model.controlled_for_weather$fitted.values, col="red", lwd=3)

#5th column, electricity when controlled for weather
ET.athletics = cbind(ET.athletics, ET.athletics[,3] - (model.controlled_for_weather$coef[2]*ET.athletics[,2] + model.controlled_for_weather$coef[1]))
ET.athletics[ET.athletics[,2]<65,5] = ET.athletics[ET.athletics[,2]<65,3]

#something else is going on, also cyclical
#ignore
#plot(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3)& ET.athletics[,2]>65,5]; return(quantile(dotws, 0.5)); }) / sapply(0:23/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.5)); }) , main="Electricity on Days of the Week", xlab="Day of the Week", ylab="Demand (KW)", type="l", col="seagreen4", lwd=2)
#lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3)& ET.athletics[,2]>65,5]; return(quantile(dotws, 0.25)); }) / sapply(0:23/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.25)); }), col="seagreen1")
#lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3)& ET.athletics[,2]>65,5]; return(quantile(dotws, 0.25)); }) / sapply(0:23/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.75)); }), col="seagreen1")

plot(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.5)); }) / rep(sapply(0:6, function(dotw){ dotws = ET.athletics[floor(ET.athletics[,4])==dotw,3]; return(quantile(dotws, 0.5)); }), each=24), main="Median Hourly / Daily Characteristic", xlab="Day of the Week", ylab="Demand (KW)", type="l", col="seagreen4", lwd=2, ylim=c(0,2))
lines( 0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.25)); }) / rep(sapply(0:6, function(dotw){ dotws = ET.athletics[floor(ET.athletics[,4])==dotw,3]; return(quantile(dotws, 0.5)); }), each=24), col="seagreen1")
lines( 0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),3]; return(quantile(dotws, 0.75)); }) / rep(sapply(0:6, function(dotw){ dotws = ET.athletics[floor(ET.athletics[,4])==dotw,3]; return(quantile(dotws, 0.5)); }), each=24), col="seagreen1")

plot(ET.athletics[,2], ET.athletics[,5], main="Energy Unaccounted for (Residuals)", xlab="Temperature", ylab="Demand (KW)")
plot(ET.athletics[,1], ET.athletics[,5], main="Energy Unaccounted for (Residuals)", xlab="Day", ylab="Demand (KW)")

plot(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.5)); }), main="Median Hourly when controlled for temperature", xlab="Day of the Week", ylab="Demand (KW)", type="l", col="seagreen4", lwd=2)
ET.athletics[,4] = round(ET.athletics[,4], 3)
dotw.normalized = cbind( round(0:167/24, 3), sapply(0:167/24, function(dotw){ dotws = ET.athletics[round(ET.athletics[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.5)); }))
plot(ET.athletics[,1], ET.athletics[,5], main="Residuals when controlled for linear temperature")
plot(ET.athletics[,1], ET.athletics[,5] / sapply(1:nrow(ET.athletics), function(i) { return(dotw.normalized[ET.athletics[i,4]==dotw.normalized[,1],2]) } ) , main="Factor over time", xlab="Time (Days since 1970-01-01)", ylab="Factor")

llfit = locpoly(ET.athletics[,2], ET.athletics[,3], bandwidth=dpill(ET.athletics[,2],ET.athletics[,3]))
#1 nearest neighbor to a 401 approximation
plot(ET.athletics[,1] , (ET.athletics[,3] - sapply(1:nrow(ET.athletics), function(i){ return( llfit$y[which.min(abs(ET.athletics[i,2] - llfit$x ))] )} ) ) / sapply(1:nrow(ET.athletics), function(i) { return(dotw.normalized[ET.athletics[i,4]==dotw.normalized[,1],2]) } ))

########################################


temperatures = read.csv("~/Documents/Work/FMP/KBOS 2012.csv", header=TRUE)
temperatures = data.matrix(temperatures[temperatures[,5]==54,])[,-6]
for(d in 1:nrow(temperatures))
{
  temperatures[d,1] = as.Date(paste(c(temperatures[d,3], temperatures[d,1], temperatures[d,2]), collapse="-"))
}
temperatures.dates = temperatures[,1] + round(temperatures[,4]/24, 3)
temperatures.temps = temperatures[,6]
temperatures = na.omit(cbind(temperatures.dates, temperatures.temps))

electricity2 = read.table("~/Documents/Work/FMP/mystery building - 2012.csv", quote="\"", header=TRUE, sep=",", stringsAsFactors=FALSE)
for(d in 1:nrow(electricity2))
{
  electricity2[d,1] = as.Date(electricity2[d,1], "%B %d, %Y")
  electricity2[d,98] = as.numeric(gsub(",", "", electricity2[d,98]))
}
electricity2 = data.matrix(electricity2)
electricity2.dates = round(as.vector(t( matrix(electricity2[-366,1], nrow=365, ncol=24) + t(matrix(0:23/24, nrow=24, ncol=365)))), 3)
electricity2.hourly = compressBy4(as.vector(t(electricity2[-366,2:97]))) # t(matrix(0:23/24, nrow=24, ncol=96))
electricity2 = cbind(electricity2.dates, electricity2.hourly)
electricity2 = na.omit(electricity2)

ET.classroom=c()
for(i in 1:nrow(temperatures))
{
  if(sum(electricity2[electricity2[,1]==temperatures[i,1]])!=0)
  {
    ET.classroom = rbind(ET.classroom, c(temperatures[i,1], temperatures[i,2], electricity2[electricity2[,1]==temperatures[i,1]][2], floor(temperatures[i,1]) %% 7))
  }
}

plot(ET.classroom[,2], ET.classroom[,3])
ET.classroom = ET.classroom[ET.classroom[,3]!=0,]
plot(ET.classroom[,2], ET.classroom[,3])
lines(ET.classroom[,2], lm(ET.classroom[,3] ~ ET.classroom[,2])$fitted.values, col="red", lwd=3)
lines(locpoly(ET.classroom[,2], ET.classroom[,3], bandwidth=dpill(ET.classroom[,2],ET.classroom[,3])), col="blue", lwd=3)

model.w.classroom = lm(ET.classroom[ET.classroom[,2]>65,3] ~ ET.classroom[ET.classroom[,2]>65,2])
plot(ET.classroom[ET.classroom[,2]>65,2], ET.classroom[ET.classroom[,2]>65,3], main="Temperature vs Demand", xlab="Temperature", ylab="Demand")
lines(ET.classroom[ET.classroom[,2]>65,2], model.w.classroom$fitted.values, col="red", lwd=3)

ET.classroom[,4] = round((ET.classroom[,1]+3) %% 7, 3)
ET.classroom = cbind( ET.classroom, ET.classroom[,3] - (model.w.classroom$coef[2]*ET.classroom[,2] + model.w.classroom$coef[1]) )
ET.classroom[ET.classroom[,2]<65,5] = ET.classroom[ET.classroom[,2]<65,3]

plot(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.5)); }), ylim=c(0, 300), main="Electricity on Hours of the Week", xlab="Hours of the Week", ylab="Demand (KW)", type="l", col="seagreen4", lwd=2)
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(mean(dotws)); }), col="tomato4", lwd=2)
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(mean(dotws) + 0.68*sd(dotws)); }), col="tomato1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(mean(dotws) - 0.68*sd(dotws)); }), col="tomato1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.25)); }), col="seagreen1")
lines(0:167/24, sapply(0:167/24, function(dotw){ dotws = ET.classroom[round(ET.classroom[,4], 3)==round(dotw, 3),5]; return(quantile(dotws, 0.75)); }), col="seagreen1")

#distribution estimated by its median
hotw.median.classroom = cbind(round(0:167/24, 3), sapply(0:167/24, function(hotw){ hotws = ET.classroom[ET.classroom[,4]==round(hotw, 3),5]; return(quantile(hotws, 0.5)); }))
plot(ET.classroom[,1], ET.classroom[,5] / sapply(1:nrow(ET.classroom), function(i) { return(hotw.median.classroom[ET.classroom[i,4]==hotw.median.classroom[,1],2]) } ) , main="Factor over time", xlab="Time (Days since 1970-01-01)", ylab="Factor")
#6th column
ET.classroom = cbind( ET.classroom, ET.classroom[,5] / sapply(1:nrow(ET.classroom), function(i) { return(hotw.median.classroom[ET.classroom[i,4]==hotw.median.classroom[,1],2]) } ) )


#hierarchial clustering wiih sqrt distances
dotw.slice = function(m, i) {return( relevant.days = matrix(m[m[,4]==i,6], ncol=1, dimnames=list(m[m[,4]==i,1], "factor")) )}
dotw.show = function(m,i) {plot(density(dotw.slice(m,i)))}

#matrix(ET.classroom[round(ET.classroom[,4], 3)==round(64/24, 3),6], ncol=1, dimnames=list(ET.classroom[round(ET.classroom[,4], 3)==round(64/24, 3),1], "factor"))

#d = density(matrix(ET.classroom[round(ET.classroom[,4], 3)==round(64/24, 3),6], ncol=1, dimnames=list(ET.classroom[round(ET.classroom[,4], 3)==round(64/24, 3),1], "factor")), bw="SJ")$y
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
    results[i] = sum(m[i,c]>d$y[valleys])+1
  }
  return(d$x[peaks[results]])
}