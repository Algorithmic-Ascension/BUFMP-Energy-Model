kma = read.csv("Documents/Work/FMP/KMACAMBR9.csv", stringsAsFactors=FALSE)
for(d in 1:nrow(kma)){
kma[d,1] = as.Date(kma[d,1])
}
kma = data.matrix(kma)
kma = cbind(kma[,1] + signif(kma[,2]/24,3), kma[,3])

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

electricity2 = read.table("~/Documents/Work/FMP/mystery building - 2012.csv", quote="\"", header=TRUE, sep=",", stringsAsFactors=FALSE)
for(d in 1:nrow(electricity2))
{
  electricity2[d,1] = as.Date(electricity2[d,1], "%B %d, %Y")
  electricity2[d,98] = as.numeric(gsub(",", "", electricity2[d,98]))
}
electricity2 = data.matrix(electricity2)
electricity2.dates = as.vector(t( matrix(electricity2[-366,1], nrow=365, ncol=24) + t(matrix(signif(0:23/24, 3), nrow=24, ncol=365))))
electricity2.hourly = compressBy4(as.vector(t(electricity2[-366,2:97]))) # t(matrix(0:23/24, nrow=24, ncol=96))
electricity2 = cbind(electricity2.dates, electricity2.hourly)
electricity2 = na.omit(electricity2)

ET.classroom = c()
for(i in 1:nrow(kma))
{
  if(sum(electricity2[electricity2[,1]==kma[i,1]])!=0)
  {
    ET.classroom = rbind(ET.classroom, c(kma[i,1], kma[i,2], electricity2[electricity2[,1]==kma[i,1]][2], kma[i,1] %% 7))
  }
}