range01 <- function(x)(x-min(x))/diff(range(x))
cRamp <- function(x){
  cols <- colorRamp(jet.colors(length(x)))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  

#Plot x vs y, colored by z
#plot(x,y,col=cRamp(z), main="Mine")

#THis code was adapted from someone using rainbow() as the colormap palette, so it needs to be adjusted
