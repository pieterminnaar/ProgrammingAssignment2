library(sp)
library(rgeos)

# Make up some 'banana shaped' points
mypts=cbind(runif(100),runif(100))
keep=4*(mypts[,1]-0.5)**2 + 0.2 > mypts[,2]
mypts=mypts[keep,]

# Coerce to SpatialPointsDataframe
mypts2=SpatialPointsDataFrame(mypts,data=data.frame(NA*mypts[,1]),
match.ID=F)

# Now take a buffer that covers up all the points
# You will need to decide on an appropriate 'width' 
# argument so that the region is connected
buf1=gBuffer(mypts2, width=0.3,byid=T)
buf1_union=gUnionCascaded(buf1) 
# Take the union -- so this polygon will
## contain all your points
# Now partly 'undo' the buffer -- again, 
## experimentation is needed to
## choose the width
buf_final=gBuffer(buf1_union,width=-0.2)

# This should look okay
plot(buf_final)
points(mypts,col=2)