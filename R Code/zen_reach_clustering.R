zendata <- read.csv(file="export-14.csv", header=TRUE, sep=",")

zendata <- zendata[!(is.na(zendata$Last.Visit) | zendata$Last.Visit==""), ]

#zendata$Tags <- as.character(zendata$Tags)


#zendata <- subset(zendata, Tags == "mexico" | Tags == "argentina" | Tags == "colombia" | Tags == "australia")

cluster <- zendata[c(2,3)]

cluster$Frequency <- as.numeric(as.character(cluster$Frequency))
cluster$Recency <- as.numeric(as.character(cluster$Recency))

cluster <- cluster[cluster$Frequency > 0 & cluster$Frequency < 100,]

#na.omit(cluster)

print(nrow(cluster))

smoothingSpline = smooth.spline(dfNorm$Recency, dfNorm$Frequency, spar=0.35)
plot(dfNorm$Recency, dfNorm$Frequency)
lines(smoothingSpline)


#elbow function

normalize <- function(x) {
  return (((x - min(x)) / (max(x) - min(x))) * 5)}

dfNorm <- as.data.frame(lapply(cluster, normalize))


set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(dfNorm, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




#clustering

k2 <- kmeans(dfNorm, centers = 4, nstart = 25)
k3 <- kmeans(dfNorm, centers = 5, nstart = 25)
k4 <- kmeans(dfNorm, centers = 6, nstart = 25)
k5 <- kmeans(dfNorm, centers = 7, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = dfNorm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = dfNorm) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = dfNorm) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = dfNorm) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


mydataCluster <- kmeans(dfNorm, 7, nstart = 20)
mydataCluster$cluster <- as.factor(mydataCluster$cluster)
library (ggplot2)
#dfNorm <- dfNorm[dfNorm$Frequency > .25,]

my.loess <- loess(Recency~Frequency, data=dfNorm)
# get SE
myPred <- predict(my.loess, se=T)
my.output <- data.frame("fitted"=myPred$fit, "SE"=myPred$se.fit)

# write out data
write.csv(my.output, file=curve.csv)



ggplot(dfNorm, aes(Recency, Frequency)) + geom_point() +  scale_x_reverse( lim=c(5,0)) +
  stat_smooth(method = "loess") 
#+ coord_fixed(xlim=c(.5,.25), ylim = c(.45, .65)) 

smooth_vals = predict(loess(Recency~Frequency,dfNorm))



#ggplot(dfNorm, aes(Recency, Frequency)) + geom_point() +  scale_x_reverse( lim=c(5,0))



dfNorm <- cbind(dfNorm, location = zendata$frequency)
#dfNorm <- cbind(dfNorm, )




clus1 <- dfNorm[dfNorm$cluster == 1, ]
plot(clus1$location)

clus2 <- dfNorm[dfNorm$cluster == 2, ]
plot(clus2$location)

clus3 <- dfNorm[dfNorm$cluster == 3, ]
plot(clus3$location)

clus4 <- dfNorm[dfNorm$cluster == 4, ]
plot(clus4$location)

clus5 <- dfNorm[dfNorm$cluster == 5, ]
plot(clus5$location)

clus6 <- dfNorm[dfNorm$cluster == 6, ]
plot(clus6$location)

clus7 <- dfNorm[dfNorm$cluster == 7, ]
plot(clus5$location)


