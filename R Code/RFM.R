install.packages("purrr")
library(purrr)
library(factoextra)
library(plyr)

#RFM <- read.csv(file="RFM R Analysis.csv", header=FALSE, sep=",", stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
RFM <- read.csv(file="RFM R Analysis.csv", header=TRUE, sep=",")

na.omit(RFM)

RFM <- RFM[- grep("#N/A", RFM$Gender),]

RFM <- RFM[- grep("#VALUE!", RFM$Age),]

RFM <- RFM[- grep("O", RFM$Gender),]


RFM$Age <- as.numeric(as.character(RFM$Age))
RFM$Gender <- as.numeric(as.character(RFM$Gender))
RFM$Ze0reach <- as.numeric(as.character(RFM$Ze0reach))
RFM$Combined.RFM.Score <- as.numeric(as.character(RFM$Combined.RFM.Score))
RFM$Participant <- as.numeric(as.character(RFM$Participant))
RFM$Influencer <- as.numeric(as.character(RFM$Influencer))


RFM <- RFM[RFM$Average.Days > 0,]

items <- RFM[c(10, 33:48)]




###############

fit <- lm(Combined.RFM.Score ~ Age + Gender + Ze0reach + Participant + Influencer, data = RFM)
summary(fit)

###############

fit <- lm(Combined.RFM.Score ~ Average.Days, data = RFM)
summary(fit)

###############

fit <- lm(Combined.RFM.Score ~ ., data = items)
summary(fit)

###############


#nrow(RFM[RFM$Frequency > 1,])


RFM$Customer.Segment <- factor(RFM$Customer.Segment, levels = c("About To Sleep", "At Risk", "Customers Needing Attention", "Potential Loyalist", "Loyal Customers", "Champions"))

print(levels(RFM$Customer.Segment))

segments <- table(RFM$Customer.Segment)

print(segments)





#segments <- as.data.frame(segments)

#segments <- segments[c(1, 2, 4, 6, 5, 3), ]

#segments <- table(segments)

#print(segments)

#print(segments)

xlabels <- expression("About To Sleep - M - 26.8", "At Risk - M - 25.7", "Customers Needing Attention - M - 29.8", "Potential Loyalist - M - 25.5", "Loyal Customers - M - 25.5", "Champions - M - 27.7")

barplot(segments, main = "RFM Segmentation", xlab = "Segments", ylab = "# of Customers", col = "pink", ylim = c(0,400), names.arg = xlabels)





abtsleep <- RFM[RFM$Customer.Segment == 'About To Sleep',]

abtsleepage <- mean(as.numeric(as.character(abtsleep$Age)))

abtgendavg <- mean(as.numeric(as.character(abtsleep$Gender)))

abtmorf <- ifelse(abtgendavg >= .5, "M", "F")

abtsleepfinal <- setNames( c("About To Sleep",abtmorf, abtsleepage), c("Category", "Gender", "Age"))

print(abtsleepfinal)


arisk <- RFM[RFM$Customer.Segment == 'At Risk',]

ariskage <- mean(as.numeric(as.character(arisk$Age)))

ariskgendavg <- mean(as.numeric(as.character(arisk$Gender)))

ariskmorf <- ifelse(ariskgendavg >= .5, "M", "F")

ariskfinal <- setNames( c("At Risk", ariskmorf, ariskage), c("Category", "Gender", "Age"))

print(ariskfinal)


champ <- RFM[RFM$Customer.Segment == 'Champions',]

champage <- mean(as.numeric(as.character(champ$Age)))

champgendavg <- mean(as.numeric(as.character(champ$Gender)))

champmorf <- ifelse(champgendavg >= .5, "M", "F")

champfinal <- setNames( c("Champions", champmorf, champage), c("Category", "Gender", "Age"))

print(champfinal)



attention <- RFM[RFM$Customer.Segment == 'Customers Needing Attention',]

attenage <- mean(as.numeric(as.character(attention$Age)))

attengendavg <- mean(as.numeric(as.character(attention$Gender)))

attenmorf <- ifelse(attengendavg >= .5, "M", "F")

attenfinal <- setNames( c("Customers Needing Attention", attenmorf, attenage), c("Category","Gender", "Age"))

print(attenfinal)


loyal <- RFM[RFM$Customer.Segment == 'Loyal Customers',]

loyalage <- mean(as.numeric(as.character(loyal$Age)))

loyalgendavg <- mean(as.numeric(as.character(loyal$Gender)))

loyalmorf <- ifelse(loyalgendavg >= .5, "M", "F")

loyalfinal <- setNames( c("Loyal Customers", loyalmorf, loyalage), c("Category", "Gender", "Age"))

print(loyalfinal)


potenloyal <- RFM[RFM$Customer.Segment == 'Potential Loyalist',]

potenloyalage <- mean(as.numeric(as.character(potenloyal$Age)))

potenloyalgendavg <- mean(as.numeric(as.character(potenloyal$Gender)))

potenloyalmorf <- ifelse(potenloyalgendavg >= .5, "M", "F")

potenloyalfinal <- setNames( c("Potential Loyalist",potenloyalmorf, potenloyalage), c("Category","Gender", "Age"))

print(potenloyalfinal)


print(abtsleepfinal)
print(ariskfinal)
print(champfinal)
print(attenfinal)
print(loyalfinal)
print(potenloyalfinal)






femaletable <- RFM[RFM$Gender == '0',]

women <- table(femaletable$Customer.Segment)

barplot(women, main = "RFM Segmentation", xlab = "Segments", ylab = "# of Customers", col = "orange", ylim = c(0,400))







freq<- table(RFM$Frequency)

barplot(freq, main = "Frequency", xlab = "Frequency", ylab = "# of Customers", col = "blue", ylim = c(0, 700))

gend<- table(RFM$Gender)

gend <- as.data.frame(gend)

gend <- gend[-1, ]

barplot(gend$Freq, names = gend$Var1, main = "Gender", xlab = "Gender", ylab = "# Male/Female", col = "green", ylim = c(0, 800))

age <- table(RFM$Age)

age <- as.data.frame(age)

age <- age[-1, ]

barplot(age$Freq, names = age$Var1, main = "Age", xlab = "Age", ylab = "# Customers", col = "yellow", ylim = c(0, 120))

zen <- table(RFM$Zenreach)

barplot(zen, main = "Zenreach Percentage", xlab = "Zenreach", ylab = "# of Customers", col = "purple")

#plot(RFM$Age, RFM$Combined.RFM.Score, xlim = c(5, 40))

cor(as.numeric(as.character(RFM$Age)), as.numeric(as.character(RFM$Combined.RFM.Score)))

cor(as.numeric(as.character(RFM$Gender)), as.numeric(as.character(RFM$Combined.RFM.Score)))

cor(as.numeric(as.character(RFM$Ze0reach)), as.numeric(as.character(RFM$Combined.RFM.Score)))

cor(as.numeric(as.character(RFM$Participant)), as.numeric(as.character(RFM$Combined.RFM.Score)))

cor(as.numeric(as.character(RFM$Influencer)), as.numeric(as.character(RFM$Combined.RFM.Score)))






count <- c(sum(RFM$Participant), sum(RFM$Influencer))

barplot(count, names.arg = c("Participant","Influencer"), ylab = "# of Customers", ylim = c(0,200), main = "Facebook Participants and Influencers", col = "green")

RFMK <- RFM[c(2:3, 10, 12, 30:31)]
RFMK <- na.omit(RFMK[-1, ])
RFMK <- RFMK[- grep("#N/A", RFMK$V2),]

RFMK <- RFMK[- grep("#VALUE!", RFMK$V3),]

sum(sapply(RFMK, is.na))

sum(sapply(RFMK, is.infinite))

sum(sapply(RFMK, is.nan))


RFMK$V2 <- as.numeric(as.character(RFMK$V2))
RFMK$V3 <- as.numeric(as.character(RFMK$V3))
RFMK$V10 <- as.numeric(as.character(RFMK$V10))
RFMK$V12 <- as.numeric(as.character(RFMK$V12))
RFMK$V30 <- as.numeric(as.character(RFMK$V30))
RFMK$V31 <- as.numeric(as.character(RFMK$V3))

RFMK<-na.omit(RFMK)



str(RFMK)



fviz_nbclust(RFMK, kmeans, method = "silhouette")



k2 <- kmeans(RFMK, centers = 2, nstart = 25)
k3 <- kmeans(RFMK, centers = 3, nstart = 25)
k4 <- kmeans(RFMK, centers = 4, nstart = 25)
k5 <- kmeans(RFMK, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = RFMK) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = RFMK) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = RFMK) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = RFMK) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


(cl <- kmeans(RFMK, 5, nstart = 25))
plot(RFMK, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(RFMK, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



__________________________________________________________________

RFMN <- RFM[c(4:6)]
RFMN <- na.omit(RFMN[-1, ])


RFMN$V4 <- as.numeric(as.character(RFMN$V4))
RFMN$V5 <- as.numeric(as.character(RFMN$V5))
RFMN$V6 <- as.numeric(as.character(RFMN$V6))


RFMN<-na.omit(RFMN)

names(RFMN) <- c("R", "F", "M")

#elbow function

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(RFMN, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#silhouette method

fviz_nbclust(RFMN, kmeans, method = "silhouette")


#visually see clusters

k2 <- kmeans(RFMN, centers = 2, nstart = 25)
k3 <- kmeans(RFMN, centers = 3, nstart = 25)
k4 <- kmeans(RFMN, centers = 4, nstart = 25)
k5 <- kmeans(RFMN, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = RFMN) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = RFMN) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = RFMN) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = RFMN) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#plotting matrix

library (ggplot2)
ggplot(RFMN, aes(R, M, color = k3$cluster)) + geom_point()

(cl <- kmeans(RFMN, 3, nstart = 25))
plot(RFMN, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


RFMChamps <- RFM[RFM$V14 == 'Champions',]

comeback <- RFM[RFM$Participant == 1,]

com <- table(comeback$Frequency)

barplot(com, ylim = c(0, 130))


_________________________________________

#Customer Lifetime Value for Each Segment

sleepdata <- RFM[RFM$Customer.Segment == 'About To Sleep',]
riskdata <- RFM[RFM$Customer.Segment == 'At Risk',]
championsdata <- RFM[RFM$Customer.Segment == 'Champions',]
attentiondata <- RFM[RFM$Customer.Segment == 'Customers Needing Attention',]
loyaldata <- RFM[RFM$Customer.Segment == 'Loyal Customers',]
potentialloyaldata <- RFM[RFM$Customer.Segment == 'Potential Loyalist',]

onedata <- RFM[RFM$Frequency == 1,]

mean(sleepdata$Frequency)
mean(riskdata$Frequency)
mean(championsdata$Frequency)
mean(attentiondata$Frequency)
mean(loyaldata$Frequency)
mean(potentialloyaldata$Frequency)



oneavgorder <- sum(onedata$Monetary)/sum(onedata$Frequency)
onepurchfreq <- sum(onedata$Frequency)/nrow(onedata)
onecustomerlifetimevalue <- oneavgorder*onepurchfreq*3.87

mean(onedata$Frequency)
mean(onedata$Monetary)
mean(loyaldata$Frequency)
mean(loyaldata$Monetary)
mean(championsdata$Frequency)
mean(championsdata$Monetary)



print(onecustomerlifetimevalue)
print(loyalcustomerlifetimevalue)
print(championscustomerlifetimevalue)
print(nrow(loyaldata))
print(nrow(championsdata))


sleepavgorder <- sum(sleepdata$Monetary)/sum(sleepdata$Frequency)
sleeppurchfreq <- sum(sleepdata$Frequency)/nrow(sleepdata)
sleepcustomerlifetimevalue <- sleepavgorder*sleeppurchfreq*3.87

riskavgorder <- sum(riskdata$Monetary)/sum(riskdata$Frequency)
riskpurchfreq <- sum(riskdata$Frequency)/nrow(riskdata)
riskcustomerlifetimevalue <- riskavgorder*riskpurchfreq*3.87

championsavgorder <- sum(championsdata$Monetary)/sum(championsdata$Frequency)
championspurchfreq <- sum(championsdata$Frequency)/nrow(championsdata)
championscustomerlifetimevalue <- championsavgorder*championspurchfreq*3.87

attentionavgorder <- sum(attentiondata$Monetary)/sum(attentiondata$Frequency)
attentionpurchfreq <- sum(attentiondata$Frequency)/nrow(attentiondata)
attentioncustomerlifetimevalue <- attentionavgorder*attentionpurchfreq*3.87

loyalavgorder <- sum(loyaldata$Monetary)/sum(loyaldata$Frequency)
loyalpurchfreq <- sum(loyaldata$Frequency)/nrow(loyaldata)
loyalcustomerlifetimevalue <- loyalavgorder*loyalpurchfreq*3.87


potentialloyalavgorder <- sum(potentialloyaldata$Monetary)/sum(potentialloyaldata$Frequency)
potentialloyalpurchfreq <- sum(potentialloyaldata$Frequency)/nrow(potentialloyaldata)
potentialloyalcustomerlifetimevalue <- potentialloyalavgorder*potentialloyalpurchfreq*3.87

xlabel <- expression("About To Sleep - M - 26.8", "At Risk - M - 25.7", "Customers Needing Attention - M - 29.8", "Potential Loyalist - M - 25.5", "Loyal Customers - M - 25.5", "Champions - M - 27.7")
nums <- c(sleepcustomerlifetimevalue, riskcustomerlifetimevalue, attentioncustomerlifetimevalue, potentialloyalcustomerlifetimevalue,loyalcustomerlifetimevalue, championscustomerlifetimevalue)

plot(nums, type = "l")

barplot(nums, names.arg = xlabel, col="orange", main = "Customer Lifetime Value", ylim = c(0, 800), xlab = "Customers", ylab = "Kilometers")

ltvnums <- c(0, 172, 281, 353, 745)

plot(ltvnums, type="l")
