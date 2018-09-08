item.data <- read.csv(file="k-means clustering data.csv", header=TRUE, sep=",")

library(randomcoloR)
n <- 11
m <- 7
palette1 <- distinctColorPalette(n)

palette2 <- distinctColorPalette(m)

item.data$order.day <- as.factor(as.character(item.data$order.day))

dayLabs<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") 

item.data$order.day <- factor(item.data$order.day, levels= dayLabs)

counts <- table(item.data$item.name, item.data$order.day)

counts2 <- table(item.data$type, item.data$order.day)

print((counts))

par(mar=c(5.1, 4.1, 4.1, 16), xpd=TRUE, yaxt = "n")

plot(item.data$order.day, item.data$item.name, main="LAS Beer Distribution by Day",
        xlab="Days of the Week", col=palette1)


legend("topright",inset=c(-0.75,0), legend=rownames(counts),
      col=palette1, lty = 1, cex=1, lwd = 10)



par(mar=c(5.1, 4.1, 4.1, 16), xpd=TRUE, yaxt = "n")

plot(item.data$order.day, item.data$type, main="LAS Beer Category Distribution by Day",
     xlab="Days of the Week", col=palette2)


legend("topright",inset=c(-0.75,0), legend=rownames(counts2),
       col=palette2, lty = 1, cex=1.5, lwd = 10)



par(mar=c(8.7, 1.1, 4.1, 8), xpd=TRUE, yaxt = "n", las= 2, cex = 1)

plot(item.data$poc.name, item.data$type, main="LAS Beer Category Distribution by POC",
    col=palette2, ylab = " ", xlab = " ")


legend("topright",inset=c(-0.26,0), legend=rownames(counts2),
       col=palette2, lty = 1, cex=.9, lwd = 10)





