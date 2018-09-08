RFM <- read.csv(file="RFM R Analysis.csv", header=TRUE, sep=",")

na.omit(RFM)

RFM <- RFM[- grep("#N/A", RFM$Gender),]

RFM <- RFM[- grep("#VALUE!", RFM$Age),]

RFM <- RFM[- grep("O", RFM$Gender),]

RFM$Frequency <- as.numeric(as.character(RFM$Frequency))
RFM$Monetary <- as.numeric(as.character(RFM$Monetary))
RFM$Age <- as.numeric(as.character(RFM$Age))
RFM$Gender <- as.numeric(as.character(RFM$Gender))


onedata <- RFM[RFM$Frequency == 1,]
onedatafreq <- mean(onedata$Frequency)
onedataage <- mean(onedata$Age)
onedatagend <- mean(onedata$Gender)
oneavgorder <- sum(onedata$Monetary)/sum(onedata$Frequency)
onepurchfreq <- sum(onedata$Frequency)/nrow(onedata)
onecustomerlifetimevalue <- oneavgorder*onepurchfreq*3.87
print(onedataage)


twodata <- RFM[RFM$Frequency == 2,]
twodatafreq <- mean(twodata$Frequency)
twodataage <- mean(twodata$Age)
twodatagend <- mean(twodata$Gender)
twoavgorder <- sum(twodata$Monetary)/sum(twodata$Frequency)
twopurchfreq <- sum(twodata$Frequency)/nrow(twodata)
twocustomerlifetimevalue <- twoavgorder*twopurchfreq*3.87
print(twodataage)



loyaldata <- RFM[RFM$Frequency > 2 & RFM$Frequency <= 4,]
loyaldatafreq <- mean(loyaldata$Frequency)
loyaldataage <- mean(loyaldata$Age)
loyaldatagend <- mean(loyaldata$Gender)
loyalavgorder <- sum(loyaldata$Monetary)/sum(loyaldata$Frequency)
loyalpurchfreq <- sum(loyaldata$Frequency)/nrow(loyaldata)
loyalcustomerlifetimevalue <- loyalavgorder*loyalpurchfreq*3.87
print(loyaldataage)



championsdata <- RFM[RFM$Frequency > 4,]
championsdatafreq <- mean(championsdata$Frequency)
championsdataage <- mean(championsdata$Age)
championsdatagend <- mean(championsdata$Gender)
championsavgorder <- sum(championsdata$Monetary)/sum(championsdata$Frequency)
championspurchfreq <- sum(championsdata$Frequency)/nrow(championsdata)
championscustomerlifetimevalue <- championsavgorder*championspurchfreq*3.87
print(championsdataage)

nrow(championsdata)/nrow(loyaldata)

print(nrow(loyaldata))

print(nrow(championsdata))

freqs <- c(nrow(onedata), nrow(twodata), nrow(loyaldata), nrow(championsdata))

par(mar = c(10, 4.1, 4.1, 2.1))
b<- barplot(freqs, main = "LAS - RFM Customer Analysis and Recommended Actions", col = "chartreuse", ylab = '# of Customers', names.arg = c('1 visit', '2 visits', 'Loyal Customer', 'Champion'), ylim = c(0, 700))
mtext(paste("LTV =  ", c(172, 281, 353, 745)), side=1, line=3, at=b)
mtext(paste("Frequency =  ", c(1, 2, 3.4, 7.5)), side=1, line=4, at=b)
mtext(paste("Spend =  ", c(44, 73, 91, 193)), side=1, line=5, at=b)
mtext(paste("Age =  ", c(26, 25, 26, 27)), side=1, line=6, at=b)
mtext(paste("Sex =  ", c('M')), side=1, line=7, at=b)
mtext(paste("Most Popular Category =  ", c('IPA')), side=1, line=8, at=b)

##############################

items <- read.csv(file="k-means clustering data.csv", header=TRUE, sep=",")
items$Frequency <- as.numeric(as.character(items$freq))

oneitems <- items[items$Frequency == 1,]
twoitems <- items[items$Frequency == 2,]
loyalitems <- items[items$Frequency > 2 & items$Frequency <= 4,]
championsitems <- items[items$Frequency > 4,]

plot(oneitems$type)
plot(twoitems$type)
plot(loyalitems$type)
plot(championsitems$type)
