childCarSeats_clean_filename <- "../Data/ChildCarSeats_clean.csv"
childCarSeats_clean <- read.csv(file=childCarSeats_clean_filename, header=TRUE, sep=",", na.strings=c(""," ","NA"))
head(childCarSeats_clean)
str(childCarSeats_clean)

childCarSeats_clean$ShelveLoc <- as.factor(childCarSeats_clean$ShelveLoc)
childCarSeats_clean$Urban <- as.factor(childCarSeats_clean$Urban)
childCarSeats_clean$US <- as.factor(childCarSeats_clean$US)
str(childCarSeats_clean)

# Gr�ficas para variables categ�ricas

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))


counts <- table(childCarSeats_clean$ShelveLoc)
barplot(counts, main="Distribuci�n de la calidad en cada ubicaci�n",
        xlab="N�mero de sillitas por cada categor�a", col = rainbow(length(levels(childCarSeats_clean$ShelveLoc))))

mytableUrban <- table(childCarSeats_clean$Urban)
pctUrban <- round(mytableUrban/sum(mytableUrban)*100)
lblsUrban <- paste(names(mytableUrban), "\n", pctUrban, sep="")
lblsUrban <- paste (lblsUrban, '%', sep="")
pie(mytableUrban, labels = lblsUrban,
    main="Pie Chart of Urban\n")


mytableUS <- table(childCarSeats_clean$US)
pctUS <- round(mytableUS/sum(mytableUS)*100)
lblsUS <- paste(names(mytableUS), "\n", pct, sep="")
lblsUS <- paste (lblsUS, '%', sep="")
pie(mytableUS, labels = lblsUS, col=rainbow(length(lblsUS)),
    main="Pie Chart of US\n")
unlist(lapply(childCarSeats_clean, is.factor))
# Gr�ficas para variables num�ricas

par(mfrow=c(2,4))

dotchart(childCarSeats_clean$Sales,labels=,cex=0.7,
         main="Ventas por ubicaci�n",
         xlab="Ventas por mil")
## Como se puede observar, la mayor�a de las tiendas venden entre 5 mil y 10 mil unidades de sillas

colorForHistograms = rainbow(table (unlist(lapply(childCarSeats_clean, is.numeric)))["TRUE"] - 1)

hist(childCarSeats_clean$CompPrice, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[1],main="Precio que cobra la competencia",
     xlab="Precio en euros")

hist(childCarSeats_clean$Income, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[2],main="Nivel de ingresos comunitarios",
     xlab="Nivel de ingresos en miles de d�lares")

hist(childCarSeats_clean$Advertising, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[3],main="Presupuesto de publicida local",
     xlab="Presupuesto en miles de d�lares")

hist(childCarSeats_clean$Population, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[4],main="Tama�o de la poblaci�n en la regi�n",
     xlab="Tama�o de la poblaci�n en miles")

hist(childCarSeats_clean$Price, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[5],main="Precio de las sillitas",
     xlab="Precio en euros")

hist(childCarSeats_clean$Age, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[6],main="Edad media de la poblaci�n",
     xlab="Edad media en a�os")

hist(childCarSeats_clean$Age, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[7],main="Media del nivel de educaci�n",
     xlab="A�os de educaci�n")

