childCarSeats_clean_filename <- "../Data/ChildCarSeats_clean.csv"
childCarSeats_clean <- read.csv(file=childCarSeats_clean_filename, header=TRUE, sep=",", na.strings=c(""," ","NA"))
head(childCarSeats_clean)
str(childCarSeats_clean)

childCarSeats_clean$ShelveLoc <- as.factor(childCarSeats_clean$ShelveLoc)
childCarSeats_clean$Urban <- as.factor(childCarSeats_clean$Urban)
childCarSeats_clean$US <- as.factor(childCarSeats_clean$US)
str(childCarSeats_clean)

# Gráficas para variables categóricas

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))


counts <- table(childCarSeats_clean$ShelveLoc)
barplot(counts, main="Distribución de la calidad en cada ubicación",
        xlab="Número de sillitas por cada categoría", col = rainbow(length(levels(childCarSeats_clean$ShelveLoc))))

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
# Gráficas para variables numéricas

par(mfrow=c(2,4))

dotchart(childCarSeats_clean$Sales,labels=,cex=0.7,
         main="Ventas por ubicación",
         xlab="Ventas por mil")
## Como se puede observar, la mayoría de las tiendas venden entre 5 mil y 10 mil unidades de sillas

colorForHistograms = rainbow(table (unlist(lapply(childCarSeats_clean, is.numeric)))["TRUE"] - 1)

hist(childCarSeats_clean$CompPrice, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[1],main="Precio que cobra la competencia",
     xlab="Precio en euros")

hist(childCarSeats_clean$Income, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[2],main="Nivel de ingresos comunitarios",
     xlab="Nivel de ingresos en miles de dólares")

hist(childCarSeats_clean$Advertising, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[3],main="Presupuesto de publicida local",
     xlab="Presupuesto en miles de dólares")

hist(childCarSeats_clean$Population, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[4],main="Tamaño de la población en la región",
     xlab="Tamaño de la población en miles")

hist(childCarSeats_clean$Price, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[5],main="Precio de las sillitas",
     xlab="Precio en euros")

hist(childCarSeats_clean$Age, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[6],main="Edad media de la población",
     xlab="Edad media en años")

hist(childCarSeats_clean$Age, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[7],main="Media del nivel de educación",
     xlab="Años de educación")


