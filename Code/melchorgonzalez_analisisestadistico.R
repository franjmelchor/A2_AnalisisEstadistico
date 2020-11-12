setwd("C:\\Users/PCI04/Desktop/Máster UOC/Est. Avanzada/A2 - Análisis estadístico I/Code/")

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

hist(childCarSeats_clean$Education, breaks=sqrt(dim(childCarSeats_clean)[1]), col=colorForHistograms[7],main="Media del nivel de educación",
     xlab="Años de educación")
min(childCarSeats_clean$Education)
max(childCarSeats_clean$Population)


dim(childCarSeats_clean)[1] 

s = sd(childCarSeats_clean$Sales)
n = dim(childCarSeats_clean)[1]

me = abs(qt((1-0.95)/2,n-1 )) * (s/sqrt(n))
me
mean(childCarSeats_clean$Sales)
x = mean(childCarSeats_clean$Sales)

x - me

confidenceInterval = c(x-me,x+me)
confidenceInterval

install.packages("Rmisc")
library(Rmisc)

CI(childCarSeats_clean$Sales, ci=0.95)
length(childCarSeats_clean$Sales)

getConfidentInterval<- function(x){
    s = sd(x)
    n = length(x)
    me = abs(qt((1-0.95)/2,n-1 )) * (s/sqrt(n))
    x = mean(childCarSeats_clean$Sales)
    confidenceInterval = c(x-me,x+me)
    return (confidenceInterval)
}
getConfidentInterval(childCarSeats_clean$Sales)
getConfidentInterval(childCarSeats_clean[childCarSeats_clean$US=='Yes',]$Sales)
getConfidentInterval(childCarSeats_clean[childCarSeats_clean$US=='No',]$Sales)

childCarSeats_clean[childCarSeats_clean$US=='Yes',]$Sales



x <- childCarSeats_clean$Sales
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)



Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
median(childCarSeats_clean$Sales)
Mode(childCarSeats_clean$Sales)
mean(childCarSeats_clean$Sales)


seq(min(x),max(x),length=40)

abs(qt((1-0.95)/2,38 ))


sqrt (25)
getComunStd <- function(m1,m2) {
    s1 = sd(m1)
    s2 = sd(m2)
    n1 = length(m1)
    n2 = length(m2)
    comunStd = sqrt( ((n1 - 1)*(s1^2) + (n2-1)*(s2^2) ) / (n1 + n2 -2) )
    return (comunStd)
}

getObservedValue <- function (m1,m2) {
    return ((mean(m1) - mean(m2)) / ( getComunStd(m1,m2) * sqrt((1/length(m1)) + (1/length(m2)) ) ) )
}

getCriticalValueStudent <- function(n1,n2) {
    return (qt((1-0.95)/2,(n1+n2-2)))
}

getPValueStudent <- function (tValue, degreesOfFreedom) {
    return (pt(t, lower.tail = TRUE))
}



##Menor que (<)
pt (0.971,8,lower.tail = TRUE)

##Distinto
2 * ( 1 - pt(2.34,8,lower.tail = TRUE))

##Mayor que (>)
 1 - pt (0.971,8,lower.tail = TRUE)



a <- c(10.7,14.8,12.3,16.5,10.2,11.9)
b <- c(13.4,11.5,11.2,15.1,13.3,12.9)

t = getObservedValue(a,b)
df = length(a) + length(b) - 2
getPValueStudent(t,df)

getComunStd(a,b)

mean(a)
mean(b)

(mean(a) - mean(b) )/ getComunStd(a,b)

getComunStd(a,b) * sqrt((1/length(a)) + (1/length(b)))


childCarSeats_clean[childCarSeats_clean$Urban=='Yes',]$Sales


qnorm((1-0.95)/2)

1 - pnorm(2.87,lower.tail = TRUE)

getCommonPopulationProportion <- function (n1,n2,p1,p2) {
    return((n1*p1 + n2*p2) / (n1+n2))
}

getProportionObservedValue <- function (n1,n2,p1,p2) {
    p = getCommonPopulationProportion(n1,n2,p1,p2)
    stdError = sqrt(p*(1-p)*((1/n1)+(1/n2)))
    z=(p1 - p2)/stdError
    return (z)
}

getCriticalValueNormal <- function() {
    return (abs((qnorm((1-0.95)/2))))
}

getPValueNormal <- function(z) {
    return (pnorm(z,lower.tail = TRUE))
}
getProportionObservedValue(200,500,(120/200),(240/500))
1 - getPValueNormal(2.87)
p1 = dim(childCarSeats_clean[childCarSeats_clean$Price < childCarSeats_clean$CompPrice,])[1] / length(childCarSeats_clean$Price)
p2 = dim(childCarSeats_clean[childCarSeats_clean$Price > childCarSeats_clean$CompPrice,])[1] / length(childCarSeats_clean$Price)
n2 = length(childCarSeats_clean$Price)
n1 = length(childCarSeats_clean$Price)
z = getProportionObservedValue(n1,n2,p1,p2)

childCarSeats_clean[childCarSeats_clean$Price > childCarSeats_clean$CompPrice,]


dim(childCarSeats_clean[childCarSeats_clean$Price < childCarSeats_clean$CompPrice,])[1]

length(childCarSeats_clean[childCarSeats_clean$Price > childCarSeats_clean$CompPrice,])

dim(childCarSeats_clean[childCarSeats_clean$Price == childCarSeats_clean$CompPrice,])[1]

childCarSeats_clean[childCarSeats_clean$Price < childCarSeats_clean$CompPrice,]

 1 - pnorm(2,87)

getCommonPopulationProportion(1500,2000,(725/1500),(1050/2000))


df6 = childCarSeats_clean[childCarSeats_clean$Price < childCarSeats_clean$CompPrice,]
df6
p16 = dim (df6[df6$US == 'Yes',])[1] / dim(df6)[1]
p26 = dim (df6[df6$US == 'No',])[1] / dim(df6)[1]
n = dim(df6)[1]

##HO: p16-p26 = 0
##H1: p16-p26 != 0

z6 = getProportionObservedValue(n,n,p16,p26)

pnorm6 = 2 * (1 - getPValueNormal(z6))


## como pnorm6 < H0 aceptamos la hipótesis nula 