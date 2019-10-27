library(readxl)
WTI2 <- read_excel("beadando/WTI2.xlsx")

#paraméterek
X = 1
Y = 2

kezdo_datum = "2015-01-01" 
veg_datum = "2016-12-31"
kesleltet<-100
ablak_meret<-100

adat_kezdo = "2010-01-01"
adat_vegso = "2016-12-31"

decent<-as.integer(as.Date(kezdo_datum)-as.Date(adat_kezdo))
i= decent + kesleltet+1 #az elő adatsort eltolom
output= NULL
korr_darab<-NROW(WTI2$CL1)-ablak_meret
vegso<-as.integer(as.Date(veg_datum)-as.Date(adat_kezdo))+1


while(i<=vegso-ablak_meret){
  correlation=cor(WTI2[[1+X]][i:(ablak_meret+i)],WTI2[[1+Y]][(i-kesleltet):(ablak_meret+i-kesleltet)])
  output=rbind(output, correlation)
  i=i+1
  
}  
output
dim(output)
dates <- as.Date(WTI2$Date[(decent+ablak_meret + kesleltet+1):vegso])

alma<-(cbind(dates, output))
plot(V2 ~ dates, alma, xaxt = "n", type = "l", xlab="Dátum", ylab="Korreláció", main="Dinamikus korreláció" )
axis(1, dates, format(dates, "20%y %b %d"), cex.axis = .7)

#plot(V2 ~ V1, alma, xaxt = "n", type = "l", xlab="dátum", ylab="korreláció", main="Dinamikus korreláció" )
#axis(1, as.Date(WTI2$Date[ablak_meret:(korr_darab+ablak_meret)]), format(as.Date(WTI2$Date[ablak_meret:(korr_darab+ablak_meret)]), "%y %b %d"), cex.axis = .7)