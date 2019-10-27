library(readxl)
library(tidyverse)
WTI2 <- read_excel("WTI2.xlsx")

#parameterek
X = 1
Y = 2

kezdo_datum = "2015-01-01"
veg_datum = "2016-12-31"
kesleltet <- 100
ablak_meret <- 100

adat_kezdo = "2010-01-01"
adat_vegso = "2016-12-31"

decent <- as.integer(as.Date(kezdo_datum) - as.Date(adat_kezdo))
i = decent + kesleltet + 1 #az elozo adatsort eltolom
output = NULL
# korr_darab <- NROW(WTI2$CL1) - ablak_meret
vegso <- as.integer(as.Date(veg_datum) - as.Date(adat_kezdo)) + 1



check_params <- function() {
  
}

read_datafromxlsx <- function() {
  
}

# Kiszamolja a korrelaciot
compute_correlation <- function() {
  while (i <= vegso - ablak_meret) {
    correlation = cor(WTI2[[1 + X]][i:(ablak_meret + i)], WTI2[[1 + Y]][(i -
                                                                           kesleltet):(ablak_meret + i - kesleltet)])
    output = rbind(output, correlation)
    i = i + 1
  }
  return (output)
}



correls <- compute_correlation()

dates <-
  as.Date(WTI2$Date[(decent + ablak_meret + kesleltet + 1) : vegso])

# Ez az a dataframe, amit kiszeretnenk plottolni. Ez ket oszlopbol all: a datumokbol
# es a korrelaciokbol. Ezeket a cbind() fuggvennyel egyesitettuk.
plot_data <- (cbind(dates, correls))

# Kirajzolja az adatokat, elso oszlopaban a datumok, masodikban a korrelaciok
plot_correl <- function (labelx, labely, labelmain) {
  # Szepen kirajzolja, label-ekkel ellatja a tengelyeket, stb.
  plot(
    V2 ~ dates,
    plot_data,
    xaxt = "n",
    type = "l",
    xlab = labelx,
    ylab = labely,
    main = labelmain
  )
  # Az x-tengelyen a datumokat ilyen formatumra pl: 2011 aug. 20 formazza
  axis(1, dates, format(dates, "20%y %b %d"), cex.axis = .7)
}

plot_correl("Datum", "Korrelacio", "Dinamikus korrelacio")


#plot(V2 ~ V1, alma, xaxt = "n", type = "l", xlab="datum", ylab="korrelacio", main="Dinamikus korrelacio" )
#axis(1, as.Date(WTI2$Date[ablak_meret:(korr_darab+ablak_meret)]), format(as.Date(WTI2$Date[ablak_meret:(korr_darab+ablak_meret)]), "%y %b %d"), cex.axis = .7)