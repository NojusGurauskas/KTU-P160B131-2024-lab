library(tidyverse)
library(dplyr)
cat("Darbinė direktorija:", getwd())
download.file("https://atvira.sodra.lt/imones/downloads/2023/monthly-2023.csv.zip", "../data/temp")
unzip("../data/temp",  exdir = "../data/")

data<-read.csv2("../data/monthly-2023.csv")

unique(data$Ekonominės.veiklos.rūšies.kodas.ecoActCode.)
data%>%
  filter(data$Ekonominės.veiklos.rūšies.kodas.ecoActCode. ==561000)%>%
  write.csv("../data/561000.csv")

file.remove("../data/temp")
file.remove("../data/monthly-2023.csv")
file.remove("../img/pavyzdys1.png")
file.remove("../img/pavyzdys2.png")
file.remove("../img/pavyzdys3.png")
file.remove("../img/shiny_example.png")