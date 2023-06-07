# LOCATION ANALYSIS APP ----



##################



install.packages("cli")
library(cli)

#PACKAGES ----

if (!require("RColorBrewer")) {
  install.packages("xgboost")
  library(RColorBrewer)
}



library(xgboost)
library(flexdashboard)
library(shiny)
library(DT)
library(readxl)
library(lubridate)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(fs)
library(dplyr)
library(scales)


# Core
library(tidyverse)
library(tidyquant)
library(scales)
library(plotly)

library(shinyWidgets)
library(shiny)
library(viridisLite)
library(treemap)


library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)


# Map
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)


# Modeling
library(timetk)
library(parsnip)
library(collapsibleTree)
# Apriori



if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("arules")) {
  install.packages("arules")
  library(RColorBrewer)
}
if (!require("arulesViz")) {
  install.packages("arulesViz")
  library(RColorBrewer)
}



library(arules)
library(arulesViz)
library(lubridate)


# 1.0 TABLES ----

## 1.1. SMART BIRIMLER ----
smart_birimler_tbl <- readRDS("C:/Users/tugce.topcu/Desktop/R/Data/smart_birimler_saved.rds")

smart_birimler_tbl$Il <- smart_birimler_tbl$Il %>% iconv(to="UTF-8")
smart_birimler_tbl$Ilce <- smart_birimler_tbl$Ilce %>% iconv(to="UTF-8")

smart_birimler_tbl

## 1.2 LOKASYON ----
birimler_lokasyon_tbl <- readRDS("C:/Users/tugce.topcu/Desktop/R/Data/birimler_lokasyon_saved.rds")
birimler_lokasyon_tbl

str(birimler_lokasyon_tbl)

## 1.3 PERSONEL ----



personel_tbl <- readRDS("C:/Users/tugce.topcu/Desktop/R/Data/personel.rds")

personel_tbl %>% filter(AktifMi == TRUE, PersonelTipiKodu == "S")
str(personel_tbl)

# 2.0 DATA PREPERATION ----

## 2.1 Birim Lokasyon ve Personel Sayısı Tablosu ----
# Birim-Aktif Personel Sayısı

birim_personel_tbl <- personel_tbl %>%
  filter(AktifMi == TRUE,
         PersonelTipiKodu == "S") %>%
  group_by(MusBirimAd) %>%
  summarise(kisisay = length((MusBirimAd))) %>%
  ungroup()

birim_personel_tbl

# Birim-Personel tablosu ve Lokasyon Tablosu eşleştirme



birimler_lokasyon_tbl_rvz <- birimler_lokasyon_tbl %>%
  rename(MusBirimAd = "Birim") %>%
  select(MusBirimAd, AxBirimId, Enlem, Boylam, Segment) %>%
  rename(MusBirimId = "AxBirimId")




birim_per_loc_tbl <- birim_personel_tbl %>%
  left_join(birimler_lokasyon_tbl_rvz, by = "MusBirimAd") %>%
  filter(!is.na(Enlem))

birim_per_loc_tbl

## 2.2 Birim Sirkülasyon Tablosu ----



# Belli tarih aralığında o birimde çalışan personel sayısı hesaplanması




donem_tarih <- as.Date(dmy("01.05.2023"))
aktif <- as.Date(dmy("01.01.1900"))
hesaplanacak_aylar <- seq(from=as.Date("2023-01-01"), to=as.Date("2023-05-01"), by="month")



toplam_personel_sayisi <- function(donem = donem_tarih) {
  istihdam_log_tbl_filtered <- istihdam_log_saha_tbl %>%
    filter(STARTDATE <= donem) %>%
    filter (ENDDATE == aktif | ENDDATE > donem) %>%
    group_by(PROJNAME, PROJECTUNITNAME, PROJECTUNITID) %>%
    summarise(saha_per = length(unique(ABTCKIMLIKNO))) %>%
    mutate(Donem_tarih = donem) %>%
    mutate(Ay = format(donem,"%B")) %>%
    ungroup()
  
}



personel_tablosu <- data.frame()




for (donem in as.list(hesaplanacak_aylar)) {
  p <- toplam_personel_sayisi(donem)
  personel_tablosu = rbind(personel_tablosu , p)
}



personel_tablosu



# Belli tarih aralığında o birimden ayrılan personel sayısı hesaplanması



ayrilma_kod <- c("01", "02", "03", "04", "05", "08", "09", "10", "12", "13", "14", "17", "18", "22", "23", "24", "25", "27", "28", "29", "34", "36")




ayrılan_personel_sayisi <- function(donem = donem_tarih) {
  istihdam_log_tbl_filtered <- istihdam_log_saha_tbl %>%
    filter(ISTENAYRILMAKODUSGK %in% ayrilma_kod) %>%
    filter(ENDDATE >= donem & ENDDATE < (donem %m+% months(1))) %>%
    
    group_by(PROJNAME, PROJECTUNITNAME, PROJECTUNITID) %>%
    summarise(ayrılan_per = length(ABTCKIMLIKNO)) %>%
    mutate(Donem_tarih = donem) %>%
    mutate(Ay = format(donem,"%B")) %>%
    ungroup()
  
}



ayrılan_tablosu <- data.frame()




for (donem in as.list(hesaplanacak_aylar)) {
  p <- ayrılan_personel_sayisi(donem)
  ayrılan_tablosu = rbind(ayrılan_tablosu , p)
}



ayrılan_tablosu





# Dönemsel toplam personel ile ayrılan personel tablolarının birleştirilmesi




per_say_cikis_tablo <- personel_tablosu %>% left_join(ayrılan_tablosu, by= c("PROJECTUNITID", "Donem_tarih")) %>%
  select(PROJNAME.x, PROJECTUNITNAME.x, PROJECTUNITID, Donem_tarih, Ay.x,saha_per, ayrılan_per) %>%
  rename("Proje" = PROJNAME.x,
         "MusBirimAd" = PROJECTUNITNAME.x,
         "MusBirimId" = PROJECTUNITID,
         "Donem" = Donem_tarih,
         "Ay" = Ay.x)




per_say_cikis_tablo$ayrılan_per[is.na(per_say_cikis_tablo$ayrılan_per)] <- 0



per_say_cikis_tablo



# Birim bazlı sirkülasyon



birim_sirkulasyon_tbl <- per_say_cikis_tablo %>%
  mutate(sirkulasyon = round(ayrılan_per/saha_per, digits = 4))



ort_birim_sirkulasyon_tbl <- birim_sirkulasyon_tbl %>%
  group_by(MusBirimAd, MusBirimId) %>%
  summarise(aylık_ort_sirk = mean(sirkulasyon),
            toplam_sirk = sum(sirkulasyon)) %>%
  ungroup()



ort_birim_sirkulasyon_tbl





## 2.3 Lokasyon-Personel-Sirkülasyon Birleştirme ----



birim_per_loc_sirk_tbl <- birim_per_loc_tbl %>% left_join(ort_birim_sirkulasyon_tbl, by = "MusBirimId") %>%
  select(-MusBirimAd.y) %>%
  rename(MusBirimAd = "MusBirimAd.x")



birim_per_loc_sirk_tbl




## 2.4 Finans ----




# Birim/Proje Bazında Kar Yüzdelerinin Hesaplanması



finans_tbl_kar <- finans_tbl %>%
  
  filter(PERIODMONTH < max(PERIODMONTH)-1) %>%
  select(AMOUNTCREDIT, AMOUNTDEBIT, BRANCHID, BUSINESSUNIT, CUSTACCOUNT, EGC, MILLI, PERIODENDDATE, PERIODMONTH, PERIODYEAR, PROJID) %>%
  group_by(CUSTACCOUNT, PROJID, EGC, MILLI, PERIODYEAR, PERIODMONTH, PERIODENDDATE) %>%
  summarise(AMOUNTCREDIT2 = sum(AMOUNTCREDIT),
            AMOUNTDEBIT2 = sum(AMOUNTDEBIT)) %>%
  
  mutate(Gelir = AMOUNTCREDIT2 * -1,
         Gider = AMOUNTDEBIT2 * -1,
         Kar1 = Gelir + Gider,
         Kar1_Yuzde = Kar1 / Gelir)



finans_tbl_kar_cum <- finans_tbl_kar %>%
  
  group_by(CUSTACCOUNT, PROJID) %>%
  
  mutate(Gelir_cum = cumsum(Gelir),
         Gider_cum = cumsum(Gider),
         Kar1_cum = Gelir_cum + Gider_cum,
         Kar1_Yuzde_cum = Kar1_cum / Gelir_cum) %>%
  ungroup()



# filter(CUSTACCOUNT == "M03014")



finans_tbl_kar_cum <- finans_tbl_kar_cum [!finans_tbl_kar_cum$CUSTACCOUNT == "", ]
finans_tbl_kar_cum



finans_tbl_kar_last <- finans_tbl_kar_cum %>%
  filter(PERIODMONTH == max(PERIODMONTH)) %>%
  select(PROJID, PERIODMONTH, Gelir_cum, Gider_cum, Kar1_cum, Kar1_Yuzde_cum) %>%
  rename(MusBirimId = PROJID) %>%
  select(-PERIODMONTH)



finans_tbl_kar_last<- finans_tbl_kar_last [!finans_tbl_kar_last$Kar1_Yuzde_cum == -Inf, ]
finans_tbl_kar_last<- finans_tbl_kar_last [!finans_tbl_kar_last$Kar1_Yuzde_cum == Inf, ]



finans_tbl_kar_last




## 2.5 Tüm Tablo Birleştirme (Lokasyon-Personel-Sirkülasyon-Finans) ----



tum_tablo <- birim_per_loc_sirk_tbl %>%
  left_join(finans_tbl_kar_last, by = "MusBirimId") %>%
  filter(!is.na(Kar1_Yuzde_cum))



tum_tablo




# 3.0 MAPPING ----



tum_tablo$Enlem <- as.double(tum_tablo$Enlem)
tum_tablo$Boylam <- as.double(tum_tablo$Boylam)



location_analysis_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = tum_tablo, lng = tum_tablo$Boylam, lat = tum_tablo$Enlem, radius = 2)



location_analysis_map




#### Distance ----




#Function for distance




earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}



earth.dist



# Nokta



nokta <- data.frame (Boylam = c(32.53112, 29.45234,35.45234),
                     Enlem = c(39.98345,38.76534,38.76534)
)



nokta




# Distance




dist <- 50



loc1 <- tum_tablo %>%
  mutate(distance = earth.dist(tum_tablo$Boylam[[1]],tum_tablo$Enlem[[1]],
                               tum_tablo$Boylam, tum_tablo$Enlem)) %>%
  filter(distance <= dist)
loc1




#### Results / Map ----



test_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = tum_tablo,
                   lng = tum_tablo$Boylam, lat = tum_tablo$Enlem, radius = 1) %>%
  #
  addCircles(
    lng = tum_tablo$Boylam[[1]],
    lat = tum_tablo$Enlem[[1]],
    radius = 50000,
    weight = 5,
    opacity = 0.5,
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>%
  addCircles(
    lng = loc1$Boylam,
    lat = loc1$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green")




test_map



##### Map Function ----



# Risk_Map_Function ----




per_map_func <- function(diameter = 50, i = 1) {
  
  # Selected Area Table
  
  area_per <- tum_tablo %>%
    
    mutate(distance = earth.dist(tum_tablo$Boylam[[i]],tum_tablo$Enlem[[i]],
                                 tum_tablo$Boylam, tum_tablo$Enlem)) %>%
    filter(distance <= diameter)
  
  total_per <- sum(area_per$kisisay)
  area_ort_sirk <- scales::percent(mean(area_per$toplam_sirk), accuracy = 0.1)
  area_aylık_ort_sirk <- scales::percent(mean(area_per$aylık_ort_sirk), accuracy =0.1)
  area_ort_kar1_yuzde <- scales::percent(mean(area_per$Kar1_Yuzde_cum), accuracy =0.1)
  
  
  # Map
  
  contenido <- paste(sep = "<br/>",
                     #paste0("<img src='https://www.r-project.org/logo/Rlogo.png", "' />"),
                     paste0("<b>Toplam Personel: </b>", total_per),
                     paste0("<b>Toplam Ortalama Sirkülasyon: </b>", area_ort_sirk),
                     paste0("<b>Aylık Ortalama Sirkülasyon: </b>",area_aylık_ort_sirk),
                     paste0("<b>Ortalama Kar1%: </b>", area_ort_kar1_yuzde)
  )
  
  # paste0("<a href='https://en.wikipedia.org/wiki/Frigor%C3%ADfico_Anglo_del_Uruguay", "'>Link</a>"))
  
  
  
  per_map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = tum_tablo, lng = tum_tablo$Boylam, lat = tum_tablo$Enlem, radius = 1) %>%
    #
    addCircles(
      lng = tum_tablo$Boylam[[i]],
      lat = tum_tablo$Enlem[[i]],
      radius = diameter*1000,
      weight = 5,
      opacity = 0.5,
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>%
    #
    
    addMarkers(lng = tum_tablo$Boylam[[i]], lat = tum_tablo$Enlem[[i]]) %>%
    #
    addPopups(data = tum_tablo,
              lng = tum_tablo$Boylam[[i]],
              lat = tum_tablo$Enlem[[i]],
              popup = contenido,
              options = popupOptions(closeButton = TRUE)) %>%
    #
    
    
    
    addCircles(
      lng = area_per$Boylam,
      lat = area_per$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green")
  
  print(per_map)
  
  print(area_per)
  print(total_per)
}



per_map_func(100, 20)

