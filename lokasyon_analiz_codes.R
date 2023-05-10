
#SİRKÜLASYON ÇALIŞMASI

#PACKAGES

library(lubridate)
library(tidyverse)

#DATA SETS


#smart_birimler_tbl <- SELECT * FROM [smartAx].[dbo].[vMusBirim]
#istihdam_log_saha_tbl <- SELECT * FROM DynamicsAX_Live.dbo.ABPYRPERSONELISTIHDAMLOGVIEW where EMPLOYEETYPE = 1
#birimler_lokasyon_tbl <- SELECT * FROM [smartAx].[dbo].[vMusProjeBirimLokasyon]




#Belli tarih aralığında o birimde çalışan personel sayısı hesaplanması


donem_tarih <- as.Date(dmy("01.05.2023"))
aktif <- as.Date(dmy("01.01.1900"))
hesaplanacak_aylar <- seq(from=as.Date("2023-01-01"), to=as.Date("2023-05-01"), by="month")


toplam_personel_sayisi <- function(donem = donem_tarih) {
  istihdam_log_tbl_filtered <- istihdam_log_saha_tbl %>%
    filter(STARTDATE <= donem) %>%
    filter(ENDDATE >= donem | is.na(ENDDATE)) %>%
    group_by(PROJNAME, PROJECTUNITNAME, PROJECTUNITID ) %>%
    rename(OrgSubeId = "BRANCHID",
           OrgBolgeId = "BUSINESSUNIT") %>%
    summarise(saha_per = length(unique(ABTCKIMLIKNO))) %>%
    mutate(Donem_tarih = donem) %>%
    mutate(Ay = format(donem, "%B")) %>%
    ungroup()

}


sirkulasyon_tablosu <- data.frame()

for (donem in as.list(hesaplanacak_aylar)) {
  p <- toplam_personel_sayisi(donem)
  sirkulasyon_tablosu = rbind(sirkulasyon_tablosu , p)
}
sirkulasyon_tablosu 

#Belli tarih aralığında o birimden ayrılan personel sayısı hesaplanması



ayrılan_personel_sayisi <- function(donem = donem_tarih) {
  istihdam_log_tbl_filtered <- istihdam_log_saha_tbl %>%
    filter(ENDDATE == donem) %>%
    group_by(PROJNAME, PROJECTUNITNAME, PROJECTUNITID ) %>%
    rename(OrgSubeId = "BRANCHID",
           OrgBolgeId = "BUSINESSUNIT") %>%
    summarise(ayrılan_per = length(unique(ABTCKIMLIKNO))) %>%
    mutate(Donem_tarih = donem) %>%
    mutate(Ay = format(donem, "%B")) %>%
    ungroup()
  
}


ayrılan_tablosu <- data.frame()

for (donem in as.list(hesaplanacak_aylar)) {
  p <- ayrılan_personel_sayisi(donem)
  ayrılan_tablosu = rbind(ayrılan_tablosu , p)
}
ayrılan_tablosu 




#TOPLAM PERSONEL İLE AYRILAN TABLOSU BİRLEŞTİRİLECEK. (5/10/2023)







#BURDAN SONRASI DATALAR BİRLEŞTİKTEN SONRA DEVAM EDECEK.



#ORTALAMA SİRKÜLASYON TABLOSU
#Birimlerin dönemlik hesaplanan sirkülasyon oranlarının ortalamasının alınarak 2023 yılı ortalama sirkülasyon oranının hesaplanması
sirkulasyon_tablosu_avg <- sirkulasyon_tablosu %>%
  group_by(PROJNAME, PROJECTUNITNAME, PROJECTUNITID) %>%
  summarise(OrtSirkülasyon = mean(sirkülasyon_oranı, na.rm = TRUE)) %>%
  ungroup()

sirkulasyon_tablosu_avg 



#LOKASYON ÇALIŞMASI ÖN HAZIRLIK
#sirkulasyon_tablosu_avg ile birimler_lokasyon_tbl tablolarının birleştirilmesi


sirkulasyon_tablosu_avg <- sirkulasyon_tablosu_avg%>%  rename(AxBirimId = "PROJECTUNITID")

colnames(birimler_lokasyon_tbl)

lokasyon_data <- sirkulasyon_tablosu_avg %>% 
  left_join(birimler_lokasyon_tbl, by= "AxBirimId") %>% 
  select(PROJNAME, PROJECTUNITNAME, AxBirimId, OrtSirkülasyon, Bölge, Sube, Segment, AltSegment, HizmetYeri, Enlem, Boylam)


lokasyon_data


