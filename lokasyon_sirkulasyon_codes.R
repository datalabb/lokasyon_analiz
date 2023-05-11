
#SİRKÜLASYON ÇALIŞMASI



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


personel_tablosu <- data.frame()

for (donem in as.list(hesaplanacak_aylar)) {
  p <- toplam_personel_sayisi(donem)
  personel_tablosu = rbind(personel_tablosu , p)
}
personel_tablosu 

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





#TOPLAM PERSONEL İLE AYRILAN TABLOSU BİRLEŞTİRİLMESİ


genel_tablo <- personel_tablosu %>% left_join(ayrılan_tablosu, by= "PROJECTUNITNAME") %>% 
  select(PROJNAME.x, PROJECTUNITNAME, PROJECTUNITID.x, saha_per, ayrılan_per, Donem_tarih.x, Ay.x)


genel_tablo$ayrılan_per[is.na(genel_tablo$ayrılan_per)] <- 0


str(genel_tablo)

#BİRİM/DÖNEMBAZLI SİRKÜLASYON KATSAYISININ DATAYA SÜTUN OLARAK EKLENMESİ


genel_tablo$sirkulasyon_oranı <- genel_tablo$ayrılan_per/ genel_tablo$saha_per


#ORTALAMA SİRKÜLASYON TABLOSU
#Birimlerin dönemlik hesaplanan sirkülasyon oranlarının ortalamasının alınarak birim bazlı 2023 yılı ortalama sirkülasyon oranının hesaplanması
personel_tablosu_avg <- genel_tablo %>%
  group_by(PROJNAME.x, PROJECTUNITNAME, PROJECTUNITID.x) %>%
  summarise(OrtSirkulasyon = mean(sirkulasyon_oranı, na.rm = TRUE)) %>%
  ungroup()

personel_tablosu_avg 


