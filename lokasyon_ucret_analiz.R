
#UCRET ÇALIŞMASI


#DATASET

#ucret_tbl <- dbo.ABDMRUNITSTRUCTUREALLOWANCESCALE

library(dplyr)


#DATA OLUŞTURULMASI
#Birim bazında gruplandırılarak ortalama ücretler hesaplandı

ortalama_ucret <- ucret_tbl %>%
  filter(ALLOWANCESTARTDATE > as.Date("2022-12-31")) %>%
  group_by(CUSTNAME, NAME, PROJID) %>%
  summarise(ortalama_ucret = mean(WAGEAMOUNT, na.rm = TRUE)) %>% 
  filter(ortalama_ucret != 0)


summary(ortalama_ucret)

#Lokasyon datası ile birleştirildi

ortalama_ucret <- ortalama_ucret %>%  rename(AxBirimId = "PROJID")

colnames(ortalama_ucret)

lokasyon_data_ucret <- ortalama_ucret %>% 
  left_join(birimler_lokasyon_tbl, by= "AxBirimId") %>% 
  select(CUSTNAME,NAME, AxBirimId,ortalama_ucret, Bölge, Sube, Segment, AltSegment, HizmetYeri, Enlem, Boylam)


lokasyon_data_ucret
