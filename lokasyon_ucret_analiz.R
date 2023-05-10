
#UCRET ÇALIŞMASI

#DATASET


#ucret_tbl <- dbo.ABDMRUNITSTRUCTUREALLOWANCESCALE
#birimler_lokasyon_tbl <- SELECT * FROM [smartAx].[dbo].[vMusProjeBirimLokasyon]

library(dplyr)

#DATA OLUŞTURULMASI
#Birim bazında gruplandırılarak ortalama ücretler hesaplandı

ortalama_ucret <- ucret_tbl %>%
  filter(ALLOWANCESTARTDATE > as.Date("2022-12-31")) %>% 
  filter(WAGEAMOUNT != 0 & WAGEAMOUNT <= 20000) %>% 
  group_by(CUSTNAME, NAME, PROJID)  %>% 
  summarise(ortalama_ucret = mean(WAGEAMOUNT, na.rm = TRUE)) %>% 
  ungroup()


summary(ortalama_ucret)

