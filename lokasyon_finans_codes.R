

#FİNANS

finans_tbl <- readRDS("C:/Users/tugce.topcu/Desktop/R/Data/finans_tbl.rds")

#Birim/Proje Bazında Kar Yüzdelerinin Hesaplanması

#finans_tbl <- DMRPROFIT1SUMMARY where PERIODYEAR = 2023

library(dplyr)

current_date <- as.Date(Sys.Date())
start_date <- as.Date(format(current_date, "%Y-%m-01")) - months(2)


filtered_tbl <- finans_tbl %>%
  filter(PERIODENDDATE >= start_date & PERIODENDDATE <= current_date)


#KAR1
filtered_tbl <- filtered_tbl %>%
  mutate(KAR1 = (-AMOUNTCREDIT - (-AMOUNTDEBIT)) / (-AMOUNTCREDIT))


kar1_data <- filtered_tbl %>%
  group_by(PROJID) %>%
  summarize(Avg_KAR1 = mean(KAR1)) %>% 
  ungroup()




################# GÖKHAN USTA PART #####################


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
  select(CUSTACCOUNT, PROJID, PERIODMONTH, Gelir_cum, Gider_cum, Kar1_cum, Kar1_Yuzde_cum) %>%
  rename(AxProjeId = CUSTACCOUNT)



finans_tbl_kar_last <- finans_tbl_kar_last [!finans_tbl_kar_last$Kar1_Yuzde_cum == -Inf, ]
finans_tbl_kar_last <- finans_tbl_kar_last [!finans_tbl_kar_last$Kar1_Yuzde_cum == Inf, ]



finans_tbl_kar_last %>% view()
