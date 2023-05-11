

#FİNANS

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

