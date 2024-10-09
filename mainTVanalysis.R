library(tidyverse)
gss_cat
head(gss_cat)

#create a dataset of married and divorced with tv hours

tv_hours_marital <- gss_cat %>% 
  select(marital, tvhours,age) %>% 
  filter(marital %in% c("Divorced", "Married")) %>% 
  filter(age < 30)

#remove na
tv_hours_marital1 <- tv_hours_marital %>%
 filter(!is.na(tvhours))
  
           
#find total tv hours by marital

total_tv_hours <- tv_hours_marital1 %>% 
  group_by(marital) %>% 
  summarise(total_tv_hours=sum(tvhours))

#total count by marital
count_marital <- tv_hours_marital1 %>% 
  group_by(marital) %>% 
  count()
         
         
#find the mean tv hours by marital
mean_tv_hours <- tv_hours_marital1 %>%
  group_by(marital) %>% 
  summarise(mean_tvhours=mean(tvhours))

#join mean_tv_hours to total_tv_hours

library(dplyr)
library(here)
tv_summary <- total_tv_hours %>%
  full_join(mean_tv_hours, by = "marital") %>% 
  full_join(count_marital, by = "marital") %>% 
  filter(marital %in% c("Divorced", "Married")) %>% 
  rename(No_of_respondents=n)

write.csv(tv_summary, here("tv_hours_marital.csv"))

#tabulate result
tibble(tv_summary)
print(tv_summary)

