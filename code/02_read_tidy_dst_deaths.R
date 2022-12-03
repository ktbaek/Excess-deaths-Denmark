dst_dodc1 <- read_csv2("https://api.statbank.dk/v1/data/DODC1/CSV?delimiter=Semicolon&K%C3%98N=1%2C2&ALDER=0-4%2C5-9%2C10-14%2C15-19%2C20-24%2C25-29%2C30-34%2C35-39%2C40-44%2C45-49%2C50-54%2C55-59%2C60-64%2C65-69%2C70-74%2C75-79%2C80-84%2C85-89%2C90-94%2C95-99%2C100OV&Tid=*")

df <- dst_dodc1 %>%
  rename(
    Sex = KØN,
    Age = ALDER,
    Date = TID,
    Deaths = INDHOLD
  ) %>%
  mutate(Date = c19dk::DSTdate_to_date(Date)) %>%
  rowwise() %>%
  mutate(
    Age = str_split(Age, " ")[[1]][1],
    Age = ifelse(Age == 100, "100+", Age),
    Sex = ifelse(Sex == "Mænd", "Male", "Female")
  ) %>%
  select(Date, Age, Sex, Deaths) %T>%
  write_csv2("data/tidy_DST_daily_deaths_age_sex_5.csv")

df %>% 
  mutate(Age = case_when(
    Age == "0-4" ~ "0-9",
    Age == "5-9" ~ "0-9",
    Age == "10-14" ~ "10-19",
    Age == "15-19" ~ "10-19",
    Age == "20-24" ~ "20-29",
    Age == "25-29" ~ "20-29",
    Age == "30-34" ~ "30-39",
    Age == "35-39" ~ "30-39",
    Age == "40-44" ~ "40-49",
    Age == "45-49" ~ "40-49",
    Age == "50-54" ~ "50-59",
    Age == "55-59" ~ "50-59",
    Age == "60-64" ~ "60-69",
    Age == "65-69" ~ "60-69",
    Age == "70-74" ~ "70-79",
    Age == "75-79" ~ "70-79",
    Age == "80-84" ~ "80-89",
    Age == "85-89" ~ "80-89",
    Age == "90-94" ~ "90+",
    Age == "95-99" ~ "90+",
    Age == "100+" ~ "90+"
  )) %>% 
  group_by(Date, Age, Sex) %>% 
  summarize(Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  write_csv2("data/tidy_DST_daily_deaths_age_sex_10.csv")
  
