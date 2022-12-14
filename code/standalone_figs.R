library(ggtext)
source("code/main_functions.R")
source("code/other_functions.R")

library(tidyverse)

theme_set(c19dk::theme_covid())

facet_theme <-
  theme(
    text = element_text(size = 7),
    plot.margin = margin(0.5, 0.5, 0.3, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 12, hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(size = 8, margin = margin(t = 0, r = 0, b = 0, l = 20))
  )

quartzFonts(lato = c("Lato-Regular", "Lato-Bold", "Lato-Light", "Lato-BoldItalic"))


pred_names <- c(
  "2010-19" = "Baseline: 2010-19",
  "2015-19" = "Baseline: 2015-19"
)

death_rates_10a <- read_csv("data/death_rates_10y_age.csv")

preds_ns <- make_prediction(death_rates_10a, "2010-19", 2010, 2019, 1.96, Age) %>%
  bind_rows(make_prediction(death_rates_10a, "2015-19", 2015, 2019, 1.96, Age))

ssi_deaths <- read_csv2("data/SSI_death_age.csv")

covid_deaths <- ssi_deaths %>% 
  pivot_longer(-Age, names_to = c(NA, "Quarter_end"), values_to = "C_deaths", names_sep = "_") %>% 
  mutate(
    Quarter_end = lubridate::ymd(Quarter_end),
    Quarter = lubridate::quarter(Quarter_end) - lubridate::quarter(1),
    Year = lubridate::year(Quarter_end),
    Year = ifelse(Quarter == 0, Year - 1, Year),
    Quarter = ifelse(Quarter == 0, 4, Quarter),
    YQ = zoo::as.yearqtr(format(paste0(Year, Quarter)), "%Y%q")
  ) %>% 
  group_by(Age) %>% 
  mutate(
    Covid = C_deaths - lead(C_deaths)
  ) %>% 
  filter(!is.na(Covid)) %>% 
  select(Age, YQ, Covid)


gglayer_2 <- list(
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100), fill = "gray75", alpha = 0.3),
  geom_ribbon(aes(YQ, ymin = ((Death_rate  - covid_rate) / fit - 1)  * 100, ymax = (Death_rate - fit) / fit * 100), fill = "#00E676", alpha = 0.7),
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3),
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70"),
  geom_vline(xintercept = 2021.00, size = 0.4, color = "#00E676"),
  geom_line(aes(YQ, (Death_rate - fit) / fit * 100), size = .6, color = "#FF5722"),
  scale_y_continuous(limits = c(NA, NA), labels = function(x) paste0(x, " %")),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 10, expand = expansion(mult = c(0.025, .015))),
  labs(
    subtitle = "<span style = 'color:#FF5722;'>**Red lines**</span> indicate the difference between observed and expected mortality rates relative to the expected mortality rates.<br><span style = 'color:#40CA74;'>**Green bands**</span> indicate deaths with a positive SARS-CoV-2 PCR (data available from 2021 Q1 [green vertical line]).<br><span style = 'color:#9F9F9F;'>**Gray bands**</span> indicate a 95% prediction interval.<br>Results based on two different reference periods (2010-2019 and 2015-2019) are shown.",
    caption = "Method described at github.com/ktbaek/Excess-deaths-Denmark",
    y = "Change relative to baseline"),
  facet_theme,
  facet_grid(Age ~ prediction, scales = "free_y", labeller = labeller(prediction = pred_names)),
  theme(
    plot.background = element_rect(color = "white"),
    axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 3, l = 0), angle = 45, hjust = 1, vjust = 1, size = rel(1)),
    panel.grid.minor.x = element_line(size = 0.1),
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.2, 0.5, "cm"),
    plot.title = element_text(size = 10), 
    plot.caption = element_text(size = 8), 
    plot.subtitle = element_textbox_simple(size = 8, lineheight = 1.1, padding = margin(0,0,5,0)))
)


preds_ns %>%
  unnest(c(predict, excess)) %>%
  filter(
    !(prediction == "2015-19" & Year < 2015),
    Year >= 2010,
    Age %in% c("40-49", "50-59", "60-69")
  ) %>%
  left_join(covid_deaths, by = c("Age", "YQ")) %>% 
  mutate(covid_rate = Covid / Population) %>% 
  ggplot() +
  gglayer_2 +
  labs(title = "Changes in quarterly mortality rates for age groups 40-69 in Denmark") 


ggsave("figures/mortality_change_ns_middle.png", width = 18, height = 10, units = "cm", dpi = 300)

preds_ns %>%
  unnest(c(predict, excess)) %>%
  filter(
    !(prediction == "2015-19" & Year < 2015),
    Year >= 2010,
    Age %in% c("0-9", "10-19", "20-29", "30-39")
  ) %>%
  left_join(covid_deaths, by = c("Age", "YQ")) %>% 
  mutate(covid_rate = Covid / Population) %>% 
  ggplot() +
  gglayer_2 +
  labs(title = "Changes in quarterly mortality rates for age groups 0-39 in Denmark") 


ggsave("figures/mortality_change_ns_young.png", width = 18, height = 12, units = "cm", dpi = 300)

preds_ns %>%
  unnest(c(predict, excess)) %>%
  filter(
    !(prediction == "2015-19" & Year < 2015),
    Year >= 2010,
    Age %in% c("70-79", "80-89", "90+")
  ) %>%
  left_join(covid_deaths, by = c("Age", "YQ")) %>% 
  mutate(covid_rate = Covid / Population) %>% 
  ggplot() +
  gglayer_2 +
  labs(title = "Changes in quarterly mortality rates for age groups 70+ in Denmark") 

ggsave("figures/mortality_change_ns_old.png", width = 18, height = 10, units = "cm", dpi = 300)

gglayer_3 <- list(
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 10, expand = expansion(mult = c(0.025, .015))),
  labs(y = "Deaths per 1000"),
  facet_theme,
  facet_grid(Age ~ prediction, scales = "free_y", labeller = labeller(prediction = pred_names)),
  theme(
    axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 2, l = 0), angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    axis.text.y = element_text(size = rel(0.6)),
    panel.grid.minor.x = element_line(size = 0.1),
    legend.text = element_text(size = 8),
    plot.background = element_rect(color = "white"),
    plot.title = element_text(size = 10), 
    plot.caption = element_text(size = 8), 
    plot.subtitle = element_text(size = 8, lineheight = 1))
  )


preds_ns %>%
  unnest(c(predict, excess)) %>%
  filter(
    !(prediction == "2015-19" & Year < 2015),
    Year >= 2010
  ) %>% 
  rename(z_death_rate = Death_rate) %>% 
  pivot_longer(c(z_death_rate, fit), names_to = "type", values_to = "rate") %>% 
  ggplot() +
  geom_ribbon(aes(YQ, ymin = conf_lo * 1000, ymax = conf_hi * 1000), fill = "gray70", alpha = 0.3) +
  geom_line(aes(YQ, rate * 1000, color = type), size = .3) +
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70") +
  scale_color_manual(values = c("black", "#FF5722"), labels = c("Baseline", "Observed mortality rate")) +
  gglayer_3 +
  labs(
    caption = "Method described at github.com/ktbaek/Excess-deaths-Denmark"
  ) +
  theme(panel.spacing.y = unit(0.7, "cm"))

ggsave("figures/baseline_ns.png", width = 12, height = 16, units = "cm", dpi = 300)



SSI_covid_resolved <- read_csv2("data/Deaths_o_weeks_covid_cause.csv") %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(-Udfald, names_to = "week_year", values_to = "value") %>% 
  mutate(
    value = ifelse(str_detect(value, "<"), NA, value), # replace <20 with NA
    value = as.integer(value),
    Year = as.integer(str_sub(week_year, 5, 8)),
    Week = as.integer(str_sub(week_year, 2, 3)),
    Date = c19dk::week_to_date(Year, Week, day = 4), # Date for the Thursday in each week
    Quarter = lubridate::quarter(Date), # quarter of the Thursday in each week. The quarterly values are thereby not completely precise
    YQ = zoo::as.yearqtr(format(paste0(Year, Quarter)), "%Y%q"),
    Udfald = case_when(
      Udfald == "D??dstal 30 dage efter infektion (PCR)" ~ "Deaths 30 days after positive PCR",
      Udfald == "D??d af covid (DAR)" ~ "Died due to Covid",
      Udfald == "Endnu ikke valideret" ~ "Pending",
      Udfald == "Afventer d??dsattest" ~ "Pending",
      Udfald == "D??d med covid (DAR)" ~ "Died with Covid")
  ) %>% 
  group_by(across(-value)) %>% 
  summarize(value = sum(value, na.rm = TRUE))
  



SSI_covid_resolved_q <- SSI_covid_resolved %>% 
  group_by(YQ) %>% 
  mutate(na_weeks = sum(is.na(value)) / 5) %>% # because 5 categories
  group_by(YQ, Udfald) %>% 
  summarize(
    na_weeks = mean(na_weeks),
    value = sum(value, na.rm = TRUE)
    )
  
# compare with the numbers from Deaths_over_time
read_csv2("data/Deaths_over_time.csv") %>% 
  mutate(
    Date = lubridate::ymd(Dato),
    Year = lubridate::year(Date),
    Quarter = lubridate::quarter(Date),
    YQ = zoo::as.yearqtr(format(paste0(Year, Quarter)), "%Y%q")
  ) %>% 
  filter(Dato != "I alt") %>% 
  group_by(YQ) %>% 
  summarize(Deaths_pcr = sum(Antal_d??de, na.rm = TRUE))

# reorder categories
SSI_covid_resolved_q$Udfald <- krisr::new_order(
  vector = SSI_covid_resolved_q$Udfald, order = c("Deaths 30 days after positive PCR",
                                                "Died with Covid",
                                                "Pending",
                                                "Died due to Covid")
) 

SSI_covid_resolved_q %>% 
  ggplot() +
  geom_bar(data = function(x) subset(x, Udfald != "Deaths 30 days after positive PCR"), stat = "identity", position = "stack",aes(YQ, value, fill = Udfald))+
  zoo::scale_x_yearqtr(format = "%Y Q%q", n = 5, expand = expansion(mult = c(0.025, .025))) +
  #scale_fill_manual(values = c(scales::hue_pal()(2)[1], "gray80", scales::hue_pal()(2)[2])) +
  scale_fill_manual(values = c("#FF6E40", "gray80", "#40C4FF")) +
  geom_label(
    aes(
      x = YQ, 
      y = -100, 
      label = na_weeks
    ),
    size = 2.3,
    color = "gray60",
    fill = "white",
    label.size = NA,
    label.padding = unit(0.1, "lines"),
    check_overlap = TRUE) +
  facet_theme + 
  labs(y = "Deaths",
       title = "Deaths with positive SARS-CoV-2 PCR by cause using death certificates",
       subtitle = "Weeks with <20 deaths are counted as zero and are unresolved for cause of death.\nThe number of unresolved weeks is indicated below the bars.\nQuarters with zero deaths in this chart had <150 deaths with positive PCR.",
       caption = "Code and data at github.com/ktbaek/Excess-deaths-Denmark, data source: covid19.ssi.dk")  +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), size = 7),
        axis.text.y = element_text(size = 7),
        plot.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(size = 10))

ggsave("figures/ssi_covid_deaths.png", width = 18, height = 10, units = "cm", dpi = 300)
  
pandemix <- read_csv2("data/Death_cause_pandemix.csv") %>% 
  pivot_longer(-Week, names_to = "Udfald", values_to = "PandemiX")


x <- SSI_covid_resolved %>% 
  filter(Year == 2022) %>% 
  left_join(pandemix, by = c("Week", "Udfald")) %>% 
  rename(DAR = value) %>% 
  pivot_longer(c(DAR, PandemiX), names_to = "Model", values_to = "value") %>% 
  filter(
    Udfald != "Deaths 30 days after positive PCR",
    Week >= 30)

# reorder categories
x$Udfald <- krisr::new_order(
  vector = x$Udfald, order = c(
                                                "Died with Covid",
                                                "Pending",
                                                "Died due to Covid")
) 

x %>% 
  ggplot() +
  geom_area(aes(Week, value, fill = Udfald)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("#FF6E40", "gray80", "#40C4FF")) +
  facet_theme +
  facet_wrap(~Model) +
  labs(y = "Deaths", x = "Week of 2022") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_text(size = 7, face = "bold"),
    strip.text.x = element_text(size = 7))

ggsave("figures/dar_pandemix.png", width = 12, height = 6.7, units = "cm", dpi = 300)

colors_9 <- c( "#FF6E40",  "#40C4FF", "#00E676", "#FFD740", "gray70", "#BA68C8", "#FFAB40",  "#0288D1", "#26A69A")

colors_9_light <- colorspace::lighten(colors_9, 0.4)

scales::show_col(c(rbind(colors_9_light, colors_9)))

  