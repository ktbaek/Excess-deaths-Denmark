source("code/main_functions.R")
source("code/other_functions.R")

library(tidyverse)

colors_2 <- c("#E69F00", "#56B4E9")

pred_names <- c(
  "2010-19" = "Baseline: 2010-19",
  "2015-19" = "Baseline: 2015-19"
)

death_rates_10a <- read_csv("data/death_rates_10y_age.csv")

preds_ns <- make_prediction(death_rates_10a, "2010-19", 2010, 2019, 1.96, Age) %>%
  bind_rows(make_prediction(death_rates_10a, "2015-19", 2015, 2019, 1.96, Age))

gglayer_2 <- list(
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100), fill = "gray75", alpha = 0.3),
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3),
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70"),
  geom_line(aes(YQ, (Death_rate - fit) / fit * 100), size = .6, color = "red"),
  scale_y_continuous(limits = c(NA, NA), labels = function(x) paste0(x, " %")),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 10, expand = expansion(mult = c(0.025, .025))),
  labs(
    subtitle = "Red lines indicate the difference between observed and expected mortality rates relative to the expected mortality rates.\nResults based on two different reference periods (2010-2019 and 2015-2019) are shown.\nGray bands indicate a 95% prediction interval.",
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
    plot.subtitle = element_text(size = 8, lineheight = 1))
)
)

preds_ns %>%
  unnest(c(predict, excess)) %>%
  filter(
    !(prediction == "2015-19" & Year < 2015),
    Year >= 2010,
    Age %in% c("40-49", "50-59", "60-69")
  ) %>%
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
  ggplot() +
  gglayer_2 +
  labs(title = "Changes in quarterly mortality rates for age groups 70+ in Denmark") 

ggsave("figures/mortality_change_ns_old.png", width = 18, height = 10, units = "cm", dpi = 300)

gglayer_3 <- list(
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 10, expand = expansion(mult = c(0.025, .025))),
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
  scale_color_manual(values = c("black", "red"), labels = c("Baseline", "Observed mortality rate")) +
  gglayer_3 +
  labs(
    caption = "Method described at github.com/ktbaek/Excess-deaths-Denmark"
  ) +
  theme(panel.spacing.y = unit(0.7, "cm"))

ggsave("figures/baseline_ns.png", width = 12, height = 16, units = "cm", dpi = 300)
