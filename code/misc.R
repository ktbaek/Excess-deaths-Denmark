source("code/main_functions.R")
source("code/other_functions.R")

library(tidyverse)


# make age (10y bins) and sex stratified predictions using 2010-19 and 2015-19 as baselines
death_rates_10 <- read_csv("data/death_rates_10y_age_sex.csv") %>% 
  mutate(YQ = zoo::as.yearqtr(YQ))

preds <- make_prediction(death_rates_10, "2010-19", 2010, 2019, 1.64, Age, Sex) %>%
  bind_rows(make_prediction(death_rates_10, "2015-19", 2015, 2019, 1.64, Age, Sex))

preds %>% 
  unnest(c(predict, excess)) %>% 
  filter(
    prediction == "2015-19",
    Year >= 2015,
    Year <= 2019
  ) %>% 
  ggplot() +
  geom_qq(aes(sample = excess_rate_sab)) +
  geom_qq_line(aes(sample = excess_rate_sab)) +
  facet_grid(Age~Sex, scales = "free_y")

pop_total <- preds %>%
  unnest(predict) %>%
  filter(prediction == "2010-19") %>%
  group_by(YQ) %>%
  summarize(Pop_total = sum(Population))

preds %>%
  unnest(c(predict, excess)) %>%
  left_join(pop_total, by = "YQ") %>%
  group_by(Year, prediction) %>%
  summarize(
    excess_deaths = sum(excess_abs),
    Pop_total = mean(Pop_total, na.rm = TRUE)
  ) %>%
  mutate(excess_deaths = ifelse(Year < as.numeric(str_sub(prediction, 1, 4)), NA, excess_deaths )) %>% 
  ggplot() +
  geom_bar(stat = "identity", position = "stack", aes(Year, excess_deaths / Pop_total * 100000), width = 0.9) +
  scale_y_continuous(limits = c(NA, NA), labels = scales::number) +
  labs(
    y = "Excess deaths per 100,000"
  ) +
  facet_wrap(~prediction) +
  theme(
    axis.text.x = element_text(size = rel(1.1)),
    axis.text.y = element_text(size = rel(1.1)),
    strip.text.x = element_text(size = rel(1.1)),
    axis.title.y = element_text(size = rel(1.2)),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(size = 8)
  )

gglayer_1 <- list(
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit)/sd), fill = "gray70", alpha = 0.35),
  geom_line(aes(YQ, (fit  - fit) / sd), size = .3),
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70"),
  geom_line(aes(YQ, z, color = Sex), size = .4),
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 5, expand = expansion(mult = c(0.025, .025))),
  labs(y = "z-score"),
  scale_color_manual(guide = "none", values = colors_2),
  facet_theme,
  facet_grid(Age ~ prediction, scales = "fixed"),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    panel.grid.minor.x = element_line(size = 0.1),
    legend.text = element_text(size = 8)
  )
)

p1 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2010-19", Year >= 2010) %>%
  ggplot() +
  gglayer_1 +
  theme(
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"))

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2015-19", Year >= 2015) %>%
  ggplot() +
  theme(
    axis.title.y = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.3, 0.2, "cm"))

p1 + p2 + plot_annotation(tag_levels = "A") & theme(legend.position = "none")

p1 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2010-19", Year >= 2020) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit) / sd, fill = Sex), alpha = 0.15) +
  geom_line(aes(YQ, (fit  - fit)/ sd), size = .3) +
  geom_line(aes(YQ, z, color = Sex), size = .7) +
  gglayer_1 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  scale_fill_manual(guide = "none", values = colors_2) +
  facet_wrap(~ Age, ncol = 5, scales = "fixed") +
  labs(y = "z-score")

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2015-19", Year >= 2020) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit) / sd, fill = Sex), alpha = 0.15) +
  geom_line(aes(YQ, (fit  - fit)/ sd), size = .3) +
  geom_line(aes(YQ, z, color = Sex), size = .7) +
  gglayer_1 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  scale_fill_manual(guide = "none", values = colors_2) +
  facet_wrap(~ Age, ncol = 5, scales = "fixed") +
  labs(y = "z-score") +
  theme(plot.margin = margin(0.3, 0.5, 0.3, 0.5, "cm"))

p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

gglayer_2 <- list(
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit)/sd), fill = "gray70", alpha = 0.30),
  geom_line(aes(YQ, (fit  - fit) / sd), size = .3),
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70"),
  geom_line(aes(YQ, z, color = Sex), size = .4),
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 12, expand = expansion(mult = c(0.025, .025))),
  labs(y = "z-score"),
  scale_color_manual(values = colors_2),
  facet_theme,
  facet_grid(Age ~ prediction, scales = "free_x"),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    panel.grid.minor.x = element_line(size = 0.1),
    legend.text = element_text(size = 8)
  )
)

preds %>%
  unnest(c(predict, excess)) %>%
  filter(!(prediction == "2015-19" & Year < 2015)) %>%
  ggplot() +
  gglayer_2 

preds_more <- make_more_preds(death_rates_10, 1.64, Age, Sex)

preds_more %>%
  unnest(predict) %>%
  filter(
    prediction == "2010-19",
    Year >= 2010,
    Year <= 2019
  ) %>%
  left_join(unnest(preds_more, qstats), by = c("Age", "Sex", "Quarter", "prediction")) %>%
  ggplot() +
  geom_ribbon(aes(x = Quarter, ymin = conf_lo_Q * 100, ymax = conf_hi_Q * 100), fill = "gray70", alpha = 0.3) +
  geom_line(aes(Quarter, .resid / .fitted * 100, color = as.factor(Year)), size = 0.15) +
  geom_pointrange(aes(x = Quarter, y = mean_Q * 100, ymin = 100 * (mean_Q - sd_Q), ymax = 100 * (mean_Q + sd_Q)), size = 0.07) +
  
  scale_y_continuous(limits = c(NA, NA), labels = function(x) paste0(x, " %")) +
  labs(
    y = "Deviation from baseline (%)",
    x = "Quarter"
  ) +
  scale_color_manual(values = colors_16) +
  facet_theme +
  facet_wrap(~Sex + Age, ncol = 10, scales = "free_y") +
  theme(
    axis.title.x = element_text(face = "bold", size = 8),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(size = 8),
    panel.spacing.y = unit(0.5, "cm"),
    legend.position = "none"
  )
   
pop_total <- preds %>%
  unnest(predict) %>%
  filter(prediction == "2010-19") %>%
  group_by(YQ) %>%
  summarize(Pop_total = sum(Population))

preds %>%
  unnest(c(predict, excess)) %>%
  left_join(pop_total, by = "YQ") %>%
  filter(Year >= 2020) %>%
  group_by(prediction) %>%
  summarize(
    excess_deaths = sum(excess_abs),
    Pop_total = mean(Pop_total, na.rm = TRUE)
  ) %>% 
  mutate(rate = excess_deaths / Pop_total * 100000)
    
gglayer_fig7 <- list(
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 5, expand = expansion(mult = c(0.025, .025))),
  labs(y = "Deaths per 1000"),
  scale_color_manual(values = colors_2),
  scale_fill_manual(values = colors_2),
  facet_theme,
  facet_wrap( ~ Age, ncol = 10, scales = "fixed"),#, labeller=function(x) {x[2]}),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    panel.grid.minor.x = element_line(size = 0.1),
    legend.text = element_text(size = 8)
  )
)

p1 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2010-19", Year >= 2020) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100, fill = Sex), alpha = 0.2) +
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3) +
  geom_line(aes(YQ, (Death_rate - fit) / fit* 100, color = Sex), size = .4) +
  gglayer_fig7 +
  labs(y = expression(bold(Delta*"death rate (per 1000)"))) +
  theme(
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"))

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2015-19", Year >= 2020) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100, fill = Sex), alpha = 0.2) +
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3) +
  geom_line(aes(YQ, (Death_rate - fit) / fit* 100, color = Sex), size = .4) +
  gglayer_fig7 +
  labs(y = expression(bold(Delta*"death rate (per 1000)"))) +
  theme(
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"))

p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "none")

gglayer_fig4 <- list(
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 5, expand = expansion(mult = c(0.025, .025))),
  labs(y = "Deaths per 1000"),
  scale_color_manual(guide = "none", values = colors_2),
  facet_theme,
  facet_grid(Age ~ Sex, scales = "free_y"),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    panel.grid.minor.x = element_line(size = 0.1),
    legend.text = element_text(size = 8)
  )
)

p1 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2010-19", Year >= 2010) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100), fill = "gray70", alpha = 0.35) +
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3) +
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70") +
  geom_line(aes(YQ, (Death_rate - fit) / fit * 100, color = Sex), size = .4) +
  gglayer_fig4 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  labs(y = "Deviation from baseline (%)") +
  theme(
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"))

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2015-19", Year >= 2015) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100), fill = "gray70", alpha = 0.35) +
  geom_line(aes(YQ, (fit  - fit) / fit * 100), size = .3) +
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70") +
  geom_line(aes(YQ, (Death_rate - fit) / fit * 100, color = Sex), size = .4) +
  gglayer_fig4 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  theme(
    panel.spacing.y = unit(0.7, "cm"), 
    axis.title.y = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.3, 0.2, "cm"))

p1 + p2 + plot_annotation(tag_levels = "A") & theme(legend.position = "none")


color_scale <- c("white", "#56B4E9")

death_rates_10a <- read_csv("data/death_rates_10y_age.csv")
preds_10_no_sex <- make_more_preds(death_rates_10a, 1.64, Age)

death_rates_5 <- read_csv("data/death_rates_5y_age_sex.csv")

death_rates_5$Age <- factor(death_rates_5$Age, levels = c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
  "80-84", "85-89", "90-94", "95+"
))

preds_5 <- make_more_preds(death_rates_5, 1.64, Age, Sex)


  
gglayer_tile <- list(  
  zoo::scale_x_yearqtr(format = "%YQ%q", n =3, expand = expansion(mult = c(0.025, .025))),
  scale_y_discrete(position = "left"),
  facet_grid(prediction~Sex),
  labs(y = ""),
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"),
    legend.position = "bottom")
)

p1 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction %in% c("2010-19", "2015-19"), Year >= 2015) %>%
  ggplot() +
  geom_tile(aes(YQ, fct_rev(Age), fill = excess_abs)) +
  scale_fill_gradient2(name = "Excess deaths", low = colorspace::lighten(colors_2[2], 0.1), mid = "gray98", high =  colorspace::darken(colors_2[1], 0.1), midpoint = 0, limits = c(NA, NA), na.value = colorspace::lighten(colors_2[2], 0.9), breaks = c(0, 300)) +
  gglayer_tile

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction %in% c("2010-19", "2015-19"), Year >= 2015) %>%
  ggplot() +
  geom_tile(aes(YQ, fct_rev(Age), fill = excess_rate / fit * 100)) +
  scale_fill_gradient2(name = "Excess deaths (%)", low = colorspace::lighten(colors_2[2], 0.1), mid = "gray98", high =  colorspace::darken(colors_2[1], 0.1), midpoint = 0, limits = c(NA, NA), na.value = colorspace::lighten(colors_2[2], 0.9)) +
  gglayer_tile

p3 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction %in% c("2010-19", "2015-19"), Year >= 2015) %>%
  ggplot() +
  geom_tile(aes(YQ, fct_rev(Age), fill = z)) +
  scale_fill_gradient2(name = "z-score", low = colorspace::lighten(colors_2[2], 0.1), mid = "gray98", high =  colorspace::darken(colors_2[1], 0.1), midpoint = 0, limits = c(NA, NA), na.value = colorspace::lighten(colors_2[2], 0.9)) +
  gglayer_tile

p1 + p2 + p3



 preds_more %>% 
   unnest(c(predict, excess)) %>% 
   filter(Year >= 2020) %>% 
   ggplot() +
   geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / fit * 100, ymax = (conf_hi - fit) / fit * 100, fill = Sex), alpha = 0.2) +
   geom_hline(yintercept = 0) +
   geom_line(aes(YQ, res / fit * 100, color = Sex)) + 
   scale_color_manual(values = colors_2) +
   scale_fill_manual(values = colors_2) +
   zoo::scale_x_yearqtr(format = "%YQ%q", n = 3, expand = expansion(mult = c(0.025, .025))) +
   scale_y_continuous(limits = c(NA, NA), labels = function(x) paste0(x, " %")) +
   labs(y = "% change") +
   facet_theme +
   facet_grid(Age~ prediction, scales = "free_y") +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
     legend.title = element_blank()
   )
 
 preds_more %>% 
   unnest(c(predict, excess)) %>% 
   filter(Year >= 2020) %>% 
   ggplot() +
   geom_ribbon(aes(YQ, ymin = -1.64, ymax = 1.64), fill = "gray75", alpha = 0.35) +
   geom_hline(yintercept = 0) +
   geom_line(aes(YQ, z, color = Sex)) + 
   scale_color_manual(values = colors_2) +
   zoo::scale_x_yearqtr(format = "%YQ%q", n = 3, expand = expansion(mult = c(0.025, .025))) +
   labs(y = "z-score") +
   facet_theme +
   facet_grid(Age~ prediction, scales = "fixed") +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
     legend.title = element_blank()
   )