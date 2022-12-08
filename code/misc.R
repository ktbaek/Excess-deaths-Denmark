source("code/functions.R")

# make age (10y bins) and sex stratified predictions using 2010-19 and 2015-19 as baselines
death_rates_10 <- read_csv("data/death_rates_10y_age_sex.csv") %>% 
  mutate(YQ = zoo::as.yearqtr(YQ))

preds <- make_prediction(death_rates_10, "2010-19", 2010, 2019, 1.96, Age, Sex) %>%
  bind_rows(make_prediction(death_rates_10, "2015-19", 2015, 2019, 1.96, Age, Sex))

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
  scale_y_continuous(limits = c(NA, NA)),
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 5, expand = expansion(mult = c(0.025, .025))),
  labs(y = "Deaths per 1000"),
  scale_color_manual(guide = "none", values = colors_2),
  facet_theme,
  facet_grid(Age ~ Sex, scales = "fixed"),
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
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit)/sd), fill = "gray70", alpha = 0.35) +
  geom_line(aes(YQ, (fit  - fit) / sd), size = .3) +
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70") +
  geom_line(aes(YQ, z, color = Sex), size = .4) +
  gglayer_1 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  labs(y = "z-score") +
  theme(
    panel.spacing.y = unit(0.7, "cm"),
    plot.margin = margin(0.5, 0.2, 0.3, 0.5, "cm"))

p2 <- preds %>%
  unnest(c(predict, excess)) %>%
  filter(prediction == "2015-19", Year >= 2015) %>%
  ggplot() +
  geom_ribbon(aes(YQ, ymin = (conf_lo - fit) / sd, ymax = (conf_hi - fit)/sd), fill = "gray70", alpha = 0.35) +
  geom_line(aes(YQ, (fit  - fit) / sd), size = .3) +
  geom_vline(xintercept = 2019.88, size = 0.4, color = "gray70") +
  geom_line(aes(YQ, z, color = Sex), size = .4) +
  gglayer_1 +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_color_manual(values = colors_2) +
  theme(
    panel.spacing.y = unit(0.7, "cm"), 
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



                     