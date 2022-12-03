# Typeface ----------------------------------------------------------------

quartzFonts(lato = c("Lato-Regular", "Lato-Bold", "Lato-Light", "Lato-BoldItalic"))

# global variables --------------------------------------------------------

standard_caption <- "Kristoffer T. Bæk, covid19danmark.dk, data: SSI"
standard_caption_en <- "Kristoffer T. Bæk, covid19danmark.dk, data: SSI"

# Set gg themes ------------------------------------------------------------

theme_set(c19dk::theme_covid())

facet_theme <-
  theme_covid() +
  theme(
    text = element_text(size = 9),
    plot.margin = margin(1, 1, 0.3, 1, "cm"),
    legend.position = "bottom",
    plot.title = element_text(size = 14, hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 20))
  )