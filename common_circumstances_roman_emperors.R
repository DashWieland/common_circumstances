library(tidyverse)
library(ggthemes)
library(png)
library(grid)

emperors <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv"
  )

rise_and_fall <- emperors %>%
  mutate(rise_fall = as.factor(
    paste(
      "Rose by",
      emperors$rise,
      "and fell because of",
      emperors$cause,
      sep = " "
    )
  )) %>%
  select(name_full, rise_fall) %>%
  group_by(rise_fall) %>%
  summarize(count_circumstances = n()) %>%
  arrange(desc(count_circumstances)) %>%
  head(10)

img <- readPNG("friends_romans_countrymen.png")
emperor_bust <- rasterGrob(img, interpolate = TRUE)

ggplot(rise_and_fall) +
  geom_segment(aes(
    x = reorder(rise_fall,
                count_circumstances),
    xend = rise_fall,
    y = 0,
    yend = count_circumstances),
    color = "#725D68") +
  geom_point(
    aes(x = reorder(rise_fall,
                    count_circumstances),
        y = count_circumstances),
    size = 6,
    shape = 19,
    color = "#725D68"
  ) +
  geom_text(aes(x = reorder(rise_fall,
                            count_circumstances),
                y = count_circumstances,
                label = count_circumstances),
            color = "white", 
            size = 3.5
  ) +
  annotation_custom(
    emperor_bust,
    xmin = -10,
    xmax = Inf,
    ymin = 4,
    ymax = 20
  ) +
  coord_flip() +
  labs(
    title = "Common circumstances: the rise and fall of Roman Emperors",
    subtitle = "Most common combinations of rise to power and death among Roman Emperors (27 BCE - 1405 CE)",
    x = NULL,
    y = NULL) +
  theme(
    plot.subtitle = element_text(family = "Palatino",
                                 size = 10,
                                 face = "italic",
                                 hjust = 2.05),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major = element_line(colour = "lavenderblush4",
                                    size = 0.15),
    panel.grid.minor = element_line(colour = "lavenderblush4",
                                    size = 0.05),
    axis.title = element_text(family = "Palatino"),
    axis.text = element_text(family = "Palatino"),
    plot.title = element_text(family = "Palatino",
                              size = 14,
                              face = "bold",
                              colour = "#725D68",
                              hjust = 2.315,
                              vjust = 0.95),
    plot.background = element_rect(fill = "gray92")
  )

ggsave("common_circumstances.png", width = 8, height = 5, units = "in")


