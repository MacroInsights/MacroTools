---
  output: html_document
editor_options:
  chunk_output_type: console
---

  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, results='hide', echo=FALSE, message=FALSE, warning=FALSE}
# Loading the required libraries
if (!require("pacman")) install.packages("pacman")
devtools::install_github("hrbrmstr/albersusa")
pacman::p_load(  'tidyverse'
                 , 'ggplot2'
                 , 'htmlwidgets'
                 , 'leaflet'
                 ,'devtools'
                 ,'albersusa'
                 ,'quantmod'
                 ,'tmap'
                 ,'cowplot'
                 ,'here'
)

source(here("code","getStateEmployment.R"))

```



```{r}
UR_raw <- getSymbols(Symbols = "UNRATE", src = "FRED", auto.assign = FALSE)
UR <- UR_raw |> tail(1) |> as.numeric()
states_unemployment <- getStateEmployment(1)
states_ur <- states_unemployment |>
  group_by(State) |>
  slice_max(date) |>
  select(name = State,
         unemployment = UnemploymentRate)
```


```{r, results='hide', echo=FALSE, message=FALSE, warning=FALSE}
# Read in the data
states_data <- states_ur |>
  mutate(estimate = unemployment - UR)
states_geo <- usa_sf("lcc") %>%
  filter(!iso_3166_2 %in% c('AS', 'GU', 'MP', 'PR'))
states_map <- states_geo %>% inner_join(states_data)
```


```{r}
tmap_mode("view")
tmap_options(check.and.fix = TRUE)
statesMap <- states_map |> tm_shape() +
  tm_borders(col = "white") +
  tm_text("iso_3166_2", size = .4, auto.placement = TRUE) +
  tm_fill("estimate", title = paste0("Percentage Points from the National ",
                                     UR,"% Unemployment Rate"),style = "fixed",
          breaks = c(-Inf,-1,-.5,0,.5,1,1.5,Inf),
          textNA = "Dunno",
          colorNA = "green",   # <-------- color for NA values
          palette = c("#C77417", "#FDAF54", "#E6C8A5", "#6ECCF9", "#2A9BCE", "#136598","#0F3562"),
          midpoint = NA,
          legend.is.portrait = FALSE) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = ("top"),
    legend.outside.size = .15,
    legend.text.size = .5,
    legend.text.fontface = "plain",
    legend.title.fontface = "italic",
    legend.width = .5,
    legend.height = .5,
    legend.stack = "horizontal",
    bg.color = "white",
    legend.title.size=1,
    main.title = "Unemployment Rate Differential",
    fontface = "bold",
    fontfamily = "serif"
  )
tmap_save(statesMap, "states_map.png", width=6, height=4, dpi = 600)
```

```{r}
states_map$estimate_cut <- cut(states_map$estimate,
                               breaks = c(-Inf,-1,-.5,0,.5,1,1.5,Inf),
                               right = FALSE)
threshold_labels <- c("Less than -1.0", "-1.0 to -0.5", "-0.5 to 0.0",
                      "0.0 to 0.5", "0.5 to 1.0", "1.0 to 1.5", "1.5 or more")

ggplot(data =  states_map) +
  geom_sf(aes(fill = estimate_cut), color = "white", lwd = 1.5) +
  geom_sf_text(aes(label = iso_3166_2),
               colour = ifelse(states_map$estimate_cut == "[0.5,1)" |
                                 states_map$estimate_cut == "[1,1.5)" |
                                 states_map$estimate_cut == "[1.5, Inf)", "white", "black")) +

  scale_fill_manual(values = c("#C77417", "#FDAF54", "#E6C8A5",
                               "#6ECCF9", "#2A9BCE", "#136598","#0F3562"),
                    labels = threshold_labels,
                    guide = guide_legend(
                      label.position = "bottom",
                      nrow = 1)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "top",           # Moves the legend to the top
        legend.direction = "horizontal",   # Makes the legend horizontal
        legend.box = "horizontal",         # Aligns legend items horizontally
        legend.title = element_blank(),    # Removes the legend title
        legend.text.align = 0.5 ,
        legend.key.width = unit(2, "cm"),    # Adjusts the width of legend keys
        legend.spacing.x = unit(0, "cm") )
```
