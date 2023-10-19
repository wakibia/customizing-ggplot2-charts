library(tidyverse)
library(systemfonts)
library(glue)
library(ggtext)

sysfonts::font_add_google("Fira Sans")

showtext::showtext_auto()


mpg_sum <- mpg %>% 
  ## just use 2008 data
  dplyr::filter(year == 2008) %>%
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    manufacturer = stringr::str_to_title(manufacturer),
    manufacturer = forcats::fct_lump(manufacturer, n = 10)
  ) %>% 
  ## add counts
  dplyr::count(manufacturer, sort = TRUE) %>% 
  mutate(manufacturer = case_when(
    manufacturer == "Dodge" ~ glue("<span style='color:goldenrod1;'>Dodge</span>"),
    manufacturer == "Toyota" ~ glue("<span style='color:mediumpurple1;'>Toyota</span>"),
    manufacturer == "Chevrolet" ~ glue("<span style='color:coral2;'>Chevrolet</span>"),
    TRUE ~ as.character(manufacturer)
  )) |> 
  dplyr::mutate(
    ## order factor levels by number, put "Other" to end
    manufacturer = forcats::fct_rev(forcats::fct_inorder(manufacturer)),
    manufacturer = forcats::fct_relevel(manufacturer, "Other", after = 0),
    ## create percentage labels with .0s
    perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"),
    ## customize label for the first category
    perc = if_else(row_number() == 1, paste(perc, "of all car models"), perc),
    ## add custom colors for top 3 and "Other"
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "mediumpurple1",
      row_number() == 3 ~ "coral2",
      manufacturer == "Other" ~ "gray80",
      ## all others should be gray
      TRUE ~ "gray55"
    )
  )



ggplot(mpg_sum, aes(n, manufacturer, fill = color)) +
  geom_col() +
  ## add text (another option would be stat_summary())
  geom_text(
    aes(label = paste0(perc, "%")), 
    ## adjust posiiton inside
    hjust = 1, nudge_x = -.5,
    ## label formatting
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.01, .01)) +
  ## add custom colors
  scale_fill_identity(guide = "none") +
  ## get rid of all elements except y axis labels
  theme_void() +
  theme(axis.text.y = element_markdown(size = 14, hjust = 1, family = "Fira Sans", face = 'bold'),
        plot.margin = margin(rep(15, 4))
        )

## export the chart
