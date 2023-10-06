library(tidyverse)

tibble(
  x = rnorm(1e4),
  y = rnorm(1e4)
) |> 
  ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.05)


## Exercise 1

penguins |>
  filter(!is.na(sex)) |>
  mutate(
    sex = factor(sex, levels = c("male", "female"))
  ) |>
  ggplot(
    aes(x=body_mass_g, fill=species)
  ) +
    geom_density(alpha=0.5, color = NA) +
    facet_grid(sex~.) + 
    labs(
      x = "Body mass (g)",
      y = "",
      fill = "Species"
    )
 


## Exercise 2

ggplot(
  penguins,
  aes(x = flipper_length_mm, y = bill_length_mm, color = species)
) +
  geom_point(
    aes(shape = species)
  ) +
  geom_smooth(method="lm", se=FALSE) +
  scale_color_manual(values=c("darkorange", "purple", "cyan4")) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.15)
  )


