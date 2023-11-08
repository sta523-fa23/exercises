library(tidyverse)

## Data setup

set.seed(3212016)
d = data.frame(x = 1:120) |>
  mutate(y = sin(2*pi*x/120) + runif(length(x),-1,1))



## Theoretical CI

l = loess(y ~ x, data=d)
p = predict(l, se=TRUE)

d = d |> mutate(
  pred_y = p$fit,
  pred_y_se = p$se.fit
)


ggplot(d, aes(x,y)) +
  geom_point(color="gray50") +
  geom_ribbon(
    aes(ymin = pred_y - 1.96 * pred_y_se, 
        ymax = pred_y + 1.96 * pred_y_se), 
    fill="red", alpha=0.25
  ) +
  geom_line(aes(y=pred_y)) +
  theme_bw()

## Bootstrap CI

