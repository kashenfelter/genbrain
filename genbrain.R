# Load packages ----
library(ggart)
library(tidyverse)

# Make reproducible ----
set.seed(101)

# Parameters ----
n <- 1000000 # number of points
points <- data.frame(x = numeric(10), y = numeric(10), flag = integer(10))
edges <- data.frame(x = numeric(10), y = numeric(10), xend = numeric(10), yend = numeric(10))

# Main loop ----
i <- 1
while (i <= n) {
  embed <- 0
  while (embed < 1) {
    point <- sample_n(points[seq(1:i-1), ] %>% filter(flag < 10), 1) # Pick a point at random
    alpha <- runif(1, -2 * pi, 2 * pi) # Pick a random direction
    v <- c(cos(alpha), sin(alpha)) # Create directional vector
    xj <- point$x[1] + v[1] * 2
    yj <- point$y[1] + v[2] * 2
    df_ring <- points %>% mutate(d = sqrt((xj - x)^2 + (yj - y)^2))
    if (min(df_ring$d) >= 2) {
      points[i, ] <- c(xj, yj, 0)
      edges[i, ] <- c(xj, yj, point$x[1], point$y[1])
      embed <- 1
    } else {
      points$flag[points$x == point$x[1] & points$y == point$y[1]] <- 
        points$flag[points$x == point$x[1] & points$y == point$y[1]] + 1
    }
  }
  i <- i + 1
  print(i)
}

# Make plot ----
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend),
               edges %>% filter(x != xend), lineend = "round") +
  coord_equal() +
  theme_blankcanvas()

#Save plot ----
ggsave("circlegrowth-24.png", width = 584, height = 831, units = "mm", dpi = 300)
# saveRDS(edges, "edges.RDS")
# edges <- readRDS("edges.RDS")
