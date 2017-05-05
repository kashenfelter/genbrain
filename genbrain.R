# Load packages ----
library(ggart)
library(tidyverse)

# Make reproducible ----
set.seed(101)

# Parameters ----
n <- 1000000 # number of points
points <- data.frame(x = numeric(10), y = numeric(10), neighbours = integer(10), times_checked = integer(10))
edges <- data.frame(x = numeric(10), y = numeric(10), xend = numeric(10), yend = numeric(10))

# Main loop ----
i <- 1
while (i <= n) {
  valid <- FALSE
  while (!valid) {
    random_point <- sample_n(points[seq(1:i-1), ] %>%
                               filter(neighbours <= 4, times_checked <= 10) %>%
                               top_n(max(nrow(.) / 2, 10), times_checked),
                             1) # Pick a point at random
    alpha <- runif(1, -2 * pi, 2 * pi) # Pick a random direction
    v <- c(cos(alpha), sin(alpha)) # Create directional vector
    xj <- random_point$x[1] + v[1] * 2
    yj <- random_point$y[1] + v[2] * 2
    points_dist <- points %>% mutate(d = sqrt((xj - x)^2 + (yj - y)^2))
    if (min(points_dist$d) >= 2) {
      points[i, ] <- c(xj, yj, 1, 0)
      edges[i, ] <- c(xj, yj, random_point$x[1], random_point$y[1])
      valid <- TRUE
      points$neighbours[points$x == random_point$x[1] & points$y == random_point$y[1]] <- 
        points$neighbours[points$x == random_point$x[1] & points$y == random_point$y[1]] + 1
    } else {
      points$times_checked[points$x == random_point$x[1] & points$y == random_point$y[1]] <- 
        points$times_checked[points$x == random_point$x[1] & points$y == random_point$y[1]] + 1
    }
  }
  i <- i + 1
  print(i)
}

# Make plot ----
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend),
               edges %>% filter(x != xend), lineend = "round", size = 0.5) +
  coord_equal() +
  theme_blankcanvas() #+
  #theme(plot.margin = unit(c(10, 10, 10, 10), units = "mm")) # top, right, bottom, and left margins

#Save plot ----
ggsave("plots/genbrain004.tiff", width = 63.5, height = 61, units = "cm", dpi = 720)
# saveRDS(edges, "edges-2.RDS")
# edges <- readRDS("edges.RDS")
