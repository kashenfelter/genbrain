# Load packages ----
library(ggart)
library(tidyverse)
library(tweenr)

# Make reproducible ----
set.seed(1000)

# Parameters ----
n <- 10000000000000000 # number of points
r <- 75 # circle radius
k <- 100 # max neighbours
X <- 10000 # canvas width
Y <- 10000 # canvas height
rectilinear <- FALSE
alpha <- 0

max_checked <- 10 # maximum times checked

# Setup
points <- data.frame(x = numeric(10), y = numeric(10), neighbours = integer(10), times_checked = integer(10))
edges <- data.frame(x = numeric(10), y = numeric(10), xend = numeric(10), yend = numeric(10))
#points <- points %>% rowwise() %>% mutate(x = runif(1, -X, X), y = runif(1, -Y, Y))
# Main loop ----
i <- 11
while (i <= n) {
  valid <- FALSE
  while (!valid) {
    random_point <- sample_n(points[seq(1:i-1), ] %>%
                               filter(neighbours <= k, times_checked <= max_checked) %>%
                               top_n(max(floor(nrow(.) / 2), 10), times_checked), 1) # Pick a point at random
    if(rectilinear) {
      alpha <- sample(c(0, pi/2, pi, 3*pi/2), 1)
    } else {
      #alpha <- runif(1, -2 * pi, 2 * pi) # Pick a random direction
      #alpha <- alpha + 30 * pi / 180
      alpha <- sample(c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4), 1)
      #alpha <- sample(c(0, 3*pi/4, pi), 1)
    }
    v <- c(cos(alpha), sin(alpha)) * r # Create directional vector
    xj <- random_point$x[1] + v[1]
    yj <- random_point$y[1] + v[2]
    if(xj < -X/2 | xj > X/2 | yj < -Y/2 | yj > Y/2) {
      next
    }
    points_dist <- points %>% mutate(d = sqrt((xj - x)^2 + (yj - y)^2))
    if (min(points_dist$d) >= 1 * r) {
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

delta <- 100
edges2 <- edges %>%
  rowwise() %>%
  mutate(#x = x + runif(1, -delta, delta),
         y = y + runif(1, -delta, delta),
         #xend = xend + runif(1, -delta, delta),
         yend = yend + runif(1, -delta, delta))

edges3 <- edges %>%
  rowwise() %>%
  mutate(x = x + runif(1, -delta, delta),
    #y = y + runif(1, -delta, delta),
    xend = xend + runif(1, -delta, delta))
    #yend = yend + runif(1, -delta, delta))

df <- list(edges, edges2)

tf <- tween_states(df, tweenlength = 2, statelength = 1,
                   ease = "exponential-out",
                   nframes = 100)

# Make plot ----
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend),
               edges, lineend = "round", size = 1, alpha = 1) +
               #tf, lineend = "round", size = 0.15, alpha = 0.15) +
  coord_equal() +
  xlim(-5000, 5000) +
  ylim(-5000, 5000) +
  theme_blankcanvas(margin_cm = 0)

#Save plot ----
ggsave("plots/plot018.png", width = 20, height = 20, units = "in", dpi = 300)
# saveRDS(edges, "edges-rectilinear.RDS")
# edges <- readRDS("edges-rectilinear.RDS")
# edges <- readRDS("edges-3.RDS")
