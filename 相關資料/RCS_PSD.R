library(tidyverse)

# Panel data (same people over time)
panel <- expand.grid(
  id = paste0("P", 1:5),
  time = 1:3
) %>%
  mutate(type = "Panel")

# Repeated cross-section (different people each wave)
cross <- tibble(
  id = paste0("C", 1:15),
  time = rep(1:3, each = 5),
  type = "Repeated Cross-section"
)

data <- bind_rows(panel, cross)

ggplot(data, aes(x = time, y = id)) +
  geom_point(size = 3) +
  geom_line(data = subset(data, type == "Panel"),
            aes(group = id),
            linewidth = 0.8) +
  facet_wrap(~type, scales = "free_y") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Wave 1", "Wave 2", "Wave 3")) +
  labs(x = "Survey Wave",
       y = "Respondents",
       title = "Repeated Cross-Section vs Panel Survey Design") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
  
  
library(tidyverse)

set.seed(123)

N <- 300
T <- 6

tscs <- expand.grid(
  unit = 1:N,
  time = 1:T
) %>%
  mutate(
    X = rnorm(n()),
    Y = 0.5 * X + rnorm(n())
  )
  
ggplot(tscs, aes(x = time, y = unit)) +
  geom_point(aes(size = X, color = Y), alpha = 0.7) +
  scale_x_continuous(
    breaks = 1:T,
    labels = paste0("t", 1:T)
  ) +
  scale_color_viridis_c() +
  labs(
    x = "Time (T = 6)",
    y = "Units (N = 300)",
    title = "TSCS Data Structure",
    subtitle = "Each unit-time observation contains X (size) and Y (color)"
  ) +
  theme_minimal(base_size = 14)
  
ggplot(tscs %>% filter(unit <= 30),
       aes(x = time, y = unit, group = unit)) +
  geom_line(alpha = .4) +
  geom_point(aes(color = Y, size = X)) +
  scale_color_viridis_c() +
  labs(
    x = "Time",
    y = "Units",
    title = "Time-Series Cross-Sectional Structure",
    subtitle = "Example subset of units"
  ) +
  theme_classic(base_size = 14)
  
ggsave('PSDonly.png', dpi = 300)