library(tidyverse)
library(broom)
library(gganimate)
library(magick)

df <- tibble(x = unique(round(rnorm(50), 1)), 
             y = x + rnorm(length(x)))
fit <- lm(y ~ x, data = df)

b0 <- coef(fit)[1]
m0 <- coef(fit)[2]


par_df <- crossing(b = seq(b0 - 1, b0 + 2, length.out = 10),
                   m = seq(m0 - 1, b0 + 2, length.out = 10)) %>%  
  arrange(b, m) %>%
  mutate(id = 1:nrow(.)) %>%
  split(.$id) %>%
  map(~ tibble(x = df$x, y = df$y, .fitted = .$m*x + .$b, id = .$id, b = .$b, m = .$m) %>%
        mutate(.resid = y - .fitted)) %>%
  bind_rows() %>%
  glimpse()

rms_df <- par_df %>%
  group_by(id, b, m) %>%
  summarize(rms_error = sqrt(mean(.resid^2))) %>%
  glimpse()

rms_df_tall <- rms_df %>%
  rename(`Slope` = m, `Intercept` = b, `R.M.S. Error` = rms_error) %>% 
  pivot_longer(cols = c(`Intercept`, `Slope`, `R.M.S. Error`)) %>%
  mutate(name = factor(name, levels = c("Intercept", "Slope", "R.M.S. Error"))) %>%
  group_by(name) %>%
  mutate(value_lag = lag(value)) %>%
  glimpse()

# gif pars
nframes <- 100  # default 100
duration <- 30
scale <- 2.5
width <- 400*scale
height <- 300*scale


gg1 <- ggplot(par_df) +
  geom_segment(aes(x = x, xend = x, y = y, yend = .fitted), color = "red", size = 0.5) +
  geom_point(aes(x = x, y = y), size = 2) +
  geom_line(aes(x = x, y = .fitted)) +
  transition_states(id, transition_length = 0) +
  shadow_wake(.1, exclude_layer = c(1, 2)) + 
  theme_minimal() + 
  labs(title = "Scatterplot, Potential Line, and Residuals")
gg1_gif<- animate(gg1, nframes = nframes, duration = duration, width = width, height = height, res = 300)
gg1_gif
gg1_mgif <- image_read(gg1_gif)


gg2 <- ggplot(par_df, aes(x = .resid)) +
  stat_density(geom = "line", size = 1) +
  scale_fill_gradient(low = "black", high = "red") + 
  theme_minimal() + theme(legend.position = "none") +
  transition_states(id, transition_length = 0) +
  shadow_mark(past = TRUE, color = "grey80", size = 0.2) + 
  labs(title = "The Distribution of the Residuals",
       x = "Residual",
       y = "")
gg2_gif<- animate(gg2, nframes = nframes, duration = duration, width = width, height = height, res = 300)
gg2_mgif <- image_read(gg2_gif)


gg3 <- ggplot(rms_df, aes(m, b, fill = rms_error, 
                          size = rms_error, 
                          label = round(rms_error, 2))) + 
  geom_label(color = "white", fontface = "bold") + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_size_area() + 
  theme_minimal() + 
  transition_states(id, transition_length = 0) + 
  shadow_mark(past = TRUE) + 
  theme(legend.position = "none") + 
  labs(x = "Slope", 
       y = "Intercept",
       title = "Grid Search for Slope and Intercept of Best-Fit Line")
gg3_gif<- animate(gg3, nframes = nframes, duration = duration, width = width, height = height, res = 300)
gg3_mgif <- image_read(gg3_gif)



gg4 <- ggplot(rms_df_tall, aes(id, value)) + 
  geom_segment(aes(xend = id - 1, yend = value_lag), linetype = 1, colour = "black") + 
  geom_segment(aes(xend = max(id), yend = value), linetype = 2, color = "grey") + 
  geom_point(size = 2) + 
  geom_text(aes(x = max(id), label = name), hjust = 0) + 
  transition_states(id, transition_length = 0, ) + 
  shadow_mark(past = TRUE, exclude_layer = c(2, 3, 4)) + 
  coord_cartesian(clip = 'off') + 
  theme_minimal() + 
  facet_wrap(vars(name), ncol = 1, scales = "free") + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        strip.text = element_blank())
gg4_gif<- animate(gg4, nframes = nframes, duration = duration, width = width, height = height, res = 300)
gg4_mgif <- image_read(gg4_gif)



new_gif <- image_append(
  c(image_append(c(gg1_mgif[1], gg2_mgif[1])),
    image_append(c(gg3_mgif[1], gg4_mgif[1]))), 
  stack = TRUE)
for(i in 2:100){
  combined_gif <- image_append(
    c(image_append(c(gg1_mgif[i], gg2_mgif[i])),
      image_append(c(gg3_mgif[i], gg4_mgif[i]))), 
    stack = TRUE)
  new_gif <- c(new_gif, combined_gif)
}
new_gif