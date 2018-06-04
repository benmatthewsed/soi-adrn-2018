
# data lexis grid for SOI ADRN 2018 ---------------------------------------
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(here)
library(Cairo)

# data range df


year <- rep(1989:2011, each = 61)
year <- as.data.frame(year)
age <- rep(0:60, 23)
age <- as.data.frame(age)
z <- rep(0:60, 23)
z <- as.data.frame(z)
df <- bind_cols(year, age, z) %>% 
  rename(Year = year,
         Age = age)

rm(age, year)


lines <- seq(-1930, -2020 , -5)

lines <- 
  as.tibble(lines) %>% 
  rename(intercept = value) %>% 
  mutate(slope = 1)




# plot axes ---------------------------------------------------------------


axes <- 
  df %>% 
  ggplot(aes(x = Year, y = Age)) +
  coord_fixed(ratio = 1, xlim = c(1989, 2011), ylim = c(0, 60)) +
  geom_abline(data = lines, aes(intercept = intercept, slope = slope), colour = "grey90") +
  geom_hline(yintercept = seq(0, 60, 5), colour = "grey50") +
  geom_vline(xintercept = seq(1985, 2016, 5), colour = "grey50") +
  scale_x_continuous(breaks = seq(1985, 2015, 5)) + #specify numbers every five years
  scale_y_continuous(breaks = seq(0, 60, 5)) + 
  theme_bw() +
  theme(text = element_text(size=20))
  
ggsave(filename = here("figures", "01-axes.png"), plot = axes, type = "cairo",
       height = 10, width = 8, device = NULL)


# plot berg's example -----------------------------------------------------

# ages 17-18, years 1992 and 2000

berg_year <- rep((1992-18):2000, each = 20)
berg_year <- as.data.frame(berg_year)
berg_age <- rep(0:19, 27)
berg_age <- as.data.frame(berg_age)
berg_z <- rep(0:19, 27)
berg_z <- as.data.frame(berg_z)
berg_df <- bind_cols(berg_year, berg_age, berg_z) %>% 
  rename(Year = berg_year,
         Age = berg_age,
         z = berg_z)

# adding soi rectangle
soi_df <- 
df %>% 
  filter(Age >= 16)

soi_plot <- 
axes +
  geom_point(data = soi_df, shape = 15, colour = "#00BFC4",
             alpha = 0.7)

ggsave(filename = here("figures", "02-soi-coverage.png"), plot = soi_plot, type = "cairo",
       height = 10, width = 8, device = NULL)


berg_data <- tribble(~Year, ~Age,
                     1992, 17, 
                     2000, 17,
                     1992, 18,
                     2000, 18)

# and berg plots

berg_plot <- 
soi_plot + 
  geom_point(data = berg_data, aes(x = Year, y = Age), shape = 15, colour = "#F8766D")

ggsave(filename = here("figures", "03-berg-points.png"), plot = berg_plot, type = "cairo",
       height = 10, width = 8, device = NULL)


berg_points <- 
#  tribble(~cohort, ~pointx, ~pointy, ~segx, ~segy, ~segxend, ~omitted,
#          "one",   1995,    0,    2000,     5,     2012,    FALSE,
#          "two",   1997,    0,    2000,     3,     2007,    TRUE,
#          "three", 2008,    0,    2008,     0,     2015.5,  FALSE,
#          "four",  2012,    0,    2012,     0,     2015.5,  FALSE)

ggsave(filename = here("figures", "03-berg-points.png"), plot = berg_plot, type = "cairo",
       height = 10, width = 8, device = NULL)
