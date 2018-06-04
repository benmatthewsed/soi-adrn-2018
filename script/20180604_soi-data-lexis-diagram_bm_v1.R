
# data lexis grid for SOI ADRN 2018 ---------------------------------------
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(here)
library(Cairo)

# data range df


year <- rep(1929:2011, each = 61)
year <- as.data.frame(year)
age <- rep(0:60, 83)
age <- as.data.frame(age)
z <- rep(0:60, 83)
z <- as.data.frame(z)
df <- bind_cols(year, age, z) %>% 
  rename(Year = year,
         Age = age)

rm(age, year)


lines <- seq(-1860, -2020 , -5)

lines <- 
  as.tibble(lines) %>% 
  rename(intercept = value) %>% 
  mutate(slope = 1)




# plot axes ---------------------------------------------------------------


axes <- 
  df %>% 
  ggplot(aes(x = Year, y = Age)) +
  coord_fixed(ratio = 1, xlim = c(1925, 2016), ylim = c(0, 60)) +
  geom_abline(data = lines, aes(intercept = intercept, slope = slope), colour = "grey90") +
  geom_hline(yintercept = seq(0, 60, 5), colour = "grey50") +
  geom_vline(xintercept = seq(1925, 2016, 5), colour = "grey50") +
  scale_x_continuous(breaks = seq(1925, 2015, 5)) + #specify numbers every five years
  theme_bw() +
  theme(text = element_text(size=20))
  
ggsave(filename = here("figures", "01-axes.png"), plot = axes, type = "cairo",
       height = 10, width = 13.333, device = NULL)



# plot data range ---------------------------------------------------------
background <- 
df %>% 
  ggplot(aes(x = Year, y = Age)) +
  coord_fixed(ratio = 1, xlim = c(1925, 2011), ylim = c(0, 60)) +
  geom_abline(data = lines, aes(intercept = intercept, slope = slope), colour = "grey90") +
  geom_hline(yintercept = seq(0, 60, 5), colour = "grey50") +
  geom_vline(xintercept = seq(1925, 2010, 5), colour = "grey50") +
  scale_x_continuous(breaks = seq(1925, 2011, 5)) + #specify numbers every five years
  geom_rect(xmin = 1920, xmax = 1989, ymin = -5, ymax = 66, alpha = 0.1, fill = "grey90") +
  geom_rect(xmin = 1989, xmax = 2021, ymin = -5, ymax = 16, alpha = 0.1, fill = "grey90") +
  geom_rect(xmin = 2011, xmax = 2021, ymin = 16, ymax = 66, alpha = 0.1, fill = "grey90") +
    theme_bw() #+
  #theme(text = element_text(size=20))

ggsave(filename = here("figures", "02-background.png"), plot = background, type = "cairo",
       height = 10, width = 13.333, device = NULL)





# plot hypothetical children ----------------------------------------------

triangle <- tribble(~group, ~y,  ~x,
                       1,      16, 1989, 
                       1,      60, 1989,
                       1,      16, (1989-(60-16)))

eg_histories <- 
background +
  geom_polygon(data = triangle, aes(x = x, y = y, group = group), alpha = 0.3, fill = "blue")
  
ggsave(filename = here("figures", "03-eg_histories.png"), plot = eg_histories, type = "cairo",
       height = 10, width = 13.333, device = NULL)




# plot berg's example -----------------------------------------------------

# ages 17-18, years 1992 and 2000

year <- rep(1929:2011, each = 61)
year <- as.data.frame(year)
age <- rep(0:60, 83)
age <- as.data.frame(age)
z <- rep(0:60, 83)
z <- as.data.frame(z)
df <- bind_cols(year, age, z) %>% 
  rename(Year = year,
         Age = age)


# plot trimmed and split histories ----------------------------------------

in_bounds <- 
background +
  geom_point(data = points, aes(x = pointx, y = pointy, colour = child), size = 8, shape = 1) +
  geom_point(data = points, aes(x = segx, y = segy, colour = child), size = 2) +
  geom_point(data = points, aes(x = segxend, y = segyend, colour = child), size = 2) +
  geom_segment(data = points, aes(x = segx, y = segy, 
                                  xend = segxend, yend = segyend,
                                  colour = child, linetype = omitted), size = 1) +
  scale_linetype_manual(values = c("solid", "dashed")) + #manually change line type
  theme_bw() +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

# send custom values to aes

ggsave(filename = here("figures", "04-in_bounds.png"), plot = in_bounds, type = "cairo",
       height = 10, width = 13.333, device = NULL)


# making trimmed data -----------------------------------------------------


points <- 
  points %>% 
  mutate(insegx = case_when(
    segx < 2008 & segxend > 2010 ~ 2008,
    segx < 2008 & segxend < 2010 ~ segx,
    segx >= 2008 ~ segx,
    TRUE ~ segx
  )) %>% 
  mutate(insegy = ifelse(child == "one", segy + 2008 - segx, segy)) %>% 
  mutate(insegxend = insegx + 2)

tmp <- 
  points %>% 
  filter(child == "three") %>% 
  mutate(insegx = 2010,
         insegxend = 2012,
         insegy = 2)

tmp2 <- 
  tmp %>% 
  mutate(insegx = 2012,
         insegxend = 2014,
         insegy = 4)

points2 <- 
  bind_rows(points, tmp, tmp2) %>% 
  mutate(insegyend = insegy + insegxend - insegx)

points2 <- 
  points2 %>% 
  arrange(child) %>%
  filter(child != "two") %>% 
  mutate(result = case_when(
    child == "four" | child == "one" ~ "trimmed",
    child == "three" ~ "split",
    TRUE ~ "NA"
  ))

points3 <- 
  points2[c(1, 2 , 4), ] # selects the first, second and fourth rows from points2


# plot trimmed data -------------------------------------------------------

trimmed <- 
  background +
  geom_point(data = points, aes(x = pointx, y = pointy, colour = child), size = 8, shape = 1) +
  geom_point(data = points, aes(x = segx, y = segy, colour = child), size = 2) +
  geom_point(data = points, aes(x = segxend, y = segyend, colour = child), size = 2) +
  geom_point(data = points2, aes(x = insegxend, y = insegyend, colour = child), size = 4, shape = 15) +
  geom_point(data = points2, aes(x = insegx, y = insegy, colour = child), size = 4, shape = 15) +
  geom_segment(data = points2, aes(x = insegx, y = insegy, 
                                   xend = insegxend, yend = insegyend,
                                   colour = child, linetype = omitted), size = 1) +
  geom_segment(data = points, aes(x = segx, y = segy, 
                                  xend = segxend, yend = segyend,
                                  colour = child), linetype = 2, alpha = 0.8, size = 1) +
  theme_bw() +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

ggsave(filename = here("figures", "05-trimmed.png"), plot = trimmed, type = "cairo",
       height = 10, width = 13.333, device = NULL)


# annotate ----------------------------------------------------------------

annotated <- 
  trimmed +
  geom_text_repel(data = points3, aes(x = insegx, y = insegy, label = result, colour = child), 
                  nudge_x = 2, size = 6)

ggsave(filename = here("figures", "06-annotated.png"), plot = annotated, type = "cairo",
       height = 10, width = 13.333, device = NULL)



# period and age ----------------------------------------------------------
period_age <- 
background +
  geom_rect(xmin = 2008, xmax = 2010, ymin = 0, ymax = 22, alpha = 0.05, fill = "#FDDBC7") +
  geom_rect(xmin = 2008, xmax = 2015.5, ymin = 0, ymax = 2, alpha = 0.05, fill = "#D1E5F0") +
  geom_point(data = points, aes(x = pointx, y = pointy, colour = child), size = 8, shape = 1) +
  geom_point(data = points, aes(x = segx, y = segy, colour = child), size = 2) +
  geom_point(data = points, aes(x = segxend, y = segyend, colour = child), size = 2) +
  geom_point(data = points2, aes(x = insegxend, y = insegyend, colour = child), size = 4, shape = 15) +
  geom_point(data = points2, aes(x = insegx, y = insegy, colour = child), size = 4, shape = 15) +
  geom_segment(data = points2, aes(x = insegx, y = insegy, 
                                   xend = insegxend, yend = insegyend,
                                   colour = child, linetype = omitted), size = 1) +
  geom_segment(data = points, aes(x = segx, y = segy, 
                                  xend = segxend, yend = segyend,
                                  colour = child), linetype = "dashed", alpha = 0.8, size = 1) +
  theme_bw() +
  theme(legend.position="none") +
  annotate("text", x = 2009, y = 9, label = "age", size = 8) +
  annotate("text", x = 2015, y = 1, label = "period", size = 8) +
  theme(text = element_text(size=20))

ggsave(filename = here("figures", "07-period_age.png"), plot = period_age, type = "cairo",
       height = 10, width = 13.333, device = NULL)
