## ---- load
source("R/theme.R")
library(tidyverse)
library(dplyr)
library(sugrrants)
library(tsibble)
library(gravitas)
library(kableExtra)
library(gganimate)
library(lubridate)
library(ggridges)
library(ggpubr)
library(lvplot)
library(magrittr)
library(countdown)
#remotes::install_github("njtierney/palap")
#library(palap)
sm <- smart_meter10 %>%
  filter(customer_id %in% c(10017936))

harmonies <- sm %>%
  harmony(ugran = "month",
        filter_in = "wknd_wday",
        filter_out = c("hhour", "fortnight"))
.data = sm
response  = "general_supply_kwh"
harmony_tbl =  harmonies
smart_harmony <- .data %>% rank_harmony(harmony_tbl = harmonies,
response = "general_supply_kwh", dist_ordered = FALSE)

##----question1

p1 <- sm %>% prob_plot("wknd_wday", 
                    "week_month",
                    plot_type = "boxplot") + ggtitle("")

p3 <- sm %>% prob_plot("wknd_wday", 
                       "day_month",
                       plot_type = "boxplot",
                       quantile_prob = c(0.25, 0.5, 0.75), symmetric = FALSE) +
  scale_x_discrete(breaks =  seq(1, 31, 5)) + ggtitle("") + ylab("")

p2 <- sm %>% prob_plot("wknd_wday", 
                          "day_week",
                          plot_type = "boxplot") + ggtitle("")



p4 <- sm %>% prob_plot("wknd_wday", 
                          "hour_day",
                          plot_type = "boxplot") + ggtitle("") + ylab("")

ggarrange(p1, p3, nrow = 2, labels = c("a", "c"))

##----question2

ggarrange(p2, p4, nrow = 2, labels = c("b", "d"))

##----blank graph
# 
# Ci = factor(c("A1", "A2", "...", "Ak", "Ak"), 
#             levels = c("A1", "A2", "...", "Ak"))
# 
# Cj = factor(c("B1", "B2", "B3","...","Bl"), 
#        levels = c("B1", "B2", "B3", "...", "Bl"))
# 
# v = c(1, 2, 3, 3,2)
# data = tibble::tibble(Ci, Cj, v)
# 
# data %>% ggplot(aes(x = Cj, y = v)) + facet_wrap(~Ci, nrow = 1) + ggplot2::theme_grey() + xlab("")
# 
# 
 # data %>% ggplot(aes(x = Cj, y = v)) + facet_wrap(~Ci, nrow = 1) + ggplot2::theme_grey() + xlab("") +   theme(strip.text = element_text(size = 20), strip.background.y = element_rect(size = 20)) + theme(text = element_text(size=18, face = "bold")) + scale_y_continuous(breaks = seq(1, 3, 1))
 # 

knitr::include_graphics("images/vic_struc.png")

##----data structure

knitr::include_graphics("images/tsibble_struc.png")


##----noclash

VIC <- tsibbledata::vic_elec

p11 <- VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
            response = "Demand",
            plot_type = "lv") + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16))



p12 <- VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Jul", "Nov")) %>%
  prob_plot("month_year",
            "day_year",
            response = "Demand",
            plot_type = "quantile",
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
            symmetric = FALSE) + ggtitle("") + theme_remark() + 
  scale_x_discrete(breaks = seq(0, 364, 20)) +
  scale_color_brewer(palette = "Dark2")

p11
p12

##----countdown

countdown(minutes = 2,
          seconds = 00, 
          left = 0,
          top = 0,
          padding = "10px",
          font_size = "1em")


##----countdown2

countdown(minutes = 2,
          seconds = 00, 
          left = 0,
          top = 0,
          padding = "10px",
          font_size = "1em")


##----question3

p1 <- sm %>% prob_plot("wknd_wday", 
                       "week_month",
                       plot_type = "boxplot") + ggtitle("")

p3 <- sm %>% prob_plot("wknd_wday", 
                       "day_month",
                       plot_type = "boxplot",
                       quantile_prob = c(0.25, 0.5, 0.75), symmetric = FALSE) +
  scale_x_discrete(breaks =  seq(1, 31, 5)) + ggtitle("") + ylab("")

p2 <- sm %>% prob_plot("wknd_wday", 
                       "day_week",
                       plot_type = "boxplot") + ggtitle("")



p4 <- sm %>% prob_plot("wknd_wday", 
                       "hour_day",
                       plot_type = "boxplot") + ggtitle("") + ylab("")

ggarrange(p1, p3, nrow = 2, labels = c("a", "c"))

##----question4

ggarrange(p2, p4, nrow = 2, labels = c("b", "d"))
