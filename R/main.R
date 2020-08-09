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
library(gghdr)
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
response = "general_supply_kwh", dist_ordered = TRUE)

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
                                        response = "general_supply_kwh", dist_ordered = TRUE)


##----question1

p1 <- sm %>% prob_plot("wknd_wday", 
                    "week_month",
                    plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
 xlab("")

p3 <- sm %>% prob_plot("wknd_wday", 
                       "day_month",
                       plot_type = "boxplot",
                       quantile_prob = c(0.25, 0.5, 0.75), symmetric = FALSE) +
  scale_x_discrete(breaks =  seq(1, 31, 5)) + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")

p2 <- sm %>% prob_plot("week_month", 
                          "day_week",
                          plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")


p4 <- sm %>% prob_plot("wknd_wday", 
                          "hour_day",
                          plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")

ggarrange(p1, p3, nrow = 2, labels = c("a", "c"))



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

##----graphical map

knitr::include_graphics("images/graphical_map.png")


##----clash

VIC <- tsibbledata::vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Jul", "Nov")) %>%
  prob_plot("month_year",
            "day_year",
            response = "Demand",
            plot_type = "quantile",
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
            symmetric = FALSE) + ggtitle("") + theme_remark() + 
  scale_x_discrete(breaks = seq(0, 364, 40)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Energy consumption (kwh)")

##----noclash

VIC <- tsibbledata::vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
            response = "Demand",
            plot_type = "lv") + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16)) + 
  scale_x_discrete(breaks = c("Sun", "Wed", "Fri"))+  ylab("Energy consumption (kwh)")

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
                       plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")

p3 <- sm %>% prob_plot("wknd_wday", 
                       "day_month",
                       plot_type = "boxplot",
                       quantile_prob = c(0.25, 0.5, 0.75), symmetric = FALSE) +
  scale_x_discrete(breaks =  seq(1, 31, 5)) + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")

p2 <- sm %>% prob_plot("week_month", 
                       "day_week",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")


p4 <- sm %>% prob_plot("wknd_wday", 
                       "hour_day",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("Energy consumption (kwh)") + 
  xlab("")

ggarrange(p1, p3, nrow = 2, labels = c("a", "c"))

##----question4

ggarrange(p2, p4, nrow = 2, labels = c("b", "d"))



##----allplot

# pbox <- smart_meter50 %>% 
#   create_gran("hour_day") %>% 
#   filter(hour_day %in% c(20, 10, 2, 15)) %>% 
#   ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
#   geom_boxplot() + ylab("") + xlab("")
# 
# 
# pviolin <- smart_meter50 %>% 
#   create_gran("hour_day") %>% 
#   filter(hour_day %in% c(20, 10, 2, 15)) %>% 
#   ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
#   geom_violin() + ylab("") + xlab("")
#   
# plv <- smart_meter50 %>% 
#   create_gran("hour_day") %>% 
#   filter(hour_day %in% c(20, 10, 2, 15)) %>% 
#   ggplot(aes(x = hour_day, y = log(general_supply_kwh))) + 
#   geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1) +
#   ylab("") + xlab("") + theme(legend.position = "None") 


mpg <- mpg %>% 
  filter (class %in% c("compact", "midsize", "suv","minivan")) %>% 
  mutate(cls = 
           case_when(
             class == "compact" ~ "A",
             class == "midsize" ~ "B",
             class == "suv" ~ "C",
             class == "minivan"  ~ "D"))

pbox <- ggplot(mpg, aes(cls, hwy)) + 
  geom_boxplot() + ylab("") + xlab("") + 
  theme(
    axis.text = element_text(size = 16), plot.title = element_text(size = 24))  +
  ggtitle("box")

pridge <-  ggplot(mpg, aes(hwy, cls)) + 
  geom_density_ridges2() + 
  xlab("") + 
  ylab("") + theme(
    axis.text = element_text(size = 14),  plot.title = element_text(size = 24))+
  ggtitle("ridge")

pviolin <-  ggplot(mpg, aes(cls, hwy)) + 
  geom_violin() + ylab("") + xlab("")+ theme(
    axis.text = element_text(size = 14), plot.title = element_text(size = 24))+
  ggtitle("violin")

plv <-  ggplot(mpg, aes(cls, hwy)) + 
  geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1) +
  ylab("") + xlab("") +  xlab("") + ylab("")+  theme(legend.position = "bottom", legend.text = element_text(size=14), plot.title = element_text(size = 24)) +  scale_fill_brewer(palette = "Dark2")+
  ggtitle("letter-value")

p4_quantile <- mpg %>% 
  group_by(cls) %>%  do({
    x <- .$hwy
    map_dfr(
      .x = c(0.25, 0.5, 0.75, 0.9),
      .f = ~ tibble(
        Quantile = .x,
        Value = quantile(x, probs = .x, na.rm = TRUE)
      )
    )
  })

pquant <- p4_quantile %>% ggplot(aes(x = cls, y = Value, group = Quantile,  col = as.factor(Quantile))) + geom_line() +   xlab("") + ylab("") + theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +   ylab("") + xlab("")  + guides(color = guide_legend(title = "quantiles"))+  theme(legend.position = "bottom", legend.text=element_text(size=16), plot.title = element_text(size = 24)) + ggtitle("quantile")

phdr <- ggplot(data = mpg,
               aes(y = hwy, fill = cls)) + 
  geom_hdr_boxplot(all.modes = FALSE, prob = c(0.5, 0.9)) +
  ylab("") +
  xlab("") + theme(legend.position = "bottom", plot.title = element_text(size = 24)) + scale_fill_brewer(palette = "Dark2")+
  ggtitle("hdr")


pbox
pridge
pviolin
plv
pquant
phdr


# ggarrange(pbox, pviolin, pridge, pquant, plv,  phdr, nrow = 2, ncol = 3, labels = c("box", "violin", "ridge","quantile", "letter-value",  "hdr-box"))



##----linear2cyclic

load("data/sm_cust50.Rdata")


sm_cust50 <- sm_cust50 %>% 
  tsibble::as_tsibble(regular = FALSE)

smart_meter50 <-  sm_cust50 %>%
  select(customer_id, 
                                      reading_datetime,
                                      general_supply_kwh, 
                                      everything())

data_cust1 <- smart_meter50 %>% filter(customer_id == 10017936)

data_cust1%>% ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh), color = "#1B9E77")+ theme(legend.position = "bottom") + ylab("")+ 
  theme_remark()


smart_meter50 %>%
  filter(customer_id==10018250) %>%
  mutate(hour_day = hour(reading_datetime)) %>% 
  ggplot() + geom_point(aes(x = hour_day, 
                            y = general_supply_kwh), color = "#1B9E77") + 
  theme_remark() + ylab("")

##----rank harmony

smart_harmony %>% 
  rename("facet" = "facet_variable",
         "x-axis" = "x_variable",
         "facet levels" = "facet_levels",
         "x levels" = "x_levels",
         "MMPD" = "MMPD") %>%  
  knitr::kable() %>% 
  kable_styling("striped", full_width = F) %>%
  row_spec(1:16, color = "black", background = "white", font_size = 18) %>% 
  row_spec(0, background = "white") 

##---- ranking disclosed

knitr::include_graphics("images/ranking.png")

##---- threshold

knitr::include_graphics("images/threshold.png")

##---- normalization

knitr::include_graphics("images/normalization.png")



##----read
gravitas::smart_meter10


##----search_gran


smart_meter10 %>%
  search_gran(lowest_unit = "hour", highest_unit =  "month", 
              filter_out = "fortnight", filter_in = "wknd_wday")



##----question1

p1 <- sm %>% prob_plot("wknd_wday", 
                       "week_month",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("") + 
  xlab("") + theme_alldist()


p4 <- sm %>% prob_plot("wknd_wday", 
                       "hour_day",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("") + 
  xlab("") + scale_x_discrete(breaks = c(0,seq(3, 23, 3))) + 
  theme_alldist()

ggarrange(p1, p4, nrow = 2, labels = c("a", "b"))


##----questionselect

p55 <- sm %>% prob_plot("week_month", 
                       "hour_day",
                       plot_type = "boxplot") + ggtitle("") +
  ylab("") + 
  xlab("") + theme_alldist()


ggarrange(p4, p55, nrow = 2, labels = c("a", "b")) 

##----questioncric


cricket_data <- read_rds("data/cricket_data.rds")

hierarchy_model <- tibble::tibble(
  units = c("index", "over", "inning", "match"),
  convert_fct = c(1, 20, 2, 1))

hierarchy_tbl <- hierarchy_model

response <- "runs_per_over"

harmonies_cric <- cricket_data %>% harmony(lgran = "over", ugran = "match", filter_in = c("lag_field", "over"), hierarchy_tbl = hierarchy_model)

rank_cricket <- cricket_data %>% rank_harmony(harmony_tbl = harmonies_cric, 
             response = response,
             hierarchy_tbl = hierarchy_model,
             dist_distribution = "normal")
# 
# cricket_data %>% 
#   create_gran("inning_match",
# hierarchy_tbl = hierarchy_model) %>% 
# prob_plot("lag_field", "inning_match", 
#           plot_type = "quantile",
#           symmetric = FALSE,
#           quantile_prob = c(0.25, 0.5, 0.75),
#           hierarchy_tbl = hierarchy_model)
# 
# 
# cricket_data %>% 
#   create_gran("inning_match",
#               hierarchy_tbl = hierarchy_model) %>% 
#   prob_plot("lag_field","over",plot_type = "quantile", symmetric = FALSE, hierarchy_tbl = hierarchy_model)
# 
# cricket_data %>%
#   filter(over!=1) %>%
#   prob_plot("over", "lag_field",
#             hierarchy_model,
#             response = "run_rate",
#             plot_type = "quantile",
#             symmetric = FALSE,
#             quantile_prob = c(0.25, 0.5, 0.75)) 

p1cric <- cricket_data %>% 
  create_gran("inning_match",
              hierarchy_tbl = hierarchy_model)%>% 
  prob_plot("lag_field", "inning_match", 
            plot_type = "quantile",
            response = "runs_per_over",
            symmetric = FALSE,
            quantile_prob = c(0.25, 0.5, 0.75),
            hierarchy_tbl = hierarchy_model)+
  ylab("") + 
  xlab("") + theme_alldist()+ ggtitle("")+ scale_color_brewer(palette = "Dark2")


p2cric <- cricket_data %>% 
  create_gran("inning_match",
              hierarchy_tbl = hierarchy_model) %>% 
  prob_plot("inning_match", "lag_field", 
            plot_type = "quantile",
            response = "runs_per_over",
            symmetric = FALSE,
            quantile_prob = c(0.25, 0.5, 0.75),
            hierarchy_tbl = hierarchy_model)+
  ylab("") + 
  xlab("") + theme_alldist() +ggtitle("") + scale_color_brewer(palette = "Dark2")


ggarrange(p1cric, p2cric, nrow = 2, labels = c("a", "b")) 
