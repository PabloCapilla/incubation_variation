###
###
#' 
#' Script for:
#' 
#' 
###
###

# Clear memory to make sure there are not files loaded that could cause problems
rm(list=ls())

##
##
##### Script aim: #####
#'
#' 
##
##

##
##
##### libraries #####
##
##
pacman::p_load(openxlsx, gtsummary, gt, ggokabeito,
               lubridate, dplyr, tidyr,
               lme4, performance, rptR,
               cowplot,
               ggdist,
               ggokabeito,   
               gghalves,     
               ggbeeswarm,
               ggplot2, extrafont)
loadfonts()

source("./scripts/FUNCTION_drop1_output.R")

#####

##
##
##### data #####
##
##
data <- readRDS("./data/data_incubation.RDS")
head(data)


table(is.na(data$night_var))

data <- data %>% 
  filter(!is.na(night_var))

#####

##
##
##### Sample sizes and data summary #####
##
##
head(data)
nrow(data) # total observations
length(unique(data$box)) # number of nest-boxes included

# number of days of observation per year and habitat
data %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n()) 

# number of clutches per year and habitat
data %>% 
  group_by(year, box) %>% 
  filter(row_number() == 1) %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n()) 

# days of incubation per clutch
data %>% 
  group_by(box,year) %>% 
  summarise(days_box = n()) %>%
  ungroup(box) %>% 
  summarise(min_range = min(days_box),
            max_range = max(days_box),
            med = median(days_box))

# number of nest-boxes per site (not clutches)
data %>% 
  group_by(site, box) %>% 
  filter(row_number() == 1) %>% 
  group_by(site) %>% 
  summarise(n_boxes = n())

# mean start of incubation per habitat
data %>% 
  group_by(year, area, box) %>% 
  summarise(mean_box = mean(inc_start_aprildays)) %>% 
  group_by(area) %>% 
  summarise(mean_date = mean(mean_box),
            n_size = n(),
            sd_date = sd(mean_box),
            se_date = sd(mean_box/sqrt(n())))

#####

##
##
##### Models for variation in night temperature across sites #####
##
##

summary(data$night_var)

# model fit
model_night_temp <- lmer(lognight_var ~ 
                           poly(inc_start_aprildays,2)[,2] +
                           poly(inc_start_aprildays,2)[,1] +
                           poly(day_before_hatch,2)[,2] +
                           poly(day_before_hatch,2)[,1] +
                           
                           
                           area : poly(inc_start_aprildays,2)[,2] +
                           area : poly(inc_start_aprildays,2)[,1] +
                           area : poly(day_before_hatch,2)[,2] +
                           area : poly(day_before_hatch,2)[,1] +

                           meantemp +
                           clutch_size + 
                           area + 
                           (1|year)+
                           (1|site) +
                           (1|box), 
                         REML = F,
                         na.action = "na.fail",
                         data= data) #full model
summary(model_night_temp)

# model diagnostics
check_model(model_night_temp, panel = T) # not too bad overall

#####

##
##
##### Are interactions significant? #####
##
##

## 1
drop1(model_night_temp, test = "Chisq")

## interaction 1
anova(model_night_temp, 
      update(model_night_temp, 
             . ~ . - poly(day_before_hatch, 2)[, 2]:area),
      test = "LRT")

## 2
model_night_temp2 <- update(model_night_temp, .~.-poly(day_before_hatch, 2)[, 2]:area)
drop1(model_night_temp2, test = "Chisq")

## interaction 2
anova(model_night_temp2, update(model_night_temp2, 
                               . ~ . - poly(day_before_hatch, 2)[, 1]:area),
      test = "LRT")

## 3
model_night_temp3 <- update(model_night_temp2, .~.-poly(day_before_hatch, 2)[, 1]:area)
drop1(model_night_temp3, test = "Chisq")

## interaction 3 
anova(model_night_temp3, update(model_night_temp3, 
                                . ~ . - poly(day_before_hatch, 2)[, 2]),
      test = "LRT")

## 4
model_night_temp4 <- update(model_night_temp3, .~.-poly(day_before_hatch, 2)[, 2])
drop1(model_night_temp4, test = "Chisq")



## removing both interactions 
full_model <- model_night_temp4
drop1(full_model, test = "Chisq")
summary(full_model)

drop1(update(full_model, .~.+area:inc_start_aprildays), test = "Chisq")
drop1(update(full_model, .~.+day_before_hatch:area), test = "Chisq")

# model diagnostics
check_model(full_model, panel = T) # not too bad overall

#####

##
##
##### Plot model predictions #####
##
##
full_model_predictions <- lmer(lognight_var ~ 
                                 inc_start_aprildays +
                                 I(inc_start_aprildays^2) +
                                 
                                 area : inc_start_aprildays +
                                 area : I(inc_start_aprildays^2) +
                                 
                                 day_before_hatch +
                                 meantemp +
                                 clutch_size + 
                                 area + 
                                 (1|year) +
                                 (1|site) +
                                 (1|box), 
                               REML = F,
                               na.action = "na.fail",
                               data= data) #full model
summary(full_model_predictions)
drop1(full_model_predictions, test = "Chisq")

# new dataframe to predict
df_pred <- expand.grid(day_before_hatch = seq(min(data$day_before_hatch), 
                                              max(data$day_before_hatch), 1),
                       area = c("City", "Forest"),
                       meantemp = mean(data$meantemp),
                       inc_start_aprildays = seq(min(data$inc_start_aprildays), 
                                                 max(data$inc_start_aprildays), 1))
df_pred$clutch_size <- NA
df_pred$clutch_size[df_pred$area == "Forest"] <- mean(data$clutch_size[data$area == "Forest"])
df_pred$clutch_size[df_pred$area == "City"] <- mean(data$clutch_size[data$area == "City"])

df_pred$prediction <- predict(full_model_predictions, df_pred, re.form = NA)

# plot only data in range
data %>% 
  group_by(area) %>% 
  summarise(min_chr = min(inc_start_aprildays),
            max_chr = max(inc_start_aprildays))

remove_city <- which((df_pred$inc_start_aprildays < 20 | 
                        df_pred$inc_start_aprildays > 44) & 
                       df_pred$area == "City")
remove_forest <- which((df_pred$inc_start_aprildays < 28 | 
                          df_pred$inc_start_aprildays > 53) & 
                         df_pred$area == "Forest")
df_pred <- df_pred[-c(remove_city, remove_forest),]

# SE for mean predictions
mm <- model.matrix(~ inc_start_aprildays +
                     I(inc_start_aprildays^2) +
                     
                     area : inc_start_aprildays +
                     area : I(inc_start_aprildays^2) +
                     
                     day_before_hatch +
                     meantemp +
                     clutch_size + 
                     area,
                   data = df_pred)
pvar1 <- diag(mm %*% tcrossprod(vcov(full_model_predictions),mm))
cmult <- 1 ## 1 SE
df_pred <- data.frame(
  df_pred
  , plo = df_pred$prediction-cmult*sqrt(pvar1)
  , phi = df_pred$prediction+cmult*sqrt(pvar1)
)


##
## plot for date from April 1
var_date_plot <- ggplot(data = data, 
                              aes(x = inc_start_aprildays, 
                                  y = lognight_var,
                                  fill = area,
                                  color = area)) +
  geom_point(alpha = 0.25,
             size = 2.5,
             shape = 21,
             position = position_jitter(width = 0.15)) +
  theme_bw() +
  facet_wrap(~area) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 15),
        axis.text.x = element_text("Arial", size = 10),
        axis.text.y = element_text("Arial", size = 12)) +
  geom_ribbon(data = df_pred %>% 
                group_by(area, inc_start_aprildays) %>% 
                summarise(mean_plo = mean(plo),
                          mean_phi = mean(phi),
                          mean_pred = mean(prediction)), 
              aes(ymin = mean_plo, 
                  ymax = mean_phi, 
                  y = mean_pred),
              color = NA,
              alpha = 0.5) +
  geom_line(data = df_pred %>% 
              group_by(area, inc_start_aprildays) %>% 
              summarise(mean_plo = mean(plo),
                        mean_phi = mean(phi),
                        mean_pred = mean(prediction)), 
            aes(y = mean_pred), 
            size = 1.5) +
  labs(x = "Incubation start date (days after April 1)", 
       y = expression(atop("log Variation in night-time", 
                           "incubation temperature (ºC)"))) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito()

var_date_plot

ggsave(filename = "./plots/Figure 1c.png", 
       plot = var_date_plot, 
       device = "png", 
       units = "mm",
       width = 135, 
       height = 100)  


##
## plot for days before hatch
var_days_before_hatch_plot <- ggplot(data = data, 
                                     aes(x = day_before_hatch, 
                                         y = lognight_var,
                                         fill = area,
                                         color = area)) +
  geom_point(alpha = 0.25,
             size = 2.5,
             shape = 21,
             position = position_jitter(width = 0.15)) +
  theme_bw() +
  facet_wrap(~area) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 15),
        axis.text.x = element_text("Arial", size = 10),
        axis.text.y = element_text("Arial", size = 12)) +
  geom_ribbon(data = df_pred %>% 
                group_by(area, day_before_hatch) %>% 
                summarise(mean_plo = mean(plo),
                          mean_phi = mean(phi),
                          mean_pred = mean(prediction)), 
              aes(ymin = mean_plo, 
                  ymax = mean_phi, 
                  y = mean_pred),
              color = NA,
              alpha = 0.5) +
  geom_line(data = df_pred %>% 
              group_by(area, day_before_hatch) %>% 
              summarise(mean_plo = mean(plo),
                        mean_phi = mean(phi),
                        mean_pred = mean(prediction)), 
            aes(y = mean_pred), 
            size = 1.5) +
  labs(x = "Days before hatching", 
       y = expression(atop("log Variation in night-time", 
                               "incubation temperature (ºC)"))) +
  scale_x_continuous(labels = 15:1, breaks = -15:-1) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito()

var_days_before_hatch_plot

ggsave(filename = "./plots/Figure 1b.png", 
       plot = var_days_before_hatch_plot, 
       device = "png", 
       units = "mm",
       width = 160, 
       height = 110)  

##
## comparison urban forest
box_plot_var <- ggplot(data = data, 
       aes(x = area, 
           y = lognight_var,
           fill = area,
           color = area)) +
  geom_point(position = position_jitter(width = 0.25),
             alpha = 0.25, 
             shape = 21,
             size = 1.5) +
  geom_boxplot(alpha = 0.25) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text("Arial", size = 10),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 15),
        axis.text = element_text("Arial", size = 12)) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito() +
labs(x = "Habitat", 
     y = expression(atop("log Variation in night-time", 
                         "incubation temperature (ºC)")))
  

ggsave(filename = "./plots/Figure 1a.png", 
       plot = box_plot_var, 
       device = "png", 
       units = "mm",
       width = 75, 
       height = 110)  

## panel

Figure1 <- ggdraw() +
  draw_plot(box_plot_var, x = 0.00, y = 0.00, width = 0.30, height = 1.00) +
  draw_plot(var_days_before_hatch_plot, x = 0.32, y = 0.50, width = 0.68, height = 0.50) +
  draw_plot(var_date_plot, x = 0.32, y = 0.00, width = 0.68, height = 0.50) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 25,
                  x = c(0.00, 0.35, 0.35), 
                  y = c(1.00, 1.00, 0.50)) 

ggsave(filename = "./plots/Figure_1.png", 
       plot = Figure1, 
       units = "mm",
       device = "png", 
       width = 220,
       height = 200)
#####


##
##
##### Table of results 1 #####
##
##

## base table
table_00 <- full_model_predictions %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `inc_start_aprildays`  = "Incubation start date", 
                   `I(inc_start_aprildays^2)` = "Incubation start date2",
                   `day_before_hatch` = "Days before hatching",
                   `meantemp` = "Mean daily temperatures",
                   `clutch_size` = "Clutch size",
                   `area` = "Habitat"),
                 estimate_fun = ~ style_number(.x, digits = 3))

## add features
table_nighttime_var <- table_00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=full_model_predictions) %>% 
                          dplyr::select(variable = term, Chisq=statistic, df),
                        by = "variable")
    output$df <- ifelse(output$row_type == "label",  output$df, NA)
    output$Chisq <- ifelse(output$row_type == "label",  output$Chisq, NA)
    return(output)
  }) %>% 
  modify_fmt_fun(c(Chisq) ~ function(x) style_number(x, digits = 2)) %>%
  modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 3)) %>%
  modify_fmt_fun(c(p.value) ~ function(x) style_number(x, digits = 3)) %>%
  modify_table_body(~.x %>% dplyr::relocate(p.value, .after = df)) %>% 
  modify_header(label ~ "**Fixed effect**") %>% 
  modify_header(std.error ~ "**SE**") %>%
  modify_header(estimate ~ "**Estimate**") %>%
  modify_header(df ~ "**df**") %>% 
  modify_header(Chisq ~ html("<b>&chi;<sup>2</sup></b>")) %>% 
  as_gt() %>% 
  opt_footnote_marks(marks = "LETTERS")

##
## save table
gtsave(table_nighttime_var, "./tables/TABLE 1.html")

#####

