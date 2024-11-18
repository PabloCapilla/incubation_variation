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
               stringr,
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

# filter out NAs and create new nestbox id column
data <- data %>% 
  filter(!is.na(night_var)) %>% 
  mutate(nestbox_id = str_extract(box, "\\d+"))

site_table_original <- table(data$site)

# adjust nestbox id to match alan table
data$nestbox_id <- ifelse(data$site == 'KG', as.numeric(data$nestbox_id) + 500, data$nestbox_id) 
data$nestbox_id <- ifelse(data$site == 'GC', as.numeric(data$nestbox_id) + 700, data$nestbox_id) 

# adjust site column to match alan table
data$site <- ifelse(data$site == 'KG', 'kelvingrove_park', data$site) 
data$site <- ifelse(data$site == 'GC', 'garscube', data$site) 
data$site <- ifelse(data$site == 'CASH', 'cashel', data$site) 
data$site <- ifelse(data$site == 'SAL', 'sallochy', data$site) 
data$site <- ifelse(data$site == 'SCENE', 'SCENE', data$site) 

site_table_modified <- table(data$site)

# check numbers are the same
site_table_original
site_table_modified

#####

##
##
##### merging ALAN dataset #####
##
##

# dataset 1
alan <- read.xlsx('./data/ALAN_2024.xlsx', sheet = 1)
alan <- alan %>% rename(nestbox_id = nestbox_number)
head(alan)

# merge both datasets
data_alan00 <- left_join(x = data,
                       y = alan %>% select(nestbox_id, site, light_meter_average), 
                       by = c('site', 'nestbox_id'))

data_completed <- data_alan00 %>% filter(!is.na(light_meter_average)) %>% mutate(dataset = '1')

# number of breeding events included?
data_completed %>% 
  group_by(year, nestbox_id) %>% 
  filter(row_number() == 1) %>% 
  nrow()

data_no_alan <- data_alan00 %>% 
  filter(is.na(light_meter_average))
data_no_alan$light_meter_average <- NULL

# dataset 2
alan2 <- read.xlsx('./data/explanatory_variables_2019.xlsx', sheet = 1)
alan2 <- alan2 %>% 
  mutate(nestbox_id = as.character(nestbox)) %>% 
  rename(light_meter_average = light_Ground)
head(alan2)

table(alan2$site)

data_alan01 <- left_join(x = data_no_alan,
                         y = alan2 %>% select(nestbox_id, site, light_meter_average), 
                         by = c('site', 'nestbox_id'))

data_completed02 <- data_alan01 %>% filter(!is.na(light_meter_average)) %>% mutate(dataset = '2')


data_no_alan <- data_alan01 %>% 
  filter(is.na(light_meter_average))

length(unique(data_no_alan$nestbox_id))

df_box <- data_no_alan %>% 
  group_by(year, nestbox_id) %>% 
  filter(row_number() == 1)

table(df_box$site)

write.csv(x = df_box, file='./data/boxes_no_alan.csv')

#####

##
##
##### merge datasets #####
##
##
data_alan <- rbind(data_completed, data_completed02) %>% filter(area == 'City')

# number of breeding events included?
data_alan %>% 
  group_by(year, nestbox_id) %>% 
  filter(row_number() == 1) %>% 
  nrow()

#####

##
##
##### Sample sizes and data summary #####
##
##
head(data_alan)
nrow(data_alan) # total observations
length(unique(data_alan$box)) # number of nest-boxes included

# number of days of observation per year and habitat
data_alan %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n()) 

# number of clutches per year and habitat
data_alan %>% 
  group_by(year, box) %>% 
  filter(row_number() == 1) %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n()) 

table(data_alan$dataset)

#####

## plot alan across sites
df_plot <- rbind(data_completed, data_completed02)

plot_alan <- ggplot(data = df_plot, aes(y = log(light_meter_average+1), x = area)) +
  #geom_boxplot(outliers = F) +
  geom_point(position = position_jitter(width = 0.1),
             size = 2.5, 
             shape = 21,
             fill = 'lightblue', 
             alpha = 0.5) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)) +
  labs(x = 'Habitat', y = 'Log Light at night (lux)')

ggsave(filename = './plots/plot_alan.png', 
       plot = plot_alan, 
       device = 'png',
       units = 'mm',
       width = 90, 
       height = 90)
##
##
##### Models for variation in night temperature including alan as predictor #####
##
##

# model fit
model_night_temp <- lmer(lognight_var ~ 
                           log(light_meter_average+1) +
                           
                           poly(inc_start_aprildays,2)[,2] +
                           poly(inc_start_aprildays,2)[,1] +
                           day_before_hatch +
                           
                           meantemp +
                           dataset +
                           clutch_size +
                           (1|year)+
                           (1|site) +
                           (1|box), 
                         REML = F,
                         na.action = "na.fail",
                         data= data_alan)
summary(model_night_temp)
drop1(model_night_temp, test = 'Chisq')

# model diagnostics
check_model(model_night_temp, panel = T) # not too bad overall

#####



## removing both interactions 
full_model <- model_night_temp
drop1(full_model, test = "Chisq")
summary(full_model)

# model diagnostics
check_model(full_model, panel = T) # not too bad overall

#####

##
##
##### Plot model predictions #####
##
##
full_model_predictions <- lmer(lognight_var ~ 
                                 log(light_meter_average+1) +
                                 
                                 poly(inc_start_aprildays,2)[,2] +
                                 poly(inc_start_aprildays,2)[,1] +
                                 day_before_hatch +
                                 
                                 meantemp +
                                 dataset +
                                 clutch_size +
                                 (1|year)+
                                 (1|site) +
                                 (1|box), 
                               REML = F,
                               na.action = "na.fail",
                               data= data_alan %>% filter(area == 'City')) #full model

summary(full_model_predictions)
drop1(full_model_predictions, test = "Chisq")

# new dataframe to predict
df_pred <- expand.grid(day_before_hatch = seq(min(data_alan$day_before_hatch), 
                                              max(data_alan$day_before_hatch), 1),
                       #area = c("City", "Forest"),
                       area = c("City"),
                       dataset = c('1', '2'),
                       meantemp = mean(data_alan$meantemp),
                       light_meter_average = seq(min(log(data_alan$light_meter_average+1)), 
                                                 max(log(data_alan$light_meter_average+1))+0.1, 0.05),
                       inc_start_aprildays = seq(min(data_alan$inc_start_aprildays), 
                                                 max(data_alan$inc_start_aprildays), 1))
df_pred$clutch_size <- NA
df_pred$clutch_size[df_pred$area == "Forest"] <- mean(data_alan$clutch_size[data_alan$area == "Forest"])
df_pred$clutch_size[df_pred$area == "City"] <- mean(data_alan$clutch_size[data_alan$area == "City"])

df_pred$prediction <- predict(full_model_predictions, df_pred, re.form = NA)

# plot only data in range
data_alan %>% 
  group_by(area) %>% 
  summarise(min_chr = min(log(data_alan$light_meter_average+1)),
            max_chr = max(log(data_alan$light_meter_average+1)))

remove_city <- which((df_pred$light_meter_average > 2.88) & 
                       df_pred$area == "City")
remove_forest <- which((df_pred$light_meter_average > 0.5) & 
                         df_pred$area == "Forest")
#df_pred <- df_pred[-c(remove_city, remove_forest),]

# SE for mean predictions
mm <- model.matrix(~  log(light_meter_average+1) +
                     
                     poly(inc_start_aprildays,2)[,2] +
                     poly(inc_start_aprildays,2)[,1] +
                     day_before_hatch +
                     
                     meantemp +
                     dataset +
                     clutch_size, 
                   data = df_pred)

pvar1 <- diag(mm %*% tcrossprod(vcov(full_model_predictions),mm))
cmult <- 1 ## 1 SE
df_pred <- data.frame(
  df_pred
  , plo = df_pred$prediction-cmult*sqrt(pvar1)
  , phi = df_pred$prediction+cmult*sqrt(pvar1)
)

alan_city <- ggplot(data = data_alan %>% 
         filter(area == 'City') %>% 
         mutate(light_meter_average = log(light_meter_average+1)), 
       aes(x = light_meter_average, 
           y = lognight_var,
           fill = area,
           color = area)) +
  geom_point(alpha = 0.25,
             size = 2.5,
             shape = 21,
             position = position_jitter(width = 0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.text = element_text("Arial", size = 10),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 12),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 10),
        axis.text = element_text("Arial", size = 10)) +
  geom_ribbon(data = df_pred %>% 
                group_by(area, light_meter_average) %>% 
                summarise(mean_plo = mean(plo),
                          mean_phi = mean(phi),
                          mean_pred = mean(prediction)), 
              aes(ymin = mean_plo, 
                  ymax = mean_phi, 
                  y = mean_pred),
              color = NA,
              alpha = 0.5) +
  geom_line(data = df_pred %>% 
              group_by(area, light_meter_average) %>% 
              summarise(mean_plo = mean(plo),
                        mean_phi = mean(phi),
                        mean_pred = mean(prediction)), 
            aes(y = mean_pred), 
            size = 1.5) +
  labs(x = "Log Light at night (lux)", 
       y = expression(atop("log Variation in night-time", 
                           "incubation temperature (ºC)"))) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito()

ggsave(filename = "./plots/Figure S1 City.png", 
       plot = alan_city, 
       device = "png", 
       units = "mm",
       width = 100, 
       height = 75)  
##
## plot for alan effect
alan_plot <- ggplot(data = data_alan %>% 
                      filter(area == 'City') %>% 
                      mutate(light_meter_average = log(light_meter_average+1)), 
                        aes(x = light_meter_average, 
                            y = lognight_var,
                            fill = area,
                            color = area)) +
  geom_point(alpha = 0.25,
             size = 2.5,
             shape = 21,
             position = position_jitter(width = 0.15)) +
  theme_bw() +
  #facet_wrap(~area) +
  theme(legend.position = "none",
        legend.text = element_text("Arial", size = 10),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 12),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 10),
        axis.text = element_text("Arial", size = 10)) +
  geom_ribbon(data = df_pred %>% 
                group_by(area, light_meter_average) %>% 
                summarise(mean_plo = mean(plo),
                         mean_phi = mean(phi),
                          mean_pred = mean(prediction)), 
              aes(ymin = mean_plo, 
                  ymax = mean_phi, 
                  y = mean_pred),
              color = NA,
              alpha = 0.5) +
  geom_line(data = df_pred %>% 
              #group_by(area, light_meter_average) %>% 
              summarise(mean_plo = mean(plo),
                        mean_phi = mean(phi),
                        mean_pred = mean(prediction)), 
            aes(y = mean_pred), 
            size = 1.5) +
  labs(x = "Light at night (lux)", 
       y = expression(atop("log Variation in night-time", 
                           "incubation temperature (ºC)"))) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito()

alan_plot

ggsave(filename = "./plots/Figure S1 Update.png", 
       plot = alan_plot, 
       device = "png", 
       units = "mm",
       width = 135, 
       height = 100)  



#####


##
##
##### Table of results#####
##
##

## base table
table_00 <- full_model_predictions %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `poly(inc_start_aprildays, 2)[, 1]`  = "Incubation start date", 
                   `poly(inc_start_aprildays, 2)[, 2]` = "Incubation start date2",
                   `day_before_hatch` = "Days before hatching",
                   `meantemp` = "Mean daily temperatures",
                   `log(light_meter_average + 1)` = "Light at night (log)",
                   `clutch_size` = "Clutch size",
                   `dataset` = "Light at night dataset"),
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
gtsave(table_nighttime_var, "./tables/TABLE S2.html")

#####

