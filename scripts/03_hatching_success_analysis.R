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
               lme4, performance, rptR, DHARMa,
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
df00 <- readRDS("./data/data_incubation.RDS")
head(df00)

# create dataset for nestbox-level analysis
data_means <- df00 %>% 
  filter(!is.na(night_var)) %>% 
  group_by(year, box) %>% 
  summarise(mean_nighttime_var = mean(night_var, rm.na = T),
            nights_for_mean = n())


data_nest <- df00 %>% 
  group_by(year, box) %>% 
  filter(row_number() == 1)

data <- left_join(x = data_nest, 
                  y = data_means, 
                  by = c('year', 'box'))

table(is.na(data$mean_nighttime_var))
table(is.na(data$clutch_size))
table(is.na(data$hatchlings))

data <- data %>% 
  filter(!is.na(mean_nighttime_var))

#####

##
##
##### Sample sizes and data summary #####
##
##
head(data)
nrow(data) # total observations
length(unique(data$box)) # number of nest-boxes included

# number of clutches
data %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n())

# number of clutches per year and habitat
data %>% 
  group_by(year, box) %>% 
  filter(row_number() == 1) %>% 
  group_by(year, area) %>% 
  summarise(n_obs = n()) 



#####

##
##
##### Models for variation in night temperature across sites #####
##
##

summary(data$night_var)
data$no_hatch <- data$clutch_size - data$hatchlings


# model fit
model_hatching <- glmer(cbind(hatchlings, no_hatch) ~ 
                          poly(inc_start_aprildays,2)[,2] : area +
                          poly(inc_start_aprildays,2)[,1] : area +
                          
                          area:mean_nighttime_var +
                          
                          mean_nighttime_var +
                          poly(inc_start_aprildays,2)[,2] +
                          poly(inc_start_aprildays,2)[,1] +
                          meantemp +
                          area + 
                          (1|year)+
                          (1|site) +
                          (1|box), 
                        control = glmerControl(optimizer = 'bobyqa'),
                        family = 'binomial',
                        na.action = "na.fail",
                        data= data) #full model
summary(model_hatching)

# model diagnostics
simul_res <- simulateResiduals(model_hatching, n = 1000) # good fit
testResiduals(simul_res, plot = T)

#####

##
##
##### Are interactions significant? #####
##
##

## 1
drop1(model_hatching, test = "Chisq")

## interaction 1
anova(model_hatching, 
      update(model_hatching, 
             . ~ . - poly(inc_start_aprildays, 2)[, 2]:area),
      test = "LRT")

## 2
model_hatching2 <- update(model_hatching, .~.-poly(inc_start_aprildays, 2)[, 2]:area)
drop1(model_hatching2, test = "Chisq")

## interaction 2
anova(model_hatching2, update(model_hatching2, 
                                . ~ . - poly(inc_start_aprildays, 2)[, 1]:area),
      test = "LRT")

## 3
model_hatching3 <- update(model_hatching2, .~.-poly(inc_start_aprildays, 2)[, 1]:area)
drop1(model_hatching3, test = "Chisq")

## interaction 3
anova(model_hatching3, update(model_hatching3, 
                              . ~ . - poly(inc_start_aprildays, 2)[, 2]),
      test = "LRT")

## 4
model_hatching4 <- update(model_hatching3, .~.-poly(inc_start_aprildays, 2)[, 2])
drop1(model_hatching4, test = "Chisq")

## interaction 4
anova(model_hatching4, update(model_hatching4, 
                              . ~ . - mean_nighttime_var:area),
      test = "LRT")

## 5
model_hatching5 <- update(model_hatching4, .~.-mean_nighttime_var:area)
drop1(model_hatching5, test = "Chisq")


## removing both interactions 
full_model <- model_hatching5
drop1(full_model, test = "Chisq")
summary(full_model)
boot::inv.logit(-0.935)

#####

##
##
##### Plot model predictions #####
##
##
full_model_predictions <- full_model
summary(full_model_predictions)
drop1(full_model_predictions, test = "Chisq")

# new dataframe to predict
df_pred <- expand.grid(mean_nighttime_var = seq(min(data$mean_nighttime_var), 
                                              max(data$mean_nighttime_var), 0.25),
                       area = c("City", "Forest"),
                       meantemp = mean(data$meantemp),
                       inc_start_aprildays = seq(min(data$inc_start_aprildays), 
                                                 max(data$inc_start_aprildays), 1))
df_pred$clutch_size <- NA
df_pred$clutch_size[df_pred$area == "Forest"] <- mean(data$clutch_size[data$area == "Forest"])
df_pred$clutch_size[df_pred$area == "City"] <- mean(data$clutch_size[data$area == "City"])

df_pred$prediction <- predict(full_model_predictions, 
                              df_pred, 
                              re.form = NA, 
                              type = 'link')

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
mm <- model.matrix(~ 
                     mean_nighttime_var +
                     poly(inc_start_aprildays,2)[,1] +
                     meantemp +
                     area,
                   data = df_pred)
pvar1 <- diag(mm %*% tcrossprod(vcov(full_model_predictions),mm))
cmult <- 1 ## 1 SE
df_pred <- data.frame(
  df_pred
  , plo = df_pred$prediction-cmult*sqrt(pvar1)
  , phi = df_pred$prediction+cmult*sqrt(pvar1)
)

df_pred$predictions_response <- boot::inv.logit(df_pred$prediction)
df_pred$plo_response <- boot::inv.logit(df_pred$plo)
df_pred$phi_response <- boot::inv.logit(df_pred$phi)

##
## plot for date from April 1
hatching_var_plot <- ggplot(data = data, 
                        aes(x = mean_nighttime_var, 
                            y = hatchlings/clutch_size,
                            fill = area,
                            color = area)) +
  geom_point(alpha = 0.25,
             size = 2.5,
             shape = 21,
             position = position_jitter(width = 0.15)) +
  theme_bw() +
  facet_wrap(~area) +
  theme(legend.position = "none",
        legend.text = element_text("Arial", size = 10),
        strip.background = element_blank(),
        strip.text = element_text("Arial", size = 15),
        panel.grid = element_blank(),
        axis.title = element_text("Arial", size = 14),
        axis.text = element_text("Arial", size = 12)) +
  geom_ribbon(data = df_pred %>% 
                group_by(area, mean_nighttime_var) %>% 
                summarise(mean_plo = mean(plo_response),
                          mean_phi = mean(phi_response),
                          mean_pred = mean(predictions_response)), 
              aes(ymin = mean_plo, 
                  ymax = mean_phi, 
                  y = mean_pred),
              color = NA,
              alpha = 0.5) +
  geom_line(data = df_pred %>% 
              group_by(area, mean_nighttime_var) %>% 
              summarise(mean_plo = mean(plo_response),
                        mean_phi = mean(phi_response),
                        mean_pred = mean(predictions_response)), 
            aes(y = mean_pred), 
            size = 1.5) +
  labs(x = bquote("Variation in night-time incubation temperature "~ (ºC^2)), 
       y = 'Hatching success') +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito()

hatching_var_plot

ggsave(filename = "./plots/Figure 2.png", 
       plot = hatching_var_plot, 
       device = "png", 
       units = "mm",
       width = 135, 
       height = 100)  

#####


##
##
##### Table of results 2 #####
##
##

## base table
table_00 <- full_model_predictions %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `poly(inc_start_aprildays, 2)[, 1]`  = "Incubation start date", 
                   `mean_nighttime_var` = "Variation in night-time incubation temperature",
                   `meantemp` = "Mean daily temperatures",
                   `area` = "Habitat"),
                 estimate_fun = ~ style_number(.x, digits = 3))

## add features
table_hatching_var <- table_00 %>% 
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
  modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 2)) %>%
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
gtsave(table_hatching_var, "./tables/TABLE 2.html")

#####

