# DisgustSensitivity
Analysis code for quantitative project examining how moral, sexual, and pathogen disgust can predict stigmatization towards mental illness.


---
title: "Quant2023 Analysis"
author: "Meg Giatzis"
date: '2023-10-19'
output: html_document
---
Do pathogen disgust scores (with moral and sexual fed in as exploratory) predict stigma scores towards different mental health conditions? 

Loading Packages and Data Wrangling 

```{r}
library(tidyverse)
library(broom)
library(psych)
library(see)
library(performance)
library(pwr)
library(ltm)
library(ggplot2)
library(patchwork)
options("scipen" = 10, "digits" = 4)
```

```{r}
rawdata <- read_csv("project824.csv")
```

```{r}
data <- rawdata %>%
  group_by(user_id, q_id) %>%
  filter(session_id == min(session_id), endtime == min(endtime)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(user_status %in% c("guest", "registered")) %>%
  dplyr::select(user_id, session_id, q_name, dv, user_sex, user_age) %>%
  pivot_wider(names_from = q_name, values_from = dv) %>%
  drop_na()
```

```{r}
fdemog <- data %>% 
  dplyr::select(user_id, user_sex, user_age) %>% 
  filter(user_sex == "female") %>% 
  summarise(min_age = min(user_age), max_age = max(user_age), mean_age = mean(user_age), sd_age = sd(user_age), sum = n())

mdemog <- data %>% 
  dplyr::select(user_id, user_sex, user_age) %>% 
  filter(user_sex == "male") %>% 
  summarise(min_age = min(user_age), max_age = max(user_age), mean_age = mean(user_age), sd_age = sd(user_age), sum = n())

nbdemog <- data %>% 
  dplyr::select(user_id, user_sex, user_age) %>% 
  filter(user_sex == "nonbinary") %>% 
  summarise(mean_age = mean(user_age), sd_age = sd(user_age), sum = n())
```


```{r}
pathogen <- data %>%
            dplyr::select(user_id, PD1, PD2, PD3, PD4, PD5, PD6, PD7) 

pathogen_long <- 
  pivot_longer(pathogen,
                         cols = c(2:8),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_path = sum(Response)) %>%
  ungroup()


moral <- data %>%
            dplyr::select(user_id, MD1, MD2, MD3, MD4, MD5, MD6, MD7) 

moral_long <- 
  pivot_longer(moral,
                         cols = c(2:8),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_moral = sum(Response)) %>%
  ungroup()


sexual <- data %>%
            dplyr::select(user_id, SD1, SD2, SD3, SD4, SD5, SD6, SD7) 

sexual_long <- 
  pivot_longer(sexual,
                         cols = c(2:8),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_sexual = sum(Response)) %>%
  ungroup()
```

```{r}
depression <- data %>%
            dplyr::select(user_id, DEP_Q1, DEP_Q2, DEP_Q3, DEP_Q4, DEP_Q5, DEP_Q6, DEP_Q7, DEP_Q8, DEP_Q9, DEP_Q10, DEP_Q11, DEP_Q12, DEP_Q13, DEP_Q14, DEP_Q15, DEP_Q16, DEP_Q17, DEP_Q18, DEP_Q19, DEP_Q20, DEP_Q21, DEP_Q22, DEP_Q23, DEP_Q24, DEP_Q25, DEP_Q26, DEP_Q27)

dep_long <- 
  pivot_longer(depression,
                         cols = c(2:28),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_dep = sum(Response)) %>%
  ungroup()


schiz <- data %>%
            dplyr::select(user_id, SCHIZ_Q1, SCHIZ_Q2, SCHIZ_Q3, SCHIZ_Q4, SCHIZ_Q5, SCHIZ_Q6, SCHIZ_Q7, SCHIZ_Q8, SCHIZ_Q9, SCHIZ_Q10, SCHIZ_Q11, SCHIZ_Q12, SCHIZ_Q13, SCHIZ_Q14, SCHIZ_Q15, SCHIZ_Q16, SCHIZ_Q17, SCHIZ_Q18, SCHIZ_Q19, SCHIZ_Q20, SCHIZ_Q21, SCHIZ_Q22, SCHIZ_Q23, SCHIZ_Q24, SCHIZ_Q25, SCHIZ_Q26, SCHIZ_Q27)

schiz_long <- 
  pivot_longer(schiz,
                         cols = c(2:28),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_schiz = sum(Response)) %>%
  ungroup()


gad <- data %>%
            dplyr::select(user_id, GAD_Q1, GAD_Q2, GAD_Q3, GAD_Q4, GAD_Q5, GAD_Q6, GAD_Q7, GAD_Q8, GAD_Q9, GAD_Q10, GAD_Q11, GAD_Q12, GAD_Q13, GAD_Q14, GAD_Q15, GAD_Q16, GAD_Q17, GAD_Q18, GAD_Q19, GAD_Q20, GAD_Q21, GAD_Q22, GAD_Q23, GAD_Q24, GAD_Q25, GAD_Q26, GAD_Q27) 

gad_long <- 
  pivot_longer(gad,
                         cols = c(2:28),
                         names_to = "Question",
                         values_to = "Response") %>%
                        mutate(Response = as.numeric(Response)) %>%
            group_by(user_id) %>%
            summarise(total_gad = sum(Response)) %>%
  ungroup()
```

Joining Data and Mean Centering 

```{r}
data1 <- inner_join(dep_long, gad_long, by = "user_id")
data2 <- inner_join(schiz_long, moral_long, by = "user_id")
data3 <- inner_join(sexual_long, pathogen_long, by = "user_id")

data4 <- inner_join(data1, data2, by = "user_id") 

joined_data <- inner_join(data3, data4, by = "user_id") 

TDDScentered <- joined_data %>% 
  mutate(path_c = total_path - mean(total_path),
         moral_c = total_moral - mean(total_moral),
         sex_c = total_sexual - mean(total_sexual)) %>% 
  dplyr::select(user_id, total_gad, total_dep, total_schiz, path_c, moral_c, sex_c)
```

Descriptive Statistics 

```{r}
path_means <- pathogen_long %>% 
  summarise(mean_path = mean(total_path), sd_path = sd(total_path))

sex_means <- sexual_long %>% 
  summarise(mean_sex = mean(total_sexual), sd_sex = sd(total_sexual))

moral_means <- moral_long %>% 
  summarise(mean_moral = mean(total_moral), sd_moral = sd(total_moral))

gad_means <- gad_long %>% 
  summarise(mean_gad = mean(total_gad), sd_gad = sd(total_gad))

dep_means <- dep_long %>% 
  summarise(mean_dep = mean(total_dep), sd_dep = sd(total_dep))

schiz_means <- schiz_long %>% 
  summarise(mean_schiz = mean(total_schiz), sd_schiz = sd(total_schiz))
```

```{r}
descriptives <- describe(joined_data)
```

Data Visualizations

```{r}
TDDScentered_mutated <- TDDScentered %>%
  pivot_longer(cols = 2:4, names_to = "disorder", values_to = "scores") %>% 
  mutate(disorder = dplyr::recode(disorder,
                                  "total_schiz" = "Schizophrenia",
                                  "total_gad" = "Generalized Anxiety Disorder",
                                  "total_dep" = "Depression"))

```

```{r}
pplot1 <- ggplot(TDDScentered, aes(x = path_c, y = total_schiz, fill = path_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Pathogen Disgust Score", y = "Schizophrenia Stigma Score") +
  theme(legend.position = "none") 

pplot2 <- ggplot(TDDScentered, aes(x = path_c, y = total_gad, fill = path_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Pathogen Disgust Score", y = "Generalized Anxiety Disorder Stigma Score") +
   theme(legend.position = "none")

pplot3 <- ggplot(TDDScentered, aes(x = path_c, y = total_dep, fill = path_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Pathogen Disgust Score", y = "Depression Stigma Score") +
   theme(legend.position = "none")

splot1 <- ggplot(TDDScentered, aes(x = sex_c, y = total_schiz, fill = sex_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Sexual Disgust Score", y = "Schizophrenia Stigma Score") +
   theme(legend.position = "none")

splot2 <- ggplot(TDDScentered, aes(x = sex_c, y = total_gad, fill = sex_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Sexual Disgust Score", y = "Generalized Anxiety Disorder Stigma Score") +
   theme(legend.position = "none")

splot3 <- ggplot(TDDScentered, aes(x = sex_c, y = total_dep, fill = sex_c)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Sexual Disgust Score", y = "Depression Stigma Score") +
   theme(legend.position = "none")

combined_plots_path <- pplot1 + pplot2 + pplot3 
combined_plots_path + plot_layout(ncol = 3) 

combined_plots_sex <- splot1 + splot2 + splot3
combined_plots_sex + plot_layout(ncol = 3) 
```

Multiple Linear Regression Analysis 

```{r}
gadmod <- lm(total_gad ~ path_c + moral_c + sex_c, TDDScentered)

summary(gadmod)

gadmod_summary <- summary(gadmod)
```

```{r}
depmod <- lm(total_dep ~ path_c + moral_c + sex_c, TDDScentered)

summary(depmod)

depmod_summary <- summary(depmod)
```

```{r}
schizmod <- lm(total_schiz ~ path_c + moral_c + sex_c, TDDScentered)

summary(schizmod)

schizmod_summary <- summary(schizmod)
```
Assumption Checking 

```{r}
check_model(gadmod)

check_normality(gadmod)

check_heteroscedasticity(gadmod)

check_collinearity(gadmod)
```

```{r}
check_model(depmod)

check_normality(depmod)

check_heteroscedasticity(depmod)

check_collinearity(depmod)
```

```{r}
check_model(schizmod)

check_normality(schizmod)

check_heteroscedasticity(schizmod)

check_collinearity(schizmod)
```

Measuring Internal Consistency 

```{r}
path2 <- data %>%   
  dplyr::select(PD1,PD2, PD3, PD4, PD5, PD6, PD7)   

cronbach_p <-cronbach.alpha(path2) 

cronbach_p

sex2 <- data %>%   
  dplyr::select(SD1, SD2, SD3, SD4, SD5, SD6, SD7)   

cronbach_sex <-cronbach.alpha(sex2) 

cronbach_sex

moral2 <- data %>%   
  dplyr::select(MD1, MD2, MD3, MD4, MD5, MD6, MD7)   

cronbach_m <-cronbach.alpha(moral2) 

cronbach_m
```

```{r}
schiz2 <- data %>%   
  dplyr::select(SCHIZ_Q1, SCHIZ_Q2, SCHIZ_Q3, SCHIZ_Q4, SCHIZ_Q5, SCHIZ_Q6, SCHIZ_Q7, SCHIZ_Q8, SCHIZ_Q9, SCHIZ_Q10, SCHIZ_Q11, SCHIZ_Q12, SCHIZ_Q13, SCHIZ_Q14, SCHIZ_Q15, SCHIZ_Q16, SCHIZ_Q17, SCHIZ_Q18, SCHIZ_Q19, SCHIZ_Q20, SCHIZ_Q21, SCHIZ_Q22, SCHIZ_Q23, SCHIZ_Q24, SCHIZ_Q25, SCHIZ_Q26, SCHIZ_Q27)   

cronbach_schiz <-cronbach.alpha(schiz2) 

cronbach_schiz

dep2 <- data %>%   
  dplyr::select(DEP_Q1, DEP_Q2, DEP_Q3, DEP_Q4, DEP_Q5, DEP_Q6, DEP_Q7, DEP_Q8, DEP_Q9, DEP_Q10, DEP_Q11, DEP_Q12, DEP_Q13, DEP_Q14, DEP_Q15, DEP_Q16, DEP_Q17, DEP_Q18, DEP_Q19, DEP_Q20, DEP_Q21, DEP_Q22, DEP_Q23, DEP_Q24, DEP_Q25, DEP_Q26, DEP_Q27)   

cronbach_dep <-cronbach.alpha(dep2) 

cronbach_dep

gad2 <- data %>%   
  dplyr::select(GAD_Q1, GAD_Q2, GAD_Q3, GAD_Q4, GAD_Q5, GAD_Q6, GAD_Q7, GAD_Q8, GAD_Q9, GAD_Q10, GAD_Q11, GAD_Q12, GAD_Q13, GAD_Q14, GAD_Q15, GAD_Q16, GAD_Q17, GAD_Q18, GAD_Q19, GAD_Q20, GAD_Q21, GAD_Q22, GAD_Q23, GAD_Q24, GAD_Q25, GAD_Q26, GAD_Q27)   

cronbach_gad <-cronbach.alpha(gad2) 

cronbach_gad
```

Calculating Minimum Effect Size

```{r}
pwr.f2.test(u = 3, v = 87 - 3 - 1, f2 = NULL, sig.level = .05, power = .80)
```
```{r}
gadf2 <- gadmod_summary$adj.r.squared/(1 - gadmod_summary$adj.r.squared)

gadf2

depf2 <- depmod_summary$adj.r.squared/(1 - depmod_summary$adj.r.squared)

depf2

schizf2 <- schizmod_summary$adj.r.squared/(1 - schizmod_summary$adj.r.squared)

schizf2
```
