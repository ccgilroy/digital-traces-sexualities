#' ---
#' purpose: make proportions & recode categories for US-level sexuality data
#' input: data/US_int_rel_1yr.csv
#' output: 
#' ---

library(readr)
library(dplyr)
library(forcats)
library(stringr)
library(here)

# data set: relationship statuses, with sexuality ----
# load data ----
d_us_rel_sex <- read_csv(here("data/US_int_rel_1yr.csv"))

# filter and recode ----
d_us_rel_sex_sub <- 
  d_us_rel_sex %>%
  filter(gender != "all", age_min < 65)

d_us_rel_sex_sub_recoded <- 
  d_us_rel_sex_sub %>%
  mutate(
    sexuality = case_when(
      (gender == "female" & interested_in == "women") | 
        (gender == "male" & interested_in == "men") ~ "Interested in same gender",
      (gender == "female" & interested_in == "men") |
        (gender == "male" & interested_in == "women") ~ "Interested in different gender",
      interested_in == "men and women" ~ "Interested in men and women",
      interested_in == "not specified" ~ "Not specified",
      TRUE ~ NA_character_), 
    sexuality = as.factor(sexuality), 
    sexuality = fct_relevel(sexuality,
                            "Interested in same gender", 
                            "Interested in men and women", 
                            "Interested in different gender", 
                            "Not specified"))%>%
  mutate(relationship_status = as_factor(relationship_status), 
         gender = fct_recode(gender, Women = "female", Men = "male"), 
         relationship_status = fct_recode(
           relationship_status, 
           `Not specified` = "not specified", 
           `Married` = "married", 
           `Single` = "single", 
           `In a relationship` = "in a relationship"
         ))

# filter to top 4 ----
top4 <- c("Not specified", "Married", "Single", "In a relationship")

d_us_rel_sex_top4 <- 
  d_us_rel_sex_sub_recoded %>%
  filter(relationship_status %in% top4) 

# aggregate, no ages ----
d_us_rel_sex_top4_noage <- 
  d_us_rel_sex_top4 %>%
  group_by(relationship_status, sexuality) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  group_by(relationship_status) %>%
  mutate(total = sum(estimate), 
         fraction = estimate/total)

# aggregate, age bins ----
d_us_rel_sex_top4_withage <- 
  d_us_rel_sex_top4 %>%
  mutate(Age = case_when(
    age_min <= 30 ~ "18—30",
    age_min <= 40 ~ "31—40", 
    age_min <= 50 ~ "41—50", 
    age_min <= 64 ~ "51—64")) %>%
  group_by(relationship_status, sexuality, Age) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup() %>%
  group_by(relationship_status, Age) %>%
  mutate(total = sum(estimate), 
         fraction = estimate/total)
