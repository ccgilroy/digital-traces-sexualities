library(stringr)
library(tibble)
library(knitr)

# source("R/data_processing/national_data.R")
source("R/data_processing/relationship_data.R")

# function ----
make_descriptive_table <- function(df) {
  df_new <- 
    df %>%
    mutate(`Count, in millions` = round(estimate/1e6, 1), 
           `Percent` = str_c(format(round(fraction, 3)*100, nsmall = 1), "%")) %>%
    select(-estimate, -fraction) 
  colnames(df_new) <- c("trait", colnames(df_new)[2:3])
  
  df_new <- 
    df_new %>%
    mutate(trait = as.character(trait), 
           trait = str_c("\\ \\ \\ \\ ", trait))
  
  trait_name <- 
    colnames(df)[1] %>%
    str_to_title() %>%
    str_replace_all("_", " ") %>%
    str_c("**", ., "**")
  
  tibble(trait = trait_name,
         `Count, in millions` = NA_real_, 
         `Percent`  = NA_character_) %>%
    bind_rows(df_new)
}


# gender ----
g_table <- 
  d_us_rel_sex_top4 %>%
  group_by(gender) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(fraction = estimate/sum(estimate)) %>%
  make_descriptive_table() %>%
  tibble::add_row()

# sexuality ----
s_table <- 
  d_us_rel_sex_top4 %>%
  group_by(sexuality) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(fraction = estimate/sum(estimate)) %>%
  make_descriptive_table() %>%
  tibble::add_row()

# age ----
age_table <- 
  d_us_rel_sex_top4 %>%
  group_by(Age = case_when(
    age_min <= 24 ~ "18--24",
    age_min <= 34 ~ "25--34", 
    age_min <= 44 ~ "35--44", 
    age_min <= 54 ~ "45--54", 
    age_min <= 64 ~ "55--64")) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(fraction = estimate/sum(estimate)) %>%
  make_descriptive_table() %>%
  tibble::add_row()

# relationship status ----
rel_table <- 
  d_us_rel_sex_top4 %>%
  group_by(relationship_status) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(relationship_status = fct_lump(
    relationship_status, n = 4, w = .$estimate,
    other_level = "(All other)"
  )) %>%
  group_by(relationship_status) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(fraction = estimate/sum(estimate)) %>%
  make_descriptive_table()  %>%
  tibble::add_row()

total_table <- 
  d_us_rel_sex %>%
  filter(gender != "all") %>%
  mutate(trait = ifelse(age_min < 65 &
                          relationship_status %in% c("single", 
                                                     "in a relationship", 
                                                     "married", 
                                                     "not specified"), 
                        "**Total included in analysis**", 
                        "*Total excluded from analysis*")) %>%
  group_by(trait) %>%
  summarize(estimate = sum(estimate)) %>%
  mutate(`Count, in millions` = round(estimate/1e6, 1), 
         `Percent`  = ifelse(str_detect(trait, "included"),
                             "100.0%", 
                             NA_character_)) %>%
  select(-estimate)

# double check those exclusions with this crosstab:
d_us_rel_sex %>%
  filter(gender != "all") %>%
  mutate(over65 = age_min == 65, 
         rare_rel = !(relationship_status %in% c("single", 
                                                 "in a relationship", 
                                                 "married", 
                                                 "not specified"))) %>%
  group_by(over65, rare_rel) %>%
  summarize(estimate = sum(estimate))

d_us_rel_sex %>%
  # filter(gender != "all") %>%
  mutate(over65 = age_min == 65, 
         rare_rel = !(relationship_status %in% c("single", 
                                                 "in a relationship", 
                                                 "married", 
                                                 "not specified"))) %>%
  group_by(gender, over65, rare_rel) %>%
  summarize(estimate = sum(estimate))
 
# combine and format table ----
options(knitr.kable.NA = '')
# bind_rows(s_table, rel_table, g_table, age_table) %>%
#   knitr::kable(format = "pandoc", col.names = c("", colnames(.[2:3])))

bind_rows(s_table, g_table, rel_table, age_table, total_table) %>%
  mutate(`Count, in millions` = as.character(`Count, in millions`)) %>%
  knitr::kable(format = "markdown",
               col.names = c("", colnames(.[2:3])), 
               align = c("l", "r", "r"), 
               caption = "Descriptive statistics")

# bind_rows(s_table, rel_table, g_table, age_table) %>%
#   pander::pander()
