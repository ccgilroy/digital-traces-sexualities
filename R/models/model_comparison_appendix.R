# load packages and relationship data ----
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)
library(broom)
library(ggeffects)
library(MASS, pos = 99)

source("R/data_processing/relationship_data.R")

d <- 
  d_us_rel_sex_top4 %>%
  mutate(relationship_status = fct_drop(relationship_status)) %>%
  mutate(age = age_min) %>%
  select(gender, age, relationship_status, sexuality, estimate)

model_id <- c(
  "M1", 
  "M2", 
  "M3", 
  "M4", 
  "M5", 
  "M6", 
  "M7", 
  "M8", 
  "M9", 
  "M10", 
  "M11", 
  "M12", 
  "M13", 
  "M14", 
  "M15", 
  "M16"
)
model_notation <- c(
  "(Intercept only)",
  "S, G, R, A", 
  "SG, SR, GR, SA, GA, RA", 
  "SGR, SA, GA, RA",
  "GRA, SG, SR, SA",
  "SGA, SR, GR, RA",  
  "SRA, SG, GR, GA", 
  # "SGA, SRA, GR",
  "SGR, SGA, GRA", 
  "SGR, SRA, GRA",
  "SGR, SGA, SRA",
  "SGA, SRA, GRA",
  "SGR, SGA, SRA, GRA", 
  "SGR, SGA, SRA, GRA", 
  "SGR, SGA, SRA, GRA", 
  "SGRA", 
  "SGRA"
)
model_description <- c(
  "Null model", 
  "No interactions", 
  "All two-way interactions", 
  "One three-way interaction", 
  "One three-way interaction", 
  "One three-way interaction", 
  "One three-way interaction", 
  # "Two three-way interactions", 
  "Three three-way interactions",
  "Three three-way interactions",
  "Three three-way interactions",
  "Three three-way interactions",
  "All three-way interactions",
  "All three-way interactions",
  "All three-way interactions",
  "Four-way interaction", 
  "Saturated model"
)
age_encoding <- c(
  "---", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "5th-order polynomial", 
  "3rd-order polynomial", 
  "5th-order polynomial", 
  "Categorical", 
  "5th-order polynomial", 
  "Categorical"
)

model_formula <- c(
  estimate ~ 1, 
  estimate ~ sexuality + relationship_status + gender + poly(age, 5),
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2,
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:gender:relationship_status,
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + gender:relationship_status:poly(age, 5),
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:gender:poly(age, 5),
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:relationship_status:poly(age, 5),
  # estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:relationship_status:poly(age, 5) + sexuality:gender:poly(age, 5),
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:gender:relationship_status + sexuality:gender:poly(age, 5) + gender:relationship_status:poly(age, 5), # SGR, SGA, GRA
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:gender:relationship_status + sexuality:relationship_status:poly(age, 5) + gender:relationship_status:poly(age, 5), # SGR, SRA, GRA
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:gender:relationship_status + sexuality:gender:poly(age, 5) + sexuality:relationship_status:poly(age, 5), # SGR, SGA, SRA
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^2 + sexuality:relationship_status:poly(age, 5) + sexuality:gender:poly(age, 5) + gender:relationship_status:poly(age, 5), # SGA, SRA, GRA
  estimate ~ (sexuality + relationship_status + gender + poly(age, 3))^3,
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^3,
  estimate ~ (sexuality + relationship_status + gender + as.factor(age))^3,
  estimate ~ (sexuality + relationship_status + gender + poly(age, 5))^4,
  estimate ~ (sexuality + relationship_status + gender + as.factor(age))^4
)

d_models <- 
  tibble(
    id = model_id, 
    notation = model_notation, 
    description = model_description,
    age_encoding, 
    formula = model_formula
  )

d_fits <- 
  d_models %>%
  mutate(fit = map_if(formula, description != "Saturated model", glm.nb, data = d, 
                      .else = ~NULL))

maybe_map_dbl <- function(x, f, e = NA_real_) {
  flatten_dbl(map_if(x, ~!is.null(.), f, .else = ~e))
}

d_fits_stats <-
  d_fits %>%
  mutate(parameters = maybe_map_dbl(fit, ~length(coef(.)), nrow(d)), 
         deviance = maybe_map_dbl(fit, deviance, 0), 
         AIC = maybe_map_dbl(fit, AIC), 
         BIC = maybe_map_dbl(fit, BIC))

options(knitr.kable.NA = "---")

# default separation is every 5 rows
# c('', '', '', '', '\\addlinespace')
line_separation <- c(
  '\\addlinespace', 
  '\\addlinespace', 
  '\\addlinespace', 
  '', 
  '', 
  '', 
  '\\addlinespace', 
  '', 
  '', 
  '', 
  '\\addlinespace', 
  '', 
  '', 
  '\\addlinespace', 
  '',
  ''
)

# for latex/pdf
d_fits_stats %>%
  select(-formula, -fit, -AIC) %>%
  knitr::kable(col.names  = c(
    "Model", 
    "Notation", 
    "Description", 
    "Age encoding", 
    "Parameters", 
    "Deviance", 
    "BIC"
  ), 
  format = "latex", 
  longtable = TRUE, booktabs = TRUE,
  caption = "Model comparisons", label = "A2", digits = 2, 
  linesep = line_separation) %>%
  kableExtra::landscape() %>%
  kableExtra::row_spec(13, background = "gray!20") 

# for word document
# markdown is easier than html
# manually added the row separation
d_fits_stats %>%
  select(-formula, -fit, -AIC) %>%
  knitr::kable(col.names  = c(
    "Model", 
    "Notation", 
    "Description", 
    "Age encoding", 
    "Parameters", 
    "Deviance", 
    "BIC"
  ), 
  format = "markdown", 
  # longtable = TRUE, booktabs = TRUE,
  caption = "Model comparisons", label = "A2", digits = 2) 
  # kableExtra::kable_styling() %>%
  # # kableExtra::landscape() %>%
  # # kableExtra::kable_classic(full_width = F, html_font = "Cambria")
  # kableExtra::row_spec(13, background = "gray!20") 

# kableExtra::add_footnote() for adding notes to tables
  
# kableExtra::column_spec(1, bold = (d_fits_stats$id == "M13"))
# kableExtra::kable_styling(latex_options = "striped",
#                           stripe_index = 13)

# https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html
# https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
