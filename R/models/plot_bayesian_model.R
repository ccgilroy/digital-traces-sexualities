library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(tidybayes)
library(patchwork)

# load data and model ----
source("R/data_processing/relationship_data.R")
d <- 
  d_us_rel_sex_top4 %>%
  mutate(relationship_status = fct_drop(relationship_status)) %>%
  mutate(age = age_min) %>%
  select(gender, age, relationship_status, sexuality, estimate)

fit_bayes <- read_rds(here("output/intermediate/fit_bayes.rds"))

# add predicted draws ----
d_y_rep <- 
  d %>%
  add_predicted_draws(fit_bayes) %>%
  mutate(disclosed = sexuality != "Not specified")

# colors and themes ----
# unified color scheme for figures: solarized color palette
solarized_8 <- ggthemes::solarized_pal()(8)
violet <- solarized_8[6] # single-color
red_cyan <- solarized_8[c(4, 7)] # two colors: gender
tetrad <- solarized_8[c(1, 5, 3, 8)] # four colors: relationship status

# theme based on Claus Wilke's cowplot
theme_dts <- function(...) {
  cowplot::theme_minimal_grid(...) + 
    theme(plot.title.position = "plot", 
          legend.position = "top", 
          legend.justification = "left", 
          panel.spacing = unit(12, "points"), 
          axis.ticks.y = element_blank(),
          axis.ticks.x.top = element_blank(), 
          axis.text.x.top = element_blank(), 
          plot.margin = unit(c(7, 21, 7, 7), "pt")) 
}

# note: figure 1 is a set of screenshots

# figure 2: disclosure ----
# panel a: who discloses their sexuality on Facebook? 
d_disclosed <- 
  d_y_rep %>% 
  group_by(disclosed, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(.draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction)) %>%
  ungroup() 

# note: the model proportion is exactly equal to the data proportion
p_disclosure_a <- 
  d_disclosed %>%
  group_by(disclosed, prop_data, estimate) %>%  
  point_interval(prop_model, .point = median, .interval = qi) %>%
  # filter(disclosed) %>%
  ggplot(aes(x = prop_model, y = 0)) + 
  geom_col(aes(x = 1), alpha = .2, fill = "grey90", color = "transparent", 
           width = .4, orientation = "y", data = ~filter(., disclosed)) +
  geom_col(alpha = .6, width = .4,  orientation = "y",
           fill = violet,
           data = ~filter(., disclosed)) + 
  geom_linerange(aes(xmin = .lower, xmax = .upper), 
                 color = violet, size = .75,
                 data = ~filter(., disclosed)) +
  geom_text(aes(label = str_c(signif(estimate/1e6, 3), " million")), 
            x = c(.283 +  0.717/2, .283/2),
            color = c("black", "white"),
            hjust = .5, 
            size = 4.5) +
  annotate(geom = "text", x = .283/2, y = .6, 
           label = "Facebook users disclosing\nany 'interested in' information", 
           size = 3) +
  annotate(geom = "text", x = .283 +  0.717/2, y = .5, 
           label = "Facebook users leaving 'interested in' unspecified", 
           size = 3) +
  scale_x_continuous(labels = scales::percent, 
                     expand = c(0, 0), 
                     limits = c(0, 1),
                     breaks = c(0, 0.283, 1)) +
  scale_y_continuous(limits = c(-.25, .8), 
                     breaks = c(0), 
                     labels = c("Any disclosure")) +
  labs(title = "(a) Proportion of US Facebook users disclosing any sexuality", 
       y = NULL, 
       x = NULL) +
  theme_dts() + 
  theme(panel.grid = element_blank())

# panel b: sexual identities disclosed 
d_sexualities <- 
  d_y_rep %>% 
  group_by(sexuality, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(.draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction)) %>%
  ungroup() 

p_disclosure_b <- 
  d_sexualities  %>%
  filter(sexuality != "Not specified") %>%
  group_by(sexuality, prop_data, estimate) %>%  
  point_interval(.prediction, .point = median, .interval = qi) %>%
  arrange(sexuality) %>%
  mutate(sexuality = str_to_sentence(str_remove(sexuality, "Interested in ")), 
         sexuality = as_factor(sexuality)) %>%
  ggplot(aes(x = estimate, y = fct_rev(sexuality))) +
  geom_col(alpha = .6, width = .6, 
           fill = violet) + 
  geom_linerange(aes(xmin = .lower, xmax = .upper), 
                 color = violet, size = .75) +
  geom_text(aes(label = str_c(signif(estimate/1e6, 3), " million")),
            color = "black",
            hjust = 0,
            nudge_x = 5e5,
            data = function(x) filter(x, sexuality %in% c(
              "Same gender",
              "Men and women"
            ))) +
  geom_text(aes(label = str_c(signif(estimate/1e6, 3), " million")),
            x = 49.5e6,
            color = "#FFFFFF",
            hjust = 1,
            data = function(x) filter(x, sexuality %in% c(
              "Different gender"
            ))) +
  scale_x_continuous(labels = function(x) str_c(x/1e6, "M"), 
                     expand = c(0, 0)) +
  labs(y = NULL, x = "Estimated count",
       title = "(b) Sexual identities disclosed by US Facebook users", 
       subtitle = "Interested in ...") + 
  theme_dts() + 
  theme(panel.grid.major.y = element_blank())

p_disclosure <- 
  p_disclosure_a / p_disclosure_b + 
  plot_layout(heights = c(1, 2)) 

# figure 3: propensity to disclose sexuality by age ----
d_sa <- 
  d_y_rep %>%
  group_by(disclosed, age, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(age, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

p_age <- 
  d_sa %>% 
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(age, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ggplot(aes(x = age, y = prop_model)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = .4, 
              fill = violet) + 
  geom_line(color = violet, size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion", x = "Age (18-64)",
       title = "Proportion of US Facebook users disclosing any sexuality by age") +
  theme_dts() + 
  theme(panel.grid.major.x = element_blank())

# figure 4: disclosure by gender (and age) ----
# panel a: propensity to disclose sexuality by age and gender ----
d_sga <-
  d_y_rep %>%
  group_by(disclosed, age, gender, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(age, gender, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

p_sga <- 
  d_sga %>%
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(age, gender, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ggplot(aes(x = age, y = prop_model, 
             fill = gender, color = gender, linetype = gender)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), 
                  alpha = .4, color = "transparent") + 
  geom_line(size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  scale_fill_manual(values = red_cyan, 
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion", x = "Age (18-64)",
       fill = NULL, color = NULL, linetype = NULL,
       title = "(a) Proportion of US Facebook users disclosing any sexuality by age and gender") +
  theme_dts() +
  theme(panel.grid.major.x = element_blank())

# panel b: sexualities disclosed by gender ----
d_sg <- 
  d_y_rep %>%
  group_by(gender, sexuality, disclosed, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(gender, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

p_sg <- 
  d_sg %>%
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(prop_data, sexuality, gender) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ungroup() %>%
  arrange(sexuality) %>%
  mutate(sexuality = str_to_sentence(str_remove(sexuality, "Interested in ")), 
         sexuality = as_factor(sexuality)) %>%
  mutate(gender = fct_rev(gender)) %>%
  ggplot(aes(x = prop_model, y = fct_rev(sexuality), 
             fill = gender, color = gender)) +
  geom_col(position = position_dodge(width = .75), 
                      width = .5, 
                      alpha = .6, 
                      color = "transparent") +
  geom_linerange(aes(xmin = .lower, xmax = .upper), 
                            position = position_dodge(width = .75), 
                            size = .75) +
  scale_fill_manual(values = rev(red_cyan),
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0), 
                     labels = scales::percent) +
  guides(fill = FALSE, 
         color = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL, linetype = NULL,
       title = "(b) Sexual identities disclosed by gender",
       subtitle = "Interested in ...") +
  theme_dts() + 
  theme(panel.grid.major.y = element_blank())

# panel c: sexual identities by age and gender ----
d_sga2 <-
  d_y_rep %>%
  group_by(disclosed, sexuality, age, gender, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(age, gender, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

d_sga2_sum <- 
  d_sga2 %>%
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(age, gender, sexuality, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ungroup() %>%
  arrange(sexuality) %>%
  mutate(sexuality = str_to_sentence(str_remove(sexuality, "Interested in ")), 
         sexuality = as_factor(sexuality))
  
# p_sga2 <- 
#   ggplot(d_sga2_sum, 
#          aes(x = age, y = prop_data, 
#              fill = gender, color = gender, linetype = gender)) +
#   geom_lineribbon(aes(y = prop_model, ymin = .lower, ymax = .upper), 
#                   size = .5, alpha = .4) + 
#   geom_line(aes(y = prop_model), size = 1) +
#   geom_hline(yintercept = 0, color = "grey40") +
#   facet_wrap(vars(sexuality), scale = "free_y") +
#   scale_fill_manual(values = red_cyan,
#                     aesthetics = c("fill", "color")) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(y = "Proportion", x = "Age (18-64)",
#        fill = NULL, color = NULL, linetype = NULL,
#        title = "Sexual orientations disclosed by age and gender",
#        subtitle = "Interested in ...") +
#   theme(legend.position = "top") +
#   theme_dts() + 
#   theme(panel.grid.major.x = element_blank())

p_sga2_1 <- 
  d_sga2_sum %>%
  filter(sexuality %in% c("Same gender", "Men and women")) %>%
  ggplot(aes(x = age, y = prop_model,
             fill = gender, color = gender, linetype = gender)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = .4, color = "transparent") +
  geom_line(size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  facet_wrap(vars(sexuality)) +
  scale_fill_manual(values = red_cyan,
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  guides(fill = FALSE, color = FALSE, linetype = FALSE) +
  labs(y = "Proportion", x = "Age (18-64)",
       fill = NULL, color = NULL, linetype = NULL,
       title = "(c) Sexual identities disclosed by age and gender",
       subtitle = "Interested in ...") +
  theme(legend.position = "top") +
  theme_dts() + 
  theme(panel.grid.major.x = element_blank(), 
        axis.title.x = element_text(hjust = .85), 
        plot.margin = unit(c(7, 0, 7, 7), "pt"))

p_sga2_2 <- 
  d_sga2_sum %>%
  filter(sexuality == "Different gender") %>%
  ggplot(aes(x = age, y = prop_model,
             fill = gender, color = gender, linetype = gender)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = .4, color = "transparent") + 
  geom_line(size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  facet_wrap(vars(sexuality)) +
  scale_fill_manual(values = red_cyan,
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = FALSE, color = FALSE, linetype = FALSE) +
  labs(y = NULL, x = NULL,
       fill = NULL, color = NULL, linetype = NULL) +
  theme(legend.position = "top") +
  theme_dts() + 
  theme(panel.grid.major.x = element_blank())

p_sga2 <- p_sga2_1 + p_sga2_2 + plot_layout(guides = "collect", widths = c(2, 1)) 

p_gender <- 
  p_sga / p_sg / p_sga2 + 
  plot_layout(guides = 'collect') & 
  labs(y = NULL) &
  theme(legend.position = "top", legend.justification = "left", 
        plot.title.position = "plot")

# figure 5: disclosure by relationship status ----
d_dr <- 
  d_y_rep %>% 
  group_by(disclosed, relationship_status, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(relationship_status, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction)) %>%
  ungroup() 

d_dr_qi <- 
  d_dr %>% 
  group_by(relationship_status, disclosed, prop_data, estimate) %>%  
  point_interval(prop_model, .point = median, .interval = qi) %>%
  mutate(text_position_x = ifelse(
    disclosed, 
    prop_model / 2, 
    1 - prop_model / 2
  ))

p_rel <- 
  d_dr_qi %>%
  # filter(disclosed) %>%
  ggplot(aes(x = prop_model, y = fct_rev(relationship_status), 
             fill = relationship_status)) +
  geom_col(aes(x = 1), fill = "gray80", alpha = .4, width = .4, 
           data = ~filter(., disclosed)) +
  geom_col(alpha = .6, width = .4, 
           data = ~filter(., disclosed)) +
  geom_linerange(aes(xmin = .lower, xmax = .upper, color = relationship_status),
                 size = .75,
                 data = ~filter(., disclosed)) + 
  geom_text(aes(label = str_c(signif(estimate/1e6, 3), "M"), 
                x = text_position_x), 
            color = "white",
            hjust = .5, 
            data = ~filter(., disclosed)) + 
  geom_text(aes(label = str_c(signif(estimate/1e6, 3), "M"), 
                x = text_position_x),
            color = "gray50", 
            hjust = .5,
            data = ~filter(., !disclosed)) + 
  scale_x_continuous(labels = scales::percent, 
                     expand = expansion(mult = 0.01), # so lines at 0 and 100 don't get cut off
                     limits = c(0, 1)) +
  scale_fill_manual(values = tetrad, 
                    aesthetics = c("fill", "color"), 
                    guide = FALSE) +  
  labs(title = "Proportion of US Facebook users disclosing any sexuality\nby relationship status", 
       y = NULL, x = NULL) +
  theme_dts() +
  theme(panel.grid.major.y = element_blank())

# figure 6: disclosure by relationship status and age ----
# plot propensity to disclose sexuality by age and relationship status ----
d_sra <- 
  d_y_rep %>%
  group_by(disclosed, age, relationship_status, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(age, relationship_status, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

d_sra_end <- 
  d_sra %>%
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(age, relationship_status, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  filter(age == 64)

p_sra <- 
  d_sra %>%
  filter(disclosed) %>%
  select(-estimate, -.prediction) %>%
  group_by(age, relationship_status, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ggplot(aes(x = age, y = prop_mdoel, 
             fill = relationship_status, color = relationship_status, 
             linetype = relationship_status)) +
  geom_ribbon(aes(y = prop_model, ymin = .lower, ymax = .upper), 
                  alpha = .4, color = "transparent") + 
  geom_line(aes(y = prop_model), size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  # geom_text(aes(label = relationship_status), 
  #           data = ~filter(., age == 64)) +
  # annotate("text", x = 64, y = .484, label = "Single") +
  scale_fill_manual(values = tetrad,
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(trans = ~ ., 
                                         breaks = d_sra_end$prop_model,
                                         labels = d_sra_end$relationship_status)) +
  guides(fill = FALSE, color = FALSE, linetype = FALSE) +
  labs(y = "Proportion", x = "Age (18-64)",
       fill = NULL, color = NULL, linetype = NULL,
       title = "Proportion of US Facebook users disclosing any sexuality\nby age and relationship status") +
  theme_dts() +
  theme(panel.grid.major.x = element_blank())

d_sra2 <- 
  d_y_rep %>%
  group_by(sexuality, age, relationship_status, .draw) %>%
  summarize(estimate = sum(estimate), 
            .prediction = sum(.prediction)) %>%
  ungroup() %>%
  group_by(age, relationship_status, .draw) %>%
  mutate(prop_data = estimate/sum(estimate), 
         prop_model = .prediction/sum(.prediction))

d_sra2 %>%
  # filter(sexuality != "Not specified") %>%
  select(-estimate, -.prediction) %>%
  group_by(age, relationship_status, sexuality, prop_data) %>%
  point_interval(prop_model, .point = median, .interval = qi) %>%
  ggplot(aes(x = age, y = prop_model, 
             fill = relationship_status, color = relationship_status, 
             linetype = relationship_status)) +
  geom_ribbon(aes(y = prop_model, ymin = .lower, ymax = .upper), 
              alpha = .4, color = "transparent") + 
  geom_line(aes(y = prop_model), size = .75) +
  geom_line(aes(y = prop_data, linetype = relationship_status), 
            color = "black") + 
  geom_hline(yintercept = 0, color = "grey40") +
  facet_wrap(vars(sexuality), scales = "free_y") +
  # geom_text(aes(label = relationship_status), 
  #           data = ~filter(., age == 64)) +
  # annotate("text", x = 64, y = .484, label = "Single") +
  scale_fill_manual(values = tetrad,
                    aesthetics = c("fill", "color")) + 
  scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(labels = scales::percent, 
  #                    sec.axis = sec_axis(trans = ~ ., 
  #                                        breaks = d_sra_end$prop_model,
  #                                        labels = d_sra_end$relationship_status)) +
  # guides(fill = FALSE, color = FALSE, linetype = FALSE) +
  labs(y = "Proportion", x = "Age (18-64)",
       fill = NULL, color = NULL, linetype = NULL,
       title = "Proportion of US Facebook users disclosing any sexuality\nby age and relationship status") +
  theme_dts() +
  theme(panel.grid.major.x = element_blank())


# figure 7: all characteristics (model) ----
d_everything <- 
  d_y_rep %>%
  ungroup() %>%
  arrange(sexuality) %>%
  mutate(sexuality = str_to_sentence(str_remove(sexuality, "Interested in ")), 
         sexuality = as_factor(sexuality)) %>%
  group_by(gender, age, relationship_status, sexuality, estimate) %>%
  point_interval(.prediction, .point = median, .interval = qi)

p_everything <- 
  d_everything %>%
  ggplot(aes(x = age, y = .prediction, ymin = .lower, ymax = .upper,
             fill = gender, color = gender, linetype = gender)) + 
  geom_ribbon(color = "transparent", alpha = .4) +
  geom_line(size = .75) +
  geom_hline(yintercept = 0, color = "grey40") +
  facet_grid(rows = vars(sexuality), 
             cols = vars(relationship_status), 
             scales = "free_y") + 
  scale_x_continuous(expand = c(0, 0), 
                     sec.axis = dup_axis(name = "Relationship status")) +
  scale_y_continuous(labels = scales::comma, 
                     sec.axis = dup_axis(name = "Sexuality")) +
  scale_fill_manual(values = red_cyan,
                    aesthetics = c("fill", "color")) + 
  labs(y = "Estimate", x = "Age (18-64)", 
       fill = NULL, color = NULL, linetype = NULL,
       title = "Sexual identities of US Facebook users by age, gender, and relationship status") +
  theme_dts() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(), 
        legend.box.spacing = unit(0, "points"))

# figure 8: all characteristics (model + data) ----
p_everything_with_data <- 
  p_everything + 
  geom_line(aes(y = estimate), color = "black", size = .75)

# show figures ----
p_disclosure
p_age
p_gender
p_rel
p_sra
p_everything
p_everything_with_data

# save figures ----
# png files
ggsave(filename = "output/figures/models/figure2_disclosure.png", 
       plot = p_disclosure,
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure3_age.png", 
       plot = p_age, 
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure4_gender.png", 
       plot = p_gender, 
       width = 10, height = 10)

ggsave(filename = "output/figures/models/figure5_relationship_status.png", 
       plot = p_rel, 
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure6_age_x_relationship_status.png", 
       plot = p_sra, 
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure7_model_estimates.png", 
       plot = p_everything, 
       width = 10, height = 8)

ggsave(filename = "output/figures/models/figure8_data_estimates.png", 
       plot = p_everything_with_data, 
       width = 10, height = 8)

# eps files
ggsave(filename = "output/figures/models/figure2_disclosure.eps", 
       plot = p_disclosure,
       device = cairo_ps, fallback_resolution = 600,
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure3_age.eps", 
       plot = p_age, 
       device = cairo_ps, fallback_resolution = 600,
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure4_gender.eps", 
       plot = p_gender, 
       device = cairo_ps, fallback_resolution = 600,
       width = 10, height = 10)

ggsave(filename = "output/figures/models/figure5_relationship_status.eps", 
       plot = p_rel, 
       device = cairo_ps, fallback_resolution = 600,
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure6_age_x_relationship_status.eps", 
       plot = p_sra, 
       device = cairo_ps, fallback_resolution = 600,
       width = 8, height = 5)

ggsave(filename = "output/figures/models/figure7_model_estimates.eps", 
       plot = p_everything, 
       device = cairo_ps, fallback_resolution = 600,
       width = 10, height = 8)

ggsave(filename = "output/figures/models/figure8_data_estimates.eps", 
       plot = p_everything_with_data, 
       device = cairo_ps, fallback_resolution = 600,
       width = 10, height = 8)
