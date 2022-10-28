# Run apportionment for duskies and northerns using rema

# set up ----

# assessment year
YEAR <- 2022

libs <- c('readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
library(rema)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# data ----

dusky <- read_csv(paste0(dat_path, '/dusky_bts_biomass.csv'))
northerns <- read_csv(paste0(dat_path, '/northern_bts_biomass.csv'))

# dusky ----

input <- prepare_rema_input(model_name = 'duskies',
                            biomass_dat = dusky,
                            end_year = YEAR,
                            # how do you deal with zero biomass observations?
                            # see ?prepare_rema_input() for more options
                            zeros = list(assumption = 'NA'))
m <- fit_rema(input)
out <- tidy_rema(m)
plots <- plot_rema(out)

out$parameter_estimates # proc error in alphabetical order by strata (e.g. cgoa, egoa, wgoa)

out$proportion_biomass_by_strata %>% 
  write_csv(paste0(out_path, '/dusky_apportionment_ratios.csv'))

out$biomass_by_strata %>% 
  pivot_wider(id_cols = year, names_from = strata, values_from = pred) %>% 
  write_csv(paste0(out_path, '/dusky_pred_biomass.csv'))

plots$proportion_biomass_by_strata 
ggsave(paste0(out_path, '/dusky_proportion_by_strata.png'), units = 'in', bg = 'white',
       height = 4, width = 7, dpi = 300)

plots$biomass_by_strata
ggsave(paste0(out_path, '/dusky_biomass_by_strata.png'), units = 'in', bg = 'white',
       height = 3, width = 7, dpi = 300)

# northerns ----
northerns %>% print(n=Inf)

input <- prepare_rema_input(model_name = 'northerns',
                            biomass_dat = northerns,
                            end_year = YEAR,
                            # how do you deal with zero biomass observations?
                            # see ?prepare_rema_input() for more options
                            zeros = list(assumption = 'NA'))
m <- fit_rema(input)
out <- tidy_rema(m)

out$parameter_estimates # proc error in alphabetical order by strata (e.g. cgoa, egoa, wgoa)

out$proportion_biomass_by_strata %>% 
  write_csv(paste0(out_path, '/northerns_apportionment_ratios.csv'))

out$biomass_by_strata %>% 
  pivot_wider(id_cols = year, names_from = strata, values_from = pred) %>% 
  write_csv(paste0(out_path, '/northerns_pred_biomass.csv'))

plots <- plot_rema(out)

plots$proportion_biomass_by_strata 
ggsave(paste0(out_path, '/northerns_proportion_by_strata.png'), units = 'in', bg = 'white',
       height = 4, width = 7, dpi = 300)

plots$biomass_by_strata +
  facet_wrap(~strata, scales = 'free_y')
ggsave(paste0(out_path, '/northerns_biomass_by_strata_freey.png'), units = 'in', bg = 'white',
       height = 3, width = 7, dpi = 300)

plots$biomass_by_strata 
ggsave(paste0(out_path, '/northerns_biomass_by_strata.png'), units = 'in', bg = 'white',
       height = 3, width = 7, dpi = 300)
