# apportionment for dusky and northern rockfish in the GOA
# ben.williams@noaa.gov
# nov 2022

# note there are 2 species codes associated with dusky rockfish
# goa dusky 
# afsc_species1 = 30150
# afsc_species2 = 30152

# goa northern rockfish
# afsc_species = 30420

# load ----
library(rema)
library(gfdata)
library(dplyr)
library(tidyr)
library(vroom)
library(ggplot2)
theme_set(afscassess::theme_report())


dir.create(here::here(yr, "data"), recursive = T)
dir.create(here::here(yr, "results"), recursive = T)

# globals ----

akfin_user = 'jsullivan'
akfin_pwd = 'sculja22'

yr = 2022

# query data ----
# connect to database
akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                       UID = akfin_user, PWD = akfin_pwd)

# run query (note filter survey data pre-1990)
sql_run(akfin, 
        "select    year, species_code, regulatory_area_name, 
        area_biomass, biomass_var
        from      afsc.race_biomassareaaigoa
        where     species_code in (30150, 30152, 30420) and
        survey = 'GOA'") %>% 
  rename_all(tolower) %>% 
  dplyr::select(year, code = species_code, strata = regulatory_area_name, biomass = area_biomass, var = biomass_var) -> dat

DBI::dbDisconnect(akfin)

# dusky rockfish 
# remove the limited dusky unid in 1990
dat %>% 
  filter(code != 30420) %>% 
  group_by(strata, year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(var)) %>% 
  ungroup() %>% 
  mutate(cv = ifelse(var==0, NA, sqrt(var) / biomass)) %>% 
  dplyr::select(strata, year, biomass, cv) -> dusky

vroom_write(dusky, here::here(yr, "data", "dusky.csv"), delim = ",")

# northern rockfish 
dat %>% 
  filter(code == 30420) %>% 
  mutate(cv = ifelse(var==0, NA, sqrt(var) / biomass)) %>% 
  dplyr::select(strata, year, biomass, cv) -> northern

vroom_write(northern, here::here(yr, "data", "northern.csv"), delim = ",")

# data ----

vroom(here::here(yr, "data", "dusky.csv"))
vroom(here::here(yr, "data", "northern.csv"))

# model ----
# dusky
input <- prepare_rema_input(model_name = 'dusky',
                            biomass_dat = dusky,
                            end_year = yr,
                            # how do you deal with zero biomass observations?
                            # see ?prepare_rema_input() for more options
                            zeros = list(assumption = 'NA'))
m <- fit_rema(input)
out <- tidy_rema(m)

out$proportion_biomass_by_strata %>% 
  vroom_write(here::here(yr, "results", "dusky_ratios.csv"), delim=",")
    
    
png(filename=here::here("figs", "dusky.png"), width = 6.5, height = 6.5, 
    units = "in", type ="cairo", res = 200)

out$biomass_by_strata %>% 
  filter(year>=1990) %>% 
  mutate(strata = case_when(strata=="EASTERN GOA" ~ "Eastern",
                            strata=="CENTRAL GOA" ~ "Central",
                            strata=="WESTERN GOA" ~ "Western"),
         strata = factor(strata, levels = c("Western", "Central", "Eastern"))) %>% 
  ggplot(aes(year, pred)) + 
  geom_point(aes(y = obs), color = "darkgray") + 
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), color = "darkgray", width = 0.8) +
  geom_line() + 
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci), alpha = 0.3) +
  facet_wrap(~strata, ncol = 1, scales = "free_y") +
  ylab("Biomass (t)") +
  xlab("Year") + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1990,2020,5))

dev.off()

# northern
# doesn't converge if exclude 1980s data from analysis...
input <- prepare_rema_input(model_name = 'northern',
                            biomass_dat = northern,
                            end_year = yr,
                            # how do you deal with zero biomass observations?
                            # see ?prepare_rema_input() for more options
                            zeros = list(assumption = 'NA'))
m <- fit_rema(input)
out <- tidy_rema(m)


out$biomass_by_strata %>% 
  filter(year>=1990) %>% 
  mutate(strata = case_when(strata=="EASTERN GOA" ~ "Eastern",
                            strata=="CENTRAL GOA" ~ "Central",
                            strata=="WESTERN GOA" ~ "Western"),
         strata = factor(strata, levels = c("Western", "Central", "Eastern"))) %>% 
  ggplot(aes(year, pred)) + 
  geom_point(aes(y = obs), color = "darkgray") + 
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), color = "darkgray", width = 0.8) +
  geom_line() + 
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci), alpha = 0.3) +
  facet_wrap(~strata, ncol = 1, scales = "free_y") +
  ylab("Biomass (t)") +
  xlab("Year") + 
  scale_y_continuous(labels = scales::comma)  +
  scale_x_continuous(breaks = seq(1990,2020,5))


