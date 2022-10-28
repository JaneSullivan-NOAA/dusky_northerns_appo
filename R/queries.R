# Queries for dusky and northern rockfish apportionment using GOA bottom trawl
# design-based biomass indices 

# set up ----

# assessment year
YEAR <- 2022

libs <- c('readr', 'dplyr', 'tidyr', 'RODBC', 'ggplot2')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

# database connection ----

# Enter your username and password for the AKFIN database. Note that these
# credentials are different than what you may use to access AKFIN Answer.
# Contact AKFIN for more information.
username_akfin = 'my_username'
password_akfin = 'my_password'
username_akfin = 'jsullivan'
password_akfin = 'sculja22'
channel_akfin <- odbcConnect("akfin", uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

# Species lookup ----

query <- paste0()

spp <- sqlQuery(channel_akfin, "
                  select   species_code, species_name, common_name
                  from     afsc.race_racespeciescodes
                ") %>% 
  rename_all(tolower) 

spp <- spp %>% filter(grepl("dusky rockfish|northern rockfish", common_name))
spp
spp_codes <- toString(sprintf("'%s'", spp$species_code)) # allows you to pass vector into sql query
spp_codes

# survey estimates ----

biom <- sqlQuery(channel_akfin, sprintf("
                  select    year, species_code, regulatory_area_name, 
                            area_biomass, biomass_var
                  from      afsc.race_biomassareaaigoa
                  where     species_code in (%s) and
                            survey = 'GOA'and
                            year >= 1984
                ", spp_codes)) 
write_csv(biom, paste0(dat_path, '/goa_bts_biomass_raw.csv')) 

# prep
biom <- read_csv(paste0(dat_path, '/goa_bts_biomass_raw.csv')) %>% 
  rename_all(tolower) %>% 
  left_join(spp) %>% 
  mutate(cv = ifelse(biomass_var == 0, NA, sqrt(biomass_var) / area_biomass)) %>% 
  select(common_name, species_code, strata = regulatory_area_name, year, biomass = area_biomass, cv) %>% 
  arrange(common_name, strata, year) 

biom %>% 
  filter(common_name == 'dusky rockfish') %>% 
  select(-common_name, -species_code) %>% 
  write_csv(paste0(dat_path, '/dusky_bts_biomass.csv'))

biom %>% 
  filter(common_name == 'northern rockfish') %>% 
  select(-common_name, -species_code) %>% 
  write_csv(paste0(dat_path, '/northern_bts_biomass.csv'))
  
biom %>% print(n=Inf)
