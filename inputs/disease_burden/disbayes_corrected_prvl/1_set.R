library(tidyverse)

#### Analysis set ####
# diseases
dis_cat <- c("stroke", "chd")

# Sex categories
sex_cat <- c("men","women")

# years needed
years <- c(2013, 2022)

# age end
age_end <- 90 # the oldest age available in the data (incd, ftlt)


#aggregate #pop data
pop_Indo <- read_fst("./inputs/pop_projections/combined_population_indonesia_final.fst")
setDT(pop_Indo)
pop_Indo <- pop_Indo[, sum(pops), keyby = .(age, year, sex, type)]
setnames(pop_Indo, "V1", "pops")
write_fst(pop_Indo, "./inputs/pop_projections/combined_population_indonesia_final_no_reg.fst")

# set
set <- expand_grid(dis_cat, sex_cat, years)
set <- set %>%
  mutate(dir_incd = paste0("./work/input/", dis_cat, "_incd.fst"), #here data from age 20
         dir_pop = "./work/input/combined_population_indonesia_final_no_reg.fst",
         dir_ftlt = paste0("./work/input/", dis_cat, "_ftlt.fst")) #here data from age 20

