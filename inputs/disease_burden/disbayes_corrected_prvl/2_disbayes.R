library(disbayes); library(tidyverse); library(fst)
library(doParallel)

#### Setting ####
# for() loop
# n_cores <- detectCores(logical = TRUE)
# options(mc.cores = n_cores)

# foreach() loop
# n_cores <- detectCores(logical = TRUE)
# registerDoParallel(n_cores)
# options(mc.cores = n_cores)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores)
# registerDoParallel(cl)


#### for() loop ####
for(i in 1:nrow(set)){
  tryCatch({

  #### Preparation of datasheets ####
  d_incd <- read_fst(set$dir_incd[i])
  d_incd <- d_incd %>%
    dplyr::filter(sex %in% set$sex_cat[i],
                  year %in% set$years[i],
                  cause_name %in% set$dis_cat[i],
                  measure_name == "Incidence",
                  age <= age_end) %>%
    mutate(inc_rate = mu,
           inc_num_ = N) %>%
    #filter(age >= 30, age <= age_end) %>% #edited here because we start from age 30 20260717 #need to have dimension of 90 rows of age.
    dplyr::select(age, inc_rate, inc_num_)

  d_pop <- read_fst(set$dir_pop[i])
  d_pop <- d_pop %>%
    dplyr::filter(year %in% set$years[i],
                  sex %in% set$sex_cat[i]) %>%
    mutate(inc_denom = pops,
           mort_denom = pops) %>%
    #filter(age >= 30, age <= age_end) %>% #edited here because we start from age 30 20260717
    dplyr::select(age, inc_denom, mort_denom)

  d_ftlt <- read_fst(set$dir_ftlt[i]) %>%
    ##mutate(sex = str_sub(sex, 2, -1),
    ##       cause_name = str_sub(cause_name, 2, -1)) %>%
    dplyr::filter(year %in% set$years[i],
                  sex %in% set$sex_cat[i],
                  cause_name %in% set$dis_cat[i],
                  age <= age_end) %>%
    mutate(mort_rate = mu2) %>% ##edited 20260617
    #filter(age >= 30, age <= age_end) %>% #edited here because we start from age 30 20260717
    dplyr::select(age, mort_rate)

  d <- left_join(d_pop, d_incd, by="age") %>% left_join(., d_ftlt, by="age")

  # d <- d %>%
  #   mutate(inc_num = round(inc_rate*inc_denom) %>% as.integer(),
  #          mort_num = round(mort_rate*mort_denom) %>% as.integer()) %>%
  #   mutate(inc_denom = as.integer(inc_denom),
  #          mort_denom = as.integer(mort_denom)) %>%
  #   select(age,inc_num,inc_denom,mort_num,mort_denom)

  d <- d %>%
    mutate(
      inc_num  = ifelse(age < 20, 0L, round(inc_rate * inc_denom) %>% as.integer()), #due to low incidence
      inc_denom = as.integer(inc_denom),
      mort_num = ifelse(age < 20, 0L, round(mort_rate * mort_denom) %>% as.integer()), #due to low mortality
      mort_denom = as.integer(mort_denom)
    ) %>%
    select(age, inc_num, inc_denom, mort_num, mort_denom) %>%
    arrange(age)

  test <- d


  #d <- d %>%
    #mutate(age = age - 30)



  # set.seed(40224)
  # res0 <- disbayes(data = d,
  #                  age = "age",
  #                  inc_num = "inc_num", inc_denom = "inc_denom",
  #                  mort_num = "mort_num", mort_denom = "mort_denom",
  #                  method = "mcmc",
  #                  eqage = 20, #eqage=40 defines an assumption that case fatality is constant for all ages below 40
  #                  chains = 4, iter = 1000
  #                  )

  eqage_i <- if (set$dis_cat[i] == "chd") 40 else 20 #to reduce R-hat

  res0 <- disbayes(data = d, age = "age",
                   inc_num = "inc_num", inc_denom = "inc_denom",
                   mort_num = "mort_num", mort_denom = "mort_denom",
                   method = "mcmc", eqage = eqage_i,
                   chains = 4, iter = 4000)

  cat("\n===", set$dis_cat[i], set$sex_cat[i], set$years[i], "===\n")
  diag <- rstan::check_hmc_diagnostics(res0$fit)

  dir.create("./work/output/disbayes_rds",
             recursive = TRUE,
             showWarnings = FALSE)

  saveRDS(res0, paste0("./work/output/disbayes_rds/", set$dis_cat[i], "_", set$sex_cat[i], "_", set$years[i], ".rds"))
}, error = function(e) {
  cat("\n!!! FAILED:", set$dis_cat[i], set$sex_cat[i], set$years[i], "-", conditionMessage(e), "\n")
})
}


#based on > disbayes:::init_smooth and disbayes:::init_rates, my initial plan to start from age 30 to 90, making 61 rows cannot be handled by disbayes, and I need to make 90 rows of age.

