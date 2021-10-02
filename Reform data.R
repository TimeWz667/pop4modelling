library(tidyverse)
library(ggplot2)



path_out <- glue::glue("ByCountry/")


##### Data loading -----

raw_death <- read_csv("../raw/WPP2019_Life_Table_Medium.csv") %>%
  filter(Variant == "Medium") %>%
  select(Location, YearRange = Time, Sex, AgeGrp, AgeGrpStart, AgeGrpSpan, mx)
  
raw_fert <-  read_csv("../raw/WPP2019_Fertility_by_Age.csv") %>%
  filter(Variant == "Medium") %>%
  select(Location, YearRange = Time, AgeGrp, AgeGrpStart, AgeGrpSpan, ASFR, PASFR, Births)


raw_pop_be <- read_csv("../raw/WPP2019_PopulationBySingleAgeSex_1950-2019.csv") %>%
  filter(Variant == "Medium") %>%
  select(Location, Year = Time, Age = AgeGrp, PopMale, PopFemale, PopTotal)

raw_pop_fo <- read_csv("../raw/WPP2019_PopulationBySingleAgeSex_2020-2100.csv") %>%
  filter(Variant == "Medium") %>%
  select(Location, Year = Time, Age = AgeGrp, PopMale, PopFemale, PopTotal)


age_link <- tibble(
  Age = 0:100,
  AgeGrp = c(0, rep("1-4", 4), rep(paste(seq(5, 95, 5), seq(9, 99, 5), sep = "-"), each = 5), "100+")
)

year_link <- tibble(
  Year = 1950:2099,
  YearRange = rep(unique(raw_death$YearRange), each = 5)
)

locations <- read_csv("Locations.csv")




for (country in unique(raw_death$Location)) {
  sel <- locations %>% filter(Data == country)
  
  country_out <- sel$Out
  
  if (is.na(country_out)) {
    next
  } else {
    dir.create(file.path(path_out, country_out), showWarnings = FALSE)
  }
  
  
  ##### Population size -----
  sel_pop = bind_rows(raw_pop_be %>% filter(Location == country),
                      raw_pop_fo %>% filter(Location == country)) %>%
    arrange(Age, Year)
  
  years <- 1950:2100
  pop_year_age <- matrix(sel_pop$PopFemale, length(years), 101)
  colnames(pop_year_age) <- 0:100
  pop_year_age <- cbind(Time = years, pop_year_age * 1000)
  write_csv(as_tibble(pop_year_age), path_out + country_out + "/PopF.csv")
                        
  pop_year_age <- matrix(sel_pop$PopMale, length(years), 101)
  colnames(pop_year_age) <- 0:100
  pop_year_age <- cbind(Time = years, pop_year_age * 1000)
  write_csv(as_tibble(pop_year_age), path_out + country_out + "/PopM.csv")
  
  pop_year_age <- matrix(sel_pop$PopTotal, length(years), 101)
  colnames(pop_year_age) <- 0:100
  pop_year_age <- cbind(Time = years, pop_year_age * 1000)
  write_csv(as_tibble(pop_year_age), path_out + country_out + "/PopT.csv")
  
  
  ##### Birth -----
  
  sel_fert <- raw_fert %>%
    filter(Location == country) %>%
    separate(YearRange, c("T0", "T1"), "-") %>%
    mutate(Ti = as.numeric(T0) + 2.5, A = AgeGrpStart + AgeGrpSpan / 2, lf = log(ASFR / 1000)) %>%
    select(Ti, A, lf)
  
  
  years <- seq(1952.5, 2097.5, 5)
  
  ext_age <- matrix(0, length(years), 101)
  
  
  for (i in 1:length(years)) {
    ti <- years[i]
    fert <- sel_fert %>% filter(Ti == ti)
    ext_age[i, 16:50] <- spline(fert$A, fert$lf, xout = 15:49)$y
  }
  
  y_mid <- years
  years <- 1950:2099 + 0.5
  ext_year_age <- matrix(0, length(years), 101)
  
  for (i in 16:50) {
    ext_year_age[, i] <- exp(spline(y_mid, ext_age[, i], xout = years)$y)
  }
  
  colnames(ext_year_age) <- 0:100
  ext_year_age <- cbind(Time = 1950:2099, ext_year_age)
  
  write_csv(as_tibble(ext_year_age), path_out + country_out + "/FertSpline.csv")
  
  # raw_fert %>%
  #   filter(Location == country) %>%
  #   left_join(year_link) %>%
  #   left_join(age_link) %>%
  #   filter(Year == 2004) %>%
  #   left_join(sel_pop) %>%
  #   select(AgeGrp, Age, ASFR, PASFR, Births, PopFemale) %>%
  #   mutate(
  #     n_b = PopFemale * ASFR 
  #   ) %>%
  #   group_by(AgeGrp) %>%
  #   summarise(bir = sum(n_b), Births = mean(Births) * 1000 / 5)
  
  
  ##### Death ----
  
  sel_death <- raw_death %>%
    filter(Sex == "Male") %>%
    filter(Location == country) %>%
    separate(YearRange, c("T0", "T1"), "-") %>%
    mutate(Ti = as.numeric(T0) + 2.5, A = AgeGrpStart + AgeGrpSpan / 2, lmx = log(mx)) %>%
    select(Ti, A, lmx)
  
  
  years <- seq(1952.5, 2097.5, 5)
  
  ext_age <- matrix(0, length(years), 101)
  
  
  for (i in 1:length(years)) {
    ti <- years[i]
    dea <- sel_death %>% filter(Ti == ti)
    ext_age[i, ] <- spline(dea$A, dea$lmx, xout = 0:100)$y
  }
  
  y_mid <- years
  years <- 1950:2099 + 0.5
  ext_year_age <- matrix(0, length(years), 101)
  
  for (i in 1:101) {
    ext_year_age[, i] <- spline(y_mid, ext_age[, i], xout = years)$y
  }
  
  colnames(ext_year_age) <- 0:100
  ext_year_age <- cbind(Time = 1950:2099, exp(ext_year_age))
  
  
  write_csv(as_tibble(ext_year_age), path_out + country_out + "/DeaSplineM.csv")
  
  
  sel_death <- raw_death %>%
    filter(Sex == "Female") %>%
    filter(Location == country) %>%
    separate(YearRange, c("T0", "T1"), "-") %>%
    mutate(Ti = as.numeric(T0) + 2.5, A = AgeGrpStart + AgeGrpSpan / 2, lmx = log(mx)) %>%
    select(Ti, A, lmx)
  
  
  years <- seq(1952.5, 2097.5, 5)
  
  ext_age <- matrix(0, length(years), 101)
  
  
  for (i in 1:length(years)) {
    ti <- years[i]
    dea <- sel_death %>% filter(Ti == ti)
    ext_age[i, ] <- spline(dea$A, dea$lmx, xout = 0:100)$y
  }
  
  y_mid <- years
  years <- 1950:2099 + 0.5
  ext_year_age <- matrix(0, length(years), 101)
  
  for (i in 1:101) {
    ext_year_age[, i] <- spline(y_mid, ext_age[, i], xout = years)$y
  }
  
  colnames(ext_year_age) <- 0:100
  ext_year_age <- cbind(Time = 1950:2099, exp(ext_year_age))
  
  
  write_csv(as_tibble(ext_year_age), path_out + country_out + "/DeaSplineF.csv")
}


