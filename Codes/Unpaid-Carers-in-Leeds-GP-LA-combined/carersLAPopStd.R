
# yearly ons estimates ----------------------------------------------------
library(openxlsx)

# test
# dir(path = "data/")
# openxlsx::getSheetNames(file = "data/population2021.xlsx")
# leeds_lsoa <- 
#   openxlsx::read.xlsx(xlsxFile = "data/population2021.xlsx", 
#                       sheet = 5, 
#                       startRow = 4,
#                       colNames = T
#   ) %>% 
#   # names()
#   filter(str_detect(string = LSOA.Name, pattern = "^Leeds")) %>% 
#   pull(LSOA.Code)

files <- 
  dir(path = "data/", pattern = ".xlsx") %>% 
  str_subset(string = ., pattern = "population")

pop_dat <- 
  tibble(
    files = files,
    year = str_remove_all(string = files, "[:alpha:]|[:punct:]"),
    path = paste("data/", files, sep = "")
  ) %>% 
  mutate(
    sheet = purrr::map(.x = path, 
                .f = ~getSheetNames(file = .x) %>% 
                  str_subset(., "Males|Females")                
    )
  ) %>% 
  unnest(sheet) %>% 
  mutate(
    gender = 
      str_replace(string = sheet, pattern = "Mid-20[:digit:][:digit:] ", 
                  replacement = "") %>% 
      str_to_lower(string = .)
  ) %>% 
  mutate(
    dat = map2(.x = path, 
               .y = sheet,
               .f = ~read_excel(path = .x, 
                                sheet = .y, 
                                col_names = T, 
                                skip = 4, 
                                # n_max = 100
               ) %>% 
                 filter(if_all(.cols = 1, .fns = ~. %in% leeds_lsoa))
    )
  )

# pop_dat %>% map(.x = .$dat, .f = ~names(.x)[1:5])
# pop_dat %>% map(.x = .$dat, .f = ~dim(.x))

# pop_dfs <- 
#   pop_dat %>%
#   select(year, gender, dat) %>% 
#   unnest(data = ., cols = dat)

# set_names(x = ., 
#             nm = pop_dat$sheet %>% 
#               str_replace(., "-", "_") %>% 
#               str_replace(., " ", "_")
#             )

# pop_dfs1 <- 
#   pop_dat[1:4,]
# pop_dfs2 <- 
#   pop_dat[5:6,]
# pop_dfs3 <- 
#   pop_dat[7:12,]

pop_dfs3 <- 
  pop_dat[7:12,] %>% 
  select(-files, -path, -sheet) %>% 
  mutate(
    dat = 
      map(.x = dat, 
          .f = ~janitor::clean_names(.) %>%
            select(-contains("la"), -all_ages) %>% 
            pivot_longer(data = ., 
                         cols = !contains("lsoa"), 
                         names_to = "age", 
                         values_to = "n") %>% 
            transmute(
              lsoa_code,
              lsoa_name,
              age = str_remove_all(age, "[:alpha:]|[:punct:]") %>% as.integer(),
              n
            )
      )
  ) %>% 
  unnest(data = ., dat) 

pop_dfs1 <-
  pop_dat[1:4,] %>% 
  select(-files, -path, -sheet) %>%
  mutate(
    dat = 
      map(.x = dat, 
          .f = ~select(.data = .x,
                       lsoa_code = `Area Codes`,
                       lsoa_name = ...3,
                       `0`:`90+`) %>% 
            janitor::clean_names(.) %>% 
            drop_na(data = ., lsoa_name) %>% 
            pivot_longer(data = ., 
                         cols = !contains("lsoa"), 
                         names_to = "age", 
                         values_to = "n") %>% 
            transmute(
              lsoa_code,
              lsoa_name,
              age = str_remove_all(age, "[:alpha:]|[:punct:]") %>% as.integer(),
              n
            )
      )
  ) %>% 
  unnest(data = ., dat) 

pop_dfs2 <-
  pop_dat[5:6,] %>% 
  select(-files, -path, -sheet) %>%
  mutate(
    dat = 
      map(.x = dat, 
          .f = ~select(.data = .x,
                       lsoa_code = `Area Codes`,
                       lsoa_name = LSOA,
                       `0`:`90+`) %>% 
            janitor::clean_names(.) %>% 
            drop_na(data = ., lsoa_name) %>% 
            pivot_longer(data = ., 
                         cols = !contains("lsoa"), 
                         names_to = "age", 
                         values_to = "n") %>% 
            transmute(
              lsoa_code,
              lsoa_name,
              age = str_remove_all(age, "[:alpha:]|[:punct:]") %>% as.integer(),
              n
            )
      )
  ) %>% 
  unnest(data = ., dat) 

# pop_dfs1 %>% group_by(year) %>% summarise(tot = sum(n))
# pop_dfs2 %>% group_by(year) %>% summarise(tot = sum(n))
# pop_dfs3 %>% group_by(year) %>% summarise(tot = sum(n))

pop_ons_leeds <- 
  bind_rows(
    pop_dfs1,
    pop_dfs2,
    pop_dfs3
  ) %>% 
  mutate(
    gender = if_else(gender == "males", "m", "f")
  )

pop_ons_leeds %>%
  group_by(year) %>%
  summarise(tot = sum(n))

# def pops
pop <-
  pop_ons_leeds %>%
  # NHSRpopulation::lsoa %>%
  left_join(.,
            NHSRpopulation::imd %>% 
              select(lsoa_code, lsoa_name, la_code, imd_decile),
            by = c("lsoa_code", "lsoa_name")
  ) %>%
  mutate(
    # imd_decile = as.numeric(imd_decile),
    # imd_quintile = as.numeric(imd_decile),
    age_band = 
      case_when(
        age < 18 ~ "0-18",
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 39 ~ "30-39",
        age >= 40 & age <= 49 ~ "40-49",
        age >= 50 & age <= 59 ~ "50-59",
        age >= 60 & age <= 69 ~ "60-69",
        age >= 70 & age <= 79 ~ "70-79",
        age >= 80 ~ "80+",
        TRUE ~ NA_character_
      ),
    imd_quintile = 
      case_when(
        between(imd_decile, 1, 2) ~ 1,
        between(imd_decile, 3, 4) ~ 2,
        between(imd_decile, 5, 6) ~ 3,
        between(imd_decile, 7, 8) ~ 4,
        between(imd_decile, 9, 10) ~ 5,
        TRUE ~ NA_real_
      ),
    gender = if_else(gender == "f", "Female", "Male"),
    overall = "overall") %>% 
  mutate(.data = ., 
         across(.cols = where(is.character), 
                .fns = ~fct_explicit_na(f = factor(.), na_level = "Unknown")),
         imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F),
                                      na_level = "Unknown"),
         imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F),
                                        na_level = "Unknown"),
         year = fct_explicit_na(f = factor(x = year, ordered = F),
                                na_level = "Unknown"),
  ) %>% 
  filter(age >= 18)

# chk
pop %>% 
  # filter(age >= 18) %>% 
  group_by(year) %>%
  # group_by(year, gender) %>% 
  # group_by(overall) %>%
  summarise(tot = sum(n))

# func
StandardiseDemographics <- function(pop, grp, summ, yearly){
  if (isTRUE(yearly)){
    pop %>%
      group_by(year, across({{grp}}), .drop = F) %>%
      summarise(across(.cols = {{summ}}, .fns = ~sum(.), .names = "pop.{.col}")) %>%
      ungroup() 
  } else {
    pop %>%
      group_by(across({{grp}}), .drop = F) %>%
      summarise(across(.cols = {{summ}}, .fns = ~floor(sum(.)/6), .names = "pop.{.col}")) %>%
      ungroup()
  }
}

# test
SummariseDemographics(df = a2, grp = imd_decile, summ = person_ref, yearly = T)
StandardiseDemographics(pop = pop, grp = overall, summ = n, yearly = F)

save(list = ls(), file = ".RData")

left_join(
  SummariseDemographics(df = a2, grp = imd_decile, summ = person_ref, yearly = T),
  StandardiseDemographics(pop = pop, grp = imd_decile, summ = n, yearly = T)
  )





# 2019 ons est --------------------------------------------------------

# def pops
pop <-
  # pop_ons_leeds %>%
  NHSRpopulation::lsoa %>%
  left_join(.,
            NHSRpopulation::imd %>% 
              select(lsoa_code, lsoa_name, la_code, imd_decile),
            by = c("lsoa_code", "lsoa_name", "la_code")
  ) %>%
  mutate(
    age_band = 
      case_when(
        age < 18 ~ "0-18",
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 39 ~ "30-39",
        age >= 40 & age <= 49 ~ "40-49",
        age >= 50 & age <= 59 ~ "50-59",
        age >= 60 & age <= 69 ~ "60-69",
        age >= 70 & age <= 79 ~ "70-79",
        age >= 80 ~ "80+",
        TRUE ~ NA_character_
      ),
    imd_quintile = 
      case_when(
        between(imd_decile, 1, 2) ~ 1,
        between(imd_decile, 3, 4) ~ 2,
        between(imd_decile, 5, 6) ~ 3,
        between(imd_decile, 7, 8) ~ 4,
        between(imd_decile, 9, 10) ~ 5,
        TRUE ~ NA_real_
      ),
    gender = if_else(gender == "f", "Female", "Male"),
    overall = "overall") %>% 
  mutate(.data = ., 
         across(.cols = where(is.character), 
                .fns = ~fct_explicit_na(f = factor(.), na_level = "Unknown")),
         imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = T), 
                                      na_level = "Unknown"),
         imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = T), 
                                        na_level = "Unknown"),
         # year = fct_explicit_na(f = factor(x = year, ordered = T), 
         #                        na_level = "Unknown"),
  ) %>% 
  filter(age >= 18)

# check
pop %>%
  distinct() %>%
  # filter(age >= 18) %>%
  group_by(la_name) %>%
  summarise(la.tot = sum(n, na.rm = T)) %>%
  dplyr::filter(str_detect(la_name, "Leeds")
                # str_detect(lsoa_name, pattern = "^Leeds", negate = T)
  )

StandardiseDemographics <- function(grp, summ){
  if (isTRUE(grp == "overall")){
  pop %>%
    # dplyr::filter(str_detect(la_name, "Leeds")) %>%
    group_by(across({{grp}}), .drop = F) %>%
    summarise(across(.cols = {{summ}}, .fns = ~sum(.), .names = "pop.{.col}")) %>%
    ungroup()
  } else {
    pop %>%
      # dplyr::filter(str_detect(la_name, "Leeds")) %>%
      group_by(across({{grp}}), .drop = F) %>%
      summarise(across(.cols = {{summ}}, .fns = ~sum(.), .names = "pop.{.col}")) %>%
      ungroup()
  }
}

# test
SummariseDemographics(df = a1, grp = gender, summ = person_ref, yearly = F)
StandardiseDemographics(grp = gender, summ = n)

# 2016 to 2021 ons est ----------------------------------------------------------

####
# get_population doesn't work
# .xlsx file headers change in 2018
####

# pop2016 <-
#   get_population(year = 2016, by_sex = T, by_age = T, age_band = T)
# 
# pop2016 %>%
#   filter(str_detect(string = lsoa_name, pattern = "^Leeds")) %>%
#   count(lsoa_name)
#   fill(lsoa_name, .direction = "down") %>%
#   group_by(lsoa_name) %>%
#   slice(.data = ., -1)
# 
# View(pop2016)
# pop2016 %>% 
#   ungroup() %>% 
#   fill(lsoa_name) %>% 
#   filter(str_detect(string = lsoa_name, pattern = "^Leeds")) %>%
#   group_by(lsoa_name) %>% 
#   slice(.data = ., -1) %>%
#   View()
# summarise(tot = sum(count, na.rm = T))
# glimpse
# 
# pop_ons_leeds <-
#   bind_rows(
#     get_population(year = 2016, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds")),
#     get_population(year = 2017, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds")),
#     get_population(year = 2018, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds")),
#     get_population(year = 2019, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds")),
#     get_population(year = 2020, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds")),
#     get_population(year = 2021, by_sex = T, by_age = T) %>%
#       filter(str_detect(string = lsoa_name, pattern = "^Leeds"))
#   )

# glimpse(pop_ons_leeds)
# range(pop_ons_leeds$age, na.rm = T)
# pop_ons_leeds %>% 
#   filter(year == 2016) %>% 
#   count(lsoa_name, sort = T) 

# 
# pop_ons_leeds %>%
#   group_by(year) %>%
#   summarise(n = sum(count))
# 
# ggplot(aes(x = age, y = count, fill = year)) +
#   geom_col(position = "stack") +
#   facet_wrap(~sex)
# 
# pop_ons_leeds %>%
#   # count(year)
#   filter(year == 2019) %>% 
#   summarise(tot = sum(count, na.rm = T))
# 
# NHSRpopulation::lsoa %>% 
#   # filter(la_code == "E08000035" & est_year == 2019) %>% 
#   filter(str_detect(lsoa_name, "^Leeds")) %>% 
#   group_by(est_year) %>% 
#   summarise(tot = sum(n, na.rm = T))
# 
# pop_ons_leeds %>% 
#   filter(year == 2019) 
# View()


# fix bugs in getPop ----------------------------------------------------------------


# end ---------------------------------------------------------------------


