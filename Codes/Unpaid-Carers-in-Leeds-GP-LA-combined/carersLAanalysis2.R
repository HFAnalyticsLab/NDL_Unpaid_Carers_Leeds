# carers LA analysis

# setup -------------------------------------------------------------------
need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'readxl',
  'odbc',
  'janitor',
  'cowplot',
  'NHSRpopulation',
  'RcppAlgos'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)

config <- get(file = "../config.yml")

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

# rm(list = ls())
# save(list = ls(), file = ".RData")
# load(file = ".RData")

# etl ----------------------------------------------------------------
# aca <- get_cis_formatted()  
glimpse(aca)
# gc(full = T)


# eda ---------------------------------------------------------------------
# aca %>% 
#   group_by(sheet) %>% 
#   summarise(n.pats = n_distinct(person_ref),
#             n.refs = n_distinct(referral_id)
#             )
# 
# aca %>% 
#   # filter(referral_id == "Unknown") 
#   filter(person_ref != "Unknown") %>%
#   summarise(n.refs = n_distinct(referral_id))


## a2 ----------------------------------------------------------------------
# person level
# use earliest record
# time since first carer flag
# number of entries  

# aca data
n_distinct(aca$person_ref)
# 1454 pats
n_distinct(aca$referral_id)
# 2088 refs

aca %>%
  group_by(person_ref) %>%
  summarise(n = n_distinct(referral_id)) %>%
  arrange(desc(n))
# 500 referral_ids with unknown person_ref

# glimpse(aca)

a2_df <- 
  aca %>% 
  select(
      person_ref, age, gender, ethnicity, imd_decile, jca,
      id_referral = referral_id, 
      date_referral = referral_contact_date,
      id_assessment= assessment_id, 
      date_assessment = assessment_start_date,
      id_support = sup_plan_id, 
      date_support = sup_start_date,
      id_service = service_id, 
      date_service = service_start_date
    ) %>% 
    pivot_longer(data = ., cols = !person_ref:jca, 
                 names_to = c(".value", "type"), 
                 names_sep = "_",
                 values_drop_na = F,
                 # names_pattern = 
                 ) %>% 
    rename(index_date = date) %>% 
    mutate(
      age_rep = age,
      age = age_rep - round(as.double((ymd("2023-01-01")-index_date)/365), digits = 0),
      time.since.first.flag = index_date - ymd("2016-01-01"),
      overall = "overall",
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
          TRUE ~ NA_real_)
      ) %>% 
    transmute(
      person_ref,
      age_rep,
      age,
      age_band,
      gender,
      imd_decile,
      imd_quintile,
      overall,
      time.since.first.flag,
      jca,
      index_date,
      event = fct_explicit_na(f = factor(type, 
                                         levels = c("referral", "assessment", "support", "service")),
                              na_level = "Unknown"),
      id
      ) %>% 
  select(-age_rep) %>% 
  mutate(
    imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F), na_level = "Unknown"),
    imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F), na_level = "Unknown")
    ) %>%
  mutate(.data = ., across(.cols = c(where(is.numeric), -age),
                           .fns = as.character)
         ) %>%
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
         )
  

# test
# glimpse(a2_df)

# a2_df %>%
#   group_by(imd_quintile) %>%
#   summarise(n = n_distinct(person_ref)
#   )

# a2_df %>%
#   filter(event == "referral") %>%
#   pull(id) %>%
#   n_distinct()
# 
# a2_df %>%
#   drop_na() %>%
#   # filter(person_ref != "Unknown") %>%
#   filter(person_ref == "1010011436") %>%
#   group_by(person_ref) %>%
#   nest(.data = ., info = !person_ref:jca) %>%
#   mutate(
#     number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
#   )

# n_distinct(a2$person_ref)
# a2 %>%
#   filter(event == "referral") %>%
#   pull(id) %>%
#   n_distinct()

a2 <-
  a2_df %>% 
  filter(person_ref != "Unknown") %>%
  drop_na(data = ., index_date) %>%
  distinct() %>% 
  filter(index_date >= ymd("2016-01-01") & index_date <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  ) %>% 
  group_by(year = year(index_date), person_ref) %>% 
  nest(.data = ., info = !person_ref:overall) %>% 
  mutate(
    number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
    ) %>% 
  unnest(data = ., cols = info) %>% 
  group_by(year, person_ref) %>% 
  arrange(index_date) %>%
  slice_head(., n = 1) %>% 
  ungroup()

# check
# glimpse(a2)
# a2 %>% count(person_ref, sort = T)
# a2 %>% count(year, person_ref, sort = T)
# a2 %>% count(event, sort = T)
# a2 %>% filter(event == "referral") %>% pull(id) %>% n_distinct()
# a2 %>% filter(event != "referral") %>% pull(id) %>% n_distinct()
# a2 %>% filter(person_ref == "1010262661")
# a2 %>% count(imd_quintile)
# range(a2$index_date)
# range(a2$age, na.rm = T)

# def funcs
SummariseDemographics <- 
  function(df, grp, summ, yearly){
    if (isTRUE(yearly)){
      df %>% 
        group_by(year = year(index_date), across({{grp}}, .drop = F)) %>%  
        summarise(across(.cols = {{summ}},
                         .fns = list(n = ~sum(. != "Unknown"), 
                                     na = ~sum(. == "Unknown")), 
                         .names = "{.fn}.{.col}")) %>%
        ungroup() %>% 
        dplyr::mutate(across(starts_with("n."), 
                             .fns = ~.x/sum(.x)*100, 
                             .names = "pcnt.{.col}")
        ) %>% 
        dplyr::mutate(year = fct_explicit_na(f = factor(year, ordered = F),
                                             na_level = "Unknown")
        )
    } else
      df %>% 
      group_by(across({{grp}}, .drop = F)) %>%  
      summarise(across(.cols = {{summ}},
                       .fns = list(n = ~sum(. != "Unknown"), na = ~sum(. == "Unknown")), 
                       .names = "{.fn}.{.col}")) %>%
      ungroup() %>% 
      dplyr::mutate(across(starts_with("n."), 
                           .fns = ~.x/sum(.x)*100, 
                           .names = "pcnt.{.col}")
      ) 
    # dplyr::mutate(year = fct_explicit_na(f = factor(year, ordered = T),
    #                                      na_level = "Unknown")
    # )
  }

# test
# glimpse(a2)
# summ_vars <- c("person_ref")
# grp_vars <- c("overall", "age_band", "gender", "imd_decile", "imd_quintile")
# SummariseDemographics(df = a2, grp = grp_vars[4], summ = summ_vars[1], yearly = T) %>%
#   pull(n.person_ref) %>%
#   sum(.)

## a2 demo --------------------------------------------------------------------
# def vars
# glimpse(a2)
summ_vars <- c("person_ref")
grp_vars <- c("overall","age_band", "gender", "imd_decile")

# demo$summ[[1]]
# demo$std[[1]]

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T2", rowname, Var1, sep = "_"),
         summ = map(.x = Var1, 
                    .f = ~SummariseDemographics(df = a2, 
                                                grp = .x, 
                                                summ = summ_vars, 
                                                yearly = T)
         ),  
         std = map(.x = Var1,
                   .f = ~StandardiseDemographics(pop = pop, 
                                                 grp = .x, 
                                                 summ = n, 
                                                 yearly = T)
                   ),
         out = map2(.x = summ, .y = std,
                    .f = ~left_join(.x, .y) %>%
                      mutate(
                        std.number = n.person_ref / pop.n,
                        lcl = std.number -
                          qnorm(1 - 0.05/2) * sqrt(std.number * (1 - std.number) / pop.n),
                        ucl = std.number +
                          qnorm(1 - 0.05/2) * sqrt(std.number * (1 - std.number) / pop.n),
                        std.na = na.person_ref / pop.n
                      ) %>% 
                      rename(
                        number.carers = n.person_ref,
                        unknown.carers = na.person_ref,
                        pcnt.carers = pcnt.n.person_ref,
                        total.population = pop.n)
         )
  )


a2_out_demo <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
# a2_out_demo$T2_1_overall %>%
#   pull(number.carers) %>%
#   sum(.)
# 
# a2_out_demo$T2_3_gender

# a2_out_demo$T2_2_age_band %>%
#   pull(number.carers) %>%
#   sum(.)

## a2 int ---------------------------------------------------------------------
int <- 
  RcppAlgos::comboGrid(grp_vars[-1], grp_vars[-1], repetition = F) %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T2", as.integer(rowname)+4, Var1, Var2, sep = "_"))

int <-
  int %>% 
  mutate(summ = map2(.x = Var1, .y = Var2, 
                     .f = ~SummariseDemographics(df = a2,
                                                 grp = c(.x, .y), 
                                                 summ = summ_vars, 
                                                 yearly = T)
  ),
  std = map(.x = Var1,
            .f = ~StandardiseDemographics(pop = pop, grp = .x, summ = n, yearly = T)),
  out = map2(.x = summ, .y = std,
             .f = ~left_join(.x, .y) %>%
               mutate(
                 std.number = n.person_ref / pop.n,
                 lcl = std.number -
                   qnorm(1 - 0.05/2) * sqrt(std.number * (1 - std.number) / pop.n),
                 ucl = std.number +
                   qnorm(1 - 0.05/2) * sqrt(std.number * (1 - std.number) / pop.n),
                 std.na = na.person_ref / pop.n
               ) %>%
               rename(
                 number.carers = n.person_ref,
                 unknown.carers = na.person_ref,
                 pcnt.carers = pcnt.n.person_ref,
                 total.population = pop.n)
  )
  )

a2_out_int <- 
  int %>% 
  pull(out) %>% 
  set_names(x = ., int$nms)

# a2_out_int$T2_6_age_band_imd_decile
# a2_out_int$T2_5_age_band_gender %>%
#   arrange(desc(number.carers)) %>%
#   pull(number.carers) %>%
#   sum(.)

# collect summaries
a2_out <-
  append(a2_out_demo, a2_out_int)
# names(a2_out)

# a2_out$T2_2_age_band
# a2_out$T2_6_age_band_imd_decile

## a2 plot --------------------------------------------------------------------
# plots
var0 <- demo$Var1
var1 <- int$Var1
var2 <- int$Var2

a2_out_demo_plots <- 
  a2_out_demo %>% 
  map(.x = ., 
      .f = ~pivot_longer(data = ., cols = where(is.numeric), 
                         names_to = "metric", values_to = "values") %>% 
        dplyr::filter(!metric %in% c("lcl", "ucl", "std.na", "unknown.carers", 
                                     "pcnt.carers", "total.population")
        ) %>%         
        ggplot(aes(x = .data[[names(.x)[2]]], y = values, fill = year)) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(guide = guide_legend(nrow = 1, title.position = "left")) +
        # coord_flip() +
        facet_wrap(~metric, scales = "free_y") +
        # facet_grid(metric~year, scales = "free") +
        theme_bw() +
        theme(legend.position = "top", axis.text = element_text(angle = 0))
  ) %>% 
  set_names(x = ., nm = names(a2_out_demo))

# a2_out_demo_plots$T2_3_gender

a2_out_int_plots <- 
  a2_out_int %>% 
  map(.x = ., 
      .f = ~
        pivot_longer(data = ., 
                     cols = where(is.numeric), 
                     names_to = "metric", 
                     values_to = "values") %>% 
        dplyr::filter(!metric %in% c("lcl", "ucl", "std.na", "unknown.carers", 
                                     "pcnt.carers", "total.population")
        ) %>% 
        ggplot(aes(x = .data[[names(.x)[2]]], y = values, fill = .data[[names(.x)[3]]])) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(guide = guide_legend(nrow = 1, title.position = "left")) +
        # facet_wrap(~metric, scales = "free_y", nrow = 1) +  
        facet_grid(metric~year, scales = "free") +
        theme_bw() +
        theme(legend.position = "top")
  ) %>% 
  set_names(x = ., nm = names(a2_out_int))

# a2_out_int_plots$T2_5_age_band_gender
# 
# a2_out$T2_4_imd_decile %>%
#   pull(number.carers) %>%
#   sum(.)
# 
# a2_out_int_plots$T2_5_age_band_gender
# a2_out_int_plots$T2_6_age_band_imd_decile
# a2_out_int_plots$T2_7_gender_imd_decile

# a2 fig ------------------------------------------------------------------
a2_int_fig <- 
  cowplot::plot_grid(plotlist = a2_out_int_plots, 
                     # labels = names(a2_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

a2_plots <- 
  append(a2_out_demo_plots, a2_out_int_plots)

# a2_plots$T2_5_age_band_gender
# a2_plots$T2_6_age_band_imd_decile
# a2_plots$T2_7_gender_imd_decile

# aca %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()
# 
# a2 %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()

# a2 misc -----------------------------------------------------------------
## time from first carer flag
# a2 %>% count(person_ref, sort = T)
# 
# a2 %>% 
#   summarise(
#     mean.dt = mean(time.since.first.flag, na.rm = T),
#     sd.dt = sd(time.since.first.flag, na.rm = T)
#   )
# 1058/365
# 598/365
# 
# a2 %>% 
#   ggplot(aes(x = time.since.first.flag)) +
#   geom_histogram(binwidth = 93, color = "white", fill = "red") +
#   # facet_wrap(~gender)
#   # facet_wrap(~age_band, scales = "free_y")
#   facet_wrap(~imd_decile)
# 
# # covid impact?
# a2_plots$T2_2_age_band
# a2_plots$T2_5_age_band_gender
# 
# # number of flags
# a2 %>% 
#   ggplot(aes(x = number.of.carer.flags)) +
#   geom_histogram(binwidth = 1, color = "white", fill = "red") +
#   # facet_wrap(~gender)
#   facet_wrap(~age_band, scales = "free_y")
#   # facet_wrap(~imd_decile)

# check services not YET delivered 
# remove unknown ids / dates

a2_out$T2_1_overall %>% 
  summarise(mean = mean(number.carers))
a2_out$T2_2_age_band
a2_plots$T2_2_age_band

a2_out$T2_2_age_band %>% 
  ggplot(aes(x = year, y = number.carers, fill = age_band)) +
  geom_col() +
  facet_wrap(~age_band) +
  theme(legend.position = "top")

a2_out$T2_3_gender %>% 
  ggplot(aes(x = year, y = number.carers, fill = gender)) +
  geom_col(position = "fill") +
  # facet_wrap(~age_band) +
  theme(legend.position = "top")


a2_out$T2_5_age_band_gender %>% 
  ggplot(aes(x = year, y = number.carers, fill = gender)) +
  geom_col(position = "fill") +
  facet_wrap(~age_band) +
  theme(legend.position = "top")

a2_out$T2_6_age_band_imd_decile %>% 
  ggplot(aes(x = year, y = number.carers, fill = imd_decile)) +
  geom_col(position = "dodge") +
  facet_wrap(~age_band) +
  theme(legend.position = "top")


