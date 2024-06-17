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
aca <- 
  get_cis_formatted()  
glimpse(aca)


# eda ---------------------------------------------------------------------
aca %>% 
  group_by(sheet) %>% 
  summarise(n.pats = n_distinct(person_ref),
            n.refs = n_distinct(referral_id)
            )

aca %>% 
  # filter(referral_id == "Unknown") 
  filter(person_ref != "Unknown") %>%
  summarise(n.refs = n_distinct(referral_id))


## a4 ----------------------------------------------------------------------
# referral level
# use all record
# time to events

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

a4_df <- 
  aca %>% 
  select(
      person_ref, age, gender, ethnicity, imd_decile, imd_quintile,
      id_referral = referral_id, 
      date_referral = referral_contact_date,
      id_assessment= assessment_id, 
      date_assessment = assessment_start_date,
      id_support = sup_plan_id, 
      date_support = sup_start_date,
      id_service = service_id, 
      date_service = service_start_date
    ) %>% 
  # pivot_longer(data = ., cols = !person_ref:jca, 
  #              names_to = c(".value", "type"), 
  #              names_sep = "_",
  #              values_drop_na = F,
  #              # names_pattern = 
  # ) %>% 
  # rename(index_date = date) %>% 
  mutate(
    age_rep = age,
    age = age_rep - round(as.double((ymd("2023-01-01")-date_referral)/365), digits = 0),
    # time.since.first.flag = index_date - ymd("2016-01-01"),
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
      )
  ) %>% 
  relocate(.data = ., c(age_band, overall), .before = id_referral) %>% 
  select(-age_rep)
    # imd_quintile =
    #   case_when(
    #     between(imd_decile, 1, 2) ~ 1,
    #     between(imd_decile, 3, 4) ~ 2,
    #     between(imd_decile, 5, 6) ~ 3,
    #     between(imd_decile, 7, 8) ~ 4,
    #     between(imd_decile, 9, 10) ~ 5,
    #     TRUE ~ NA_real_)
  # )
  # transmute(
  #   person_ref,
  #   age_rep,
  #   age,
  #   age_band,
  #   gender,
  #   imd_decile,
  #   imd_quintile,
  #   overall,
  #   time.since.first.flag,
  #   jca,
  #   index_date,
  #   event = fct_explicit_na(f = factor(type, 
  #                                      levels = c("referral", "assessment", "support", "service")),
  #                           na_level = "Unknown"),
  #   id
  # ) %>% 
  # select(-age_rep) %>% 
  # mutate(
  #   imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F), na_level = "Unknown"),
  #   imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F), na_level = "Unknown")
  # ) %>%
  # mutate(.data = ., across(.cols = c(where(is.numeric), -age),
  #                          .fns = as.character)
  # ) %>%
  # mutate(.data = ., across(.cols = where(is.character),
  #                          .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
  # )

glimpse(a4_df)
n_distinct(a4_df$person_ref)

aca %>% 
  filter(referral_id == "757759") %>% 
  View()

a4 <-
  a4_df %>% 
  filter(person_ref != "Unknown") %>%
  filter(date_referral >= ymd("2016-01-01") & date_referral <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  ) %>% 
  distinct()
  
n_distinct(a4$person_ref)

## a4 paths ----------------------------------------------------------------
a4_ref_amt <- 
  a4 %>% 
  select(person_ref:overall, contains(c("referral", "assessment"))) %>% 
  distinct() %>% 
  mutate(
    period = date_referral,
    source = case_when(
     !is.na(date_referral) ~ "referral"
     ),
    destination = case_when(
      !is.na(date_assessment) ~ "assessment",
      is.na(date_assessment) ~ "no assessment"
      ),
    dt_event = date_assessment - date_referral,
    dt_referral = date_assessment - date_referral,
    overall = factor("la carers")
  ) %>% 
  select(-starts_with("date"))

a4_amt_sup <- 
  a4 %>% 
  filter(person_ref != "Unknown") %>% 
  select(person_ref:overall, date_referral, id_referral,
         date_assessment, date_support) %>% 
  distinct() %>%
  mutate(
    period = date_referral,
    source = case_when(
      !is.na(date_assessment) ~ "assessment",
      is.na(date_assessment) ~ "no assessment"
    ),
    destination = case_when(
      !is.na(date_support) ~ "support plan",
      is.na(date_support) ~ "no support plan"
    ),
    dt_event = date_support - date_assessment,
    dt_referral = date_support - date_referral,
    overall = factor("la carers")
  ) %>% 
  select(-starts_with("date"))

a4_sup_serv <- 
  a4 %>% 
  filter(person_ref != "Unknown") %>%
  select(person_ref:overall, date_referral, id_referral,
         date_support, date_service) %>% 
  mutate(
    period = date_referral,
    source = case_when(
      !is.na(date_support) ~ "support plan",
      is.na(date_support) ~ "no support plan"
    ),
    destination = case_when(
      !is.na(date_service) ~ "services delivered",
      is.na(date_service) ~ "no services delivered"
    ),
    dt_event = date_service - date_support,
    dt_referral = date_service - date_referral,
    overall = factor("la carers")
  ) %>% 
  select(-starts_with("date"))
  
# chk
glimpse(a4_sup_serv)
a4_sup_serv %>%
  select(-contains("dt")) %>% 
  distinct() %>%
  # count(id_referral, sort = T)
  group_by(source, destination) %>% 
  summarise(n = n_distinct(id_referral, na.rm = T)) %>% 
  pull(n) %>%
  sum(.)

names(a4_ref_amt)
names(a4_amt_sup)
names(a4_sup_serv)
dim(a4_ref_amt)
dim(a4_amt_sup)
dim(a4_sup_serv)

glimpse(a4_paths)
a4_paths <-
  bind_rows(
    a4_ref_amt,
    a4_amt_sup,
    # a4_sup_serv
    ) %>% 
  select(-person_ref, -starts_with("dt")) %>%
  distinct() %>% 
  mutate(age_band = fct_explicit_na(factor(age_band), na_level = "Unknown")) %>% 
  mutate(across(.cols = c(source, destination), 
                .fns = ~factor(., 
                               levels = c("referral", "assessment", "support plan", "services delivered",
                                             "no support plan", "no services delivered")
                               )
                )
         ) 

glimpse(a4_paths)
n_distinct(a4_paths$id_referral)
# a4_paths %>% count(id_referral, sort = T)
# a4_paths %>% filter(id_referral == "757759")
# a4_df %>% filter(id_referral == "757759") %>% View()
n_distinct(a4$id_referral)
n_distinct(a4$id_assessment)
n_distinct(a4$id_support)
n_distinct(a4$id_service)

a4_paths_out <- 
  a4_paths %>%
  group_by(source, destination, .drop = T) %>%
  # group_by(carers = gender, source, destination, .drop = T) %>%
  # group_by(period = year(period), carers = gender, source, destination) %>%
  summarise(number.of.requests = n_distinct(id_referral)) %>% 
  arrange(source)

glimpse(a4_paths_out)
992+457+28
n_distinct(a4_paths$id_referral)
a4_paths %>% count(id_referral, id_assessment, sort = T)

a4_paths_out %>% 
  group_by(source) %>% 
  summarise(n = sum(number.of.requests))

# def funcs
SummarisePaths <- 
  function(df, grp, summ, yearly){
    if (isTRUE(yearly)){
      df %>% 
        group_by(period = year(period), across(c({{grp}}, source, destination), .drop = F)) %>%  
        summarise(across(.cols = {{summ}},
                         .fns = list(n = ~n_distinct(.)), 
                         .names = "{.fn}.{.col}"), .groups = "keep") %>%
        ungroup() %>% 
        dplyr::mutate(across(starts_with("n."), 
                             .fns = ~.x/sum(.x)*100, 
                             .names = "pct.{.col}")
        ) %>%  
        dplyr::mutate(period = fct_explicit_na(f = factor(period, ordered = T),
                                             na_level = "Unknown")
        ) %>% 
        rename(
          number.requests = n.id_referral,
          pct.requests = pct.n.id_referral
          )
    } else
      df %>% 
        group_by(across(c({{grp}}, source, destination), .drop = F)) %>%  
        summarise(across(.cols = {{summ}},
                         .fns = list(n = ~n_distinct(.)), 
                         .names = "{.fn}.{.col}"), .groups = "keep") %>%
        ungroup() %>% 
        dplyr::mutate(across(starts_with("n."), 
                             .fns = ~.x/sum(.x)*100, 
                             .names = "pct.{.col}")
      ) %>% 
      rename(
        number.requests = n.id_referral,
        pct.requests = pct.n.id_referral
      )
  }

# test
glimpse(a4_paths)
summ_vars <- c("id_referral")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")
SummarisePaths(df = a4_paths, grp = grp_vars[2], summ = summ_vars[1], yearly = T) %>% 
  pull(number.requests) %>% 
  sum(.)


## a4 tte ------------------------------------------------------------------
a4_tte <-
  bind_rows(
    a4_ref_amt,
    a4_amt_sup
    # a4_sup_serv
  ) %>% 
  select(-person_ref) %>%
  distinct() %>% 
  mutate(age_band = fct_explicit_na(factor(age_band), na_level = "Unknown")) %>% 
  mutate(across(.cols = where(is.character), 
                .fns = ~factor(., 
                               levels = c("referral", "assessment", "support plan", "services delivered",
                                          "no support plan", "no services delivered")
                               )
                )
         ) 

a4_tte_out <- 
  a4_tte %>% 
  # group_by(source, destination, .drop = F) %>% 
  # group_by(carers = gender, source, destination, .drop = F) %>%
  group_by(period = year(period), carers = overall, source, destination) %>%
  summarise(
    number.of.requests = n_distinct(id_referral),
    mean.time.to.event = mean(as.numeric(dt_event), na.rm = T),
    sd.time.to.event = sd(as.numeric(dt_event), na.rm = T),
    mean.time.from.referral = mean(as.numeric(dt_referral), na.rm = T),
    sd.time.from.referral = sd(as.numeric(dt_referral), na.rm = T)
    ) %>% 
  ungroup() %>% 
  drop_na() %>%
  arrange(source)


SummariseTTE <- 
  function(df, grp, summ, yearly){
    if (isTRUE(yearly)){
      df %>% 
        group_by(period = year(period), across(c({{grp}}, source, destination), .drop = F)) %>%  
        summarise(across(.cols = {{summ}},
                         .fns = list(mean = ~mean(as.numeric(.), na.rm = T), 
                                     sd = ~sd(as.numeric(.), na.rm = T)
                                     ), 
                         .names = "{.fn}_{.col}"), .groups = "keep") %>%
        ungroup() %>% 
        dplyr::mutate(period = fct_explicit_na(f = factor(period, ordered = T),
                                               na_level = "Unknown")
        )
        # rename(
        #   number.requests = n.id_referral,
        #   pct.requests = pct.n.id_referral
        # )
    } else
      df %>% 
      group_by(across(c({{grp}}, source, destination), .drop = F)) %>%  
      summarise(across(.cols = {{summ}},
                       .fns = list(mean = ~mean(as.numeric(.), na.rm = T), 
                                   sd = ~sd(as.numeric(.), na.rm = T)
                       ), 
                       .names = "{.fn}_{col}"), .groups = "keep") %>%
      ungroup()
      # rename(
      #   number.requests = n.id_referral,
      #   pct.requests = pct.n.id_referral
      # )
  }

# test
glimpse(a4_tte)
summ_vars <- c("dt_event", "dt_referral")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")
SummariseTTE(df = a4_tte, grp = grp_vars[1], summ = summ_vars[1], yearly = F) 



## a4 demo paths --------------------------------------------------------------------
# def vars
# glimpse(a4)
summ_vars <- c("id_referral")
grp_vars <- c("overall","age_band", "gender", "imd_decile")

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T4", rowname, Var1, "paths", sep = "_"),
         out = map(.x = Var1, 
                    .f = ~SummarisePaths(df = a4_paths, 
                                         grp = .x, 
                                         summ = summ_vars, 
                                         yearly = F)
                   )
         )

a4_out_demo_path <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
a4_out_demo_path$T4_1_overall_paths
a4_out_demo_path$T4_2_age_band_paths
a4_out_demo_path$T4_3_gender_paths



## a4 demo tte --------------------------------------------------------------------
# def vars
# glimpse(a4)

summ_vars <- c("dt_event", "dt_referral")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T4", as.integer(rowname)+4, Var1, "tte", sep = "_"),
         out = map(.x = Var1, 
                   .f = ~SummariseTTE(df = a4_tte, 
                                        grp = .x, 
                                        summ = summ_vars, 
                                        yearly = F)
         )
  )

a4_out_demo_tte <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
a4_out_demo_tte

# collect summaries
a4_out <-
  append(a4_out_demo_path, a4_out_demo_tte)
# names(a4_out)

a4_out$T4_1_overall_paths
# zeros dropped

## a4 plot --------------------------------------------------------------------
# plots
var0 <- demo$Var1
var1 <- int$Var1
var2 <- int$Var2

a4_out_demo_plots <- 
  a4_out_demo %>% 
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
  set_names(x = ., nm = names(a4_out_demo))

a4_out_demo_plots$T2_3_gender

a4_out_int_plots <- 
  a4_out_int %>% 
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
  set_names(x = ., nm = names(a4_out_int))

a4_out_int_plots$T2_5_age_band_gender

a4_int_fig <- 
  cowplot::plot_grid(plotlist = a4_out_int_plots, 
                     # labels = names(a4_out_demo_plots),
                     # label_size = 10, 
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8, 
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

a4_out$T2_1_overall %>% 
  pull(number.carers) %>% 
  sum(.)

a4_out_int_plots$T2_5_age_band_gender

# a4 fig ------------------------------------------------------------------
a4_int_fig <- 
  cowplot::plot_grid(plotlist = a4_out_int_plots, 
                     # labels = names(a4_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

a4_plots <- 
  append(a4_out_demo_plots, a4_out_int_plots)


# a4 misc -----------------------------------------------------------------
## time from first carer flag
a4 %>% count(person_ref, year, sort = T)

a4 %>% 
  group_by(year) %>% 
  summarise(
    mean.dt = mean(time.since.first.flag, na.rm = T),
    sd.dt = sd(time.since.first.flag, na.rm = T)
  )
1058/365
598/365

a4 %>% 
  ggplot(aes(x = time.since.first.flag)) +
  geom_histogram(binwidth = 93, color = "white", fill = "red") +
  # facet_wrap(~gender)
  # facet_wrap(~age_band, scales = "free_y")
  facet_wrap(~imd_decile, scales = "free_y")

# number of flags
a4 %>% 
  ggplot(aes(x = number.of.carer.flags)) +
  geom_histogram(binwidth = 1, color = "white", fill = "red") +
  facet_grid(year~gender, scales = "free_y")
  # facet_grid(year~age_band, scales = "free_y")
  # facet_wrap(~imd_decile)

# check services not YET delivered 
# remove unknown ids / dates

a4 %>% 
  group_by(id_referral) %>% 
  summarise(n.pat = n_distinct(person_ref),
            n.ass = n_distinct(id_assessment),
            n.sup = n_distinct(id_support),
            n.ser = n_distinct(id_service)
            ) %>% 
  arrange(desc(n.ser))

a4_out$T4_1_overall_paths
568/1477*100
1034/1477*100
a4_out$T4_2_age_band_paths

a4_out$T4_5_overall_tte