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


## a5 ----------------------------------------------------------------------
# referral level
# use all record
# time to events

# aca data
n_distinct(aca$person_ref)
# 1351 pats
n_distinct(aca$referral_id)
# 1977 refs

aca %>% 
  group_by(person_ref) %>% 
  summarise(n = n_distinct(referral_id)) %>% 
  arrange(desc(n))
# 500 referral_ids with unknown person_ref

a5_df <- 
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
  mutate(age_band = fct_explicit_na(factor(age_band, ordered = F), na_level = "Unknown")) %>%
  relocate(.data = ., c(age_band, overall), .before = id_referral) %>% 
  select(-age_rep)

glimpse(a5_df)
n_distinct(a5_df$person_ref)
n_distinct(a5_df$id_referral)

## a5 support ----------------------------------------------------------------
a5 <-
  a5_df %>% 
  filter(person_ref != "Unknown") %>% 
  filter(date_referral >= ymd("2016-01-01") & date_referral <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  ) %>% 
  distinct() %>% 
  mutate(
    support_provision = case_when(
     !is.na(date_support) ~ "received",
     is.na(date_support) ~ "not received"
     )
  ) %>%
  group_by(person_ref) %>% 
  nest(.data = ., info = !person_ref) %>% 
  mutate(info = map(.x = info, 
                    .f = ~arrange(.data = .x, date_support) %>% 
                      slice_head(.data = ., n = 1)
                    )
         ) %>% 
  unnest(data = ., cols = info) %>% 
  ungroup() %>% 
  mutate(overall = factor("la carers"),
         support_provision = factor(support_provision)) %>% 
  select(-contains("date"), -contains("id")) %>% 
  relocate(.data = ., overall, .after = person_ref)

n_distinct(a5$person_ref)
a5 %>% count(person_ref, sort = T)

a5 %>% 
  group_by(gender, support_provision, .drop = T) %>%
  summarise(
    number_carers = n_distinct(person_ref)) %>%
  mutate(
    pct_carers = number_carers/sum(number_carers)*100
    ) %>% 
  pull(number_carers) %>% 
  sum(.)

# def funcs
SummariseSupport <- 
  function(df, grp, summ){
      df %>% 
        group_by(across(c({{grp}}, support_provision), .drop = F)) %>%  
        summarise(across(.cols = {{summ}},
                         .fns = list(n = ~n_distinct(.)), 
                         .names = "{.fn}.{.col}"), .groups = "keep") %>%
        ungroup() %>% 
        dplyr::mutate(across(starts_with("n."), 
                             .fns = ~.x/sum(.x)*100, 
                             .names = "pct.{.col}")
        ) %>% 
      rename(
        number.carers = n.person_ref,
        pct.carers = pct.n.person_ref)
    } 
  
# test
glimpse(a5)
summ_vars <- c("person_ref")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")
SummariseSupport(df = a5, grp = grp_vars[2], summ = summ_vars[1]) %>% 
  pull(number.carers) %>%
  # pull(pct.carers) %>% 
  sum(.)

## a5 demo --------------------------------------------------------------------
# def vars
summ_vars <- c("person_ref")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T5", rowname, Var1, sep = "_"),
         out = map(.x = Var1, 
                    .f = ~SummariseSupport(df = a5, 
                                           grp = .x, 
                                           summ = summ_vars)
                    )
  )

a5_out_demo <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
a5_out_demo$T5_1_overall %>%
  pull(number.carers) %>%
  sum(.)

a5_out_demo$T5_2_age_band %>%
  pull(number.carers) %>%
  sum(.)

# collect summaries
a5_out <-
  # a5_out_demo
  append(a5_out_demo, a5_out_int)

a5_out$T5_3_gender
# zeros dropped

## a5 int ------------------------------------------------------------------
# def vars
summ_vars <- c("person_ref")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")

int <- 
  RcppAlgos::comboGrid(grp_vars[-1], grp_vars[-1], repetition = F) %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T5", as.integer(rowname)+4, Var1, Var2, sep = "_"))

int <-
  int %>% 
  mutate(out = map2(.x = Var1,
                    .y = Var2,
                    .f = ~SummariseSupport(df = a5, 
                                          grp = c(.x, .y), 
                                          summ = summ_vars
                                          )
         )
  )

a5_out_int <- 
  int %>% 
  pull(out) %>% 
  set_names(x = ., int$nms)

a5_out_int$T5_5_age_band_gender


## a5 plot --------------------------------------------------------------------
# plots
a5_out_demo$T5_1_overall

a5_out_demo_plots <- 
  a5_out_demo %>% 
  map(.x = ., 
      .f = ~pivot_longer(data = ., cols = where(is.numeric), 
                         names_to = "metric", values_to = "values") %>%
        ggplot(aes(x = .data[[names(.x)[1]]], y = values, fill = support_provision)) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(guide = guide_legend(nrow = 1, title.position = "left")) +
        # coord_flip() +
        facet_wrap(~metric, scales = "free_y") +
        # facet_grid(metric~year, scales = "free") +
        theme_bw() +
        theme(legend.position = "top", axis.text = element_text(angle = 0))
      ) %>%
  set_names(x = ., nm = names(a5_out_demo))

a5_out_demo$T5_2_age_band
a5_out_demo_plots$T5_2_age_band

a5_out_int_plots <- 
  a5_out_int %>% 
  map(.x = ., 
      .f = ~
        pivot_longer(data = ., 
                     cols = where(is.numeric), 
                     names_to = "metric", 
                     values_to = "values") %>% 
        dplyr::filter(!metric %in% c("lcl", "ucl", "std.na", "unknown.carers", 
                                     "pcnt.carers", "total.population")
        ) %>% 
        ggplot(aes(x = .data[[names(.x)[1]]], y = values, fill = .data[[names(.x)[2]]])) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(guide = guide_legend(nrow = 1, title.position = "left")) +
        facet_wrap(~support_provision, scales = "free_y", nrow = 1) +
        # facet_grid(metric~year, scales = "free") +
        theme_bw() +
        theme(legend.position = "top")
  ) %>% 
  set_names(x = ., nm = names(a5_out_int))

a5_plots <- 
  append(a5_out_demo_plots, a5_out_int_plots)

# a5 fig ------------------------------------------------------------------
a5_int_fig <- 
  cowplot::plot_grid(plotlist = a5_out_int_plots, 
                     # labels = names(a5_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

# % diff in support heatmap
a5 %>% 
  group_by(gender, age_band, support_provision, .drop = F) %>% 
  summarise(n = n_distinct(person_ref)) %>%
  pivot_wider(data = ., names_from = "support_provision", values_from = "n") %>% 
  replace_na(data = ., replace = list(`not received` = 0, `received` = 0)) %>% 
  mutate(pct_received = received/(`not received` + received)*100) %>% 
  filter(age_band != "0-18" & gender != "Unknown") %>% 
  replace_na(data = ., replace = list(pct_received = 0)) %>% 
  ggplot() +
  geom_tile(aes(x = age_band, y = gender, fill = pct_received)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank())
  View()

# % diff in support heatmap
p5b <- 
  a5 %>% 
  # group_by(gender, age_band, support_provision, .drop = F) %>% 
  group_by(gender, imd_decile, support_provision, .drop = F) %>%
  summarise(n = n_distinct(person_ref)) %>%
  pivot_wider(data = ., names_from = "support_provision", values_from = "n") %>% 
  replace_na(data = ., replace = list(`not received` = 0, `received` = 0)) %>% 
  mutate(pct_received = received/(`not received` + received)*100) %>% 
  # filter(age_band != "0-18" & gender != "Unknown") %>%
  filter(gender != "Unknown") %>%
  replace_na(data = ., replace = list(pct_received = 0)) %>% 
  ggplot() +
  # geom_tile(aes(x = age_band, y = gender, fill = pct_received)) +
  geom_tile(aes(x = imd_decile, y = gender, fill = pct_received)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank()
        )
View()

p5_fig <- 
  cowplot::plot_grid(plotlist = list(p5a, p5b), 
                     # labels = names(a5_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

a5_out$T5_1_overall
  
# end ---------------------------------------------------------------------

a5_out$T5_1_overall
817+533
a5_plots$T5_2_age_band

a5_out$T5_2_age_band %>% 
  select(-pct.carers) %>% 
  pivot_wider(data = ., names_from = support_provision, values_from = number.carers) %>% 
  mutate(prop = `not received`/(`not received` + received)*100) %>% 
  summarise(mean = mean(prop),
            sd = sd(prop))

a5_out$T5_3_gender %>% 
  select(-pct.carers) %>% 
  pivot_wider(data = ., names_from = support_provision, values_from = number.carers) %>% 
  mutate(prop = `not received`/(`not received` + received)*100) %>% 
  summarise(mean = mean(prop),
            sd = sd(prop))

a5_plots$T5_4_imd_decile
a5_out$T5_4_imd_decile %>% 
  select(-pct.carers) %>% 
  pivot_wider(data = ., names_from = support_provision, values_from = number.carers) %>% 
  mutate(prop = `not received`/(`not received` + received)*100) %>% 
  summarise(mean = mean(prop),
            sd = sd(prop))


a5_out$T5_5_age_band_gender
