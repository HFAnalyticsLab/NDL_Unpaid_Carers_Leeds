# carers GP3 analysis

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
save(list = ls(), file = ".RData")
# load(file = ".RData")

# etl ----------------------------------------------------------------
# aca <- get_cis_formatted()  
# glimpse(aca)
# gc(full = T)

a2_gp <- 
  readRDS(file = "data/a2_gp.rds")

n_distinct(a2_gp$nhs_number)
a2_gp %>% 
  count(nhs_number, sort = T) %>% 
  summarise(tot = sum(n))

a2_gp %>% 
  group_by(year) %>% 
  summarise(
    tot = n_distinct(nhs_number)
    )

# age
range(a2_gp$age, na.rm = T)

# date
range(a2_gp$gp_date, na.rm = T)


# eda ---------------------------------------------------------------------
glimpse(a2_gp)

a2_gp %>% count(nhs_number, sort = T)

names(a2_gp)

## a2 gp ----------------------------------------------------------------------

a2_df <- 
  a2_gp %>% 
  select(
    nhs_number, gp_date, sex, age, imd_decile
  ) %>% 
    # pivot_longer(data = ., cols = !person_ref:jca, 
    #              names_to = c(".value", "type"), 
    #              names_sep = "_",
    #              values_drop_na = F,
    #              # names_pattern = 
    #              ) %>% 
    rename(index_date = gp_date,
           person_ref = nhs_number) %>% 
    mutate(
      # age_rep = age,
      # age = age_rep - round(as.double((ymd("2023-01-01")-index_date)/365), digits = 0),
      time.since.first.flag = index_date - ymd("2016-01-01"),
      overall = "overall",
      gender = 
        case_when(
          sex == "F" ~ "Female",
          sex == "M" ~ "Male",
          TRUE ~ NA_character_
          ),
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
      # nhs_number,
      index_date,
      age,
      age_band,
      gender,
      imd_decile,
      imd_quintile,
      overall,
      time.since.first.flag
      ) %>% 
  mutate(
    imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F), 
                                 na_level = "Unknown"),
    imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F), 
                                   na_level = "Unknown")
    ) %>%
  mutate(.data = ., across(.cols = c(where(is.numeric), -age),
                           .fns = as.character)
         ) %>%
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
         )

# test
glimpse(a2_df)
n_distinct(a2_df$person_ref)
a2_df %>% count(person_ref, sort = T)
a1_df %>% count(person_ref, sort = T)

# a2_df %>%
#   drop_na() %>%
#   # filter(person_ref != "Unknown") %>%
#   filter(person_ref == "1010011436") %>%
#   group_by(person_ref) %>%
#   nest(.data = ., info = !person_ref:jca) %>%
#   mutate(
#     number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
#   )

a2 <-
  a2_df %>% 
  # filter(person_ref != "Unknown") %>%
  # drop_na(data = ., index_date) %>%
  distinct() %>% 
  filter(index_date >= ymd("2016-01-01") & index_date <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  )
  # group_by(person_ref) %>% 
  # nest(.data = ., info = !person_ref:overall) %>% 
  # mutate(
  #   number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
  #   ) %>% 
  # unnest(data = ., cols = info) %>% 
  # arrange(index_date) %>%
  # slice_head(., n = 1) %>% 
  # ungroup()

# check
glimpse(a2)
a2 %>% count(person_ref, sort = T)
range(a2$index_date)
range(a2$age, na.rm = T)

# def funcs
SummariseDemographics <- 
  function(df, grp, summ, yearly){
    if (isTRUE(yearly)){
      df %>% 
        group_by(year = year(index_date), across({{grp}}), .drop = F) %>%  
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
      group_by(across({{grp}}), .drop = F) %>%  
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
glimpse(a2)
summ_vars <- c("person_ref")
grp_vars <- c("overall", "age_band", "gender", "imd_decile")
SummariseDemographics(df = a2, grp = grp_vars[4], summ = summ_vars[1], yearly = T)


## a2 demo --------------------------------------------------------------------
# def vars
# glimpse(a2)
summ_vars <- c("person_ref")
grp_vars <- c("overall","age_band", "gender", "imd_decile")

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T2", rowname, Var1, "gp", sep = "_"),
         summ = map(.x = Var1, 
                    .f = ~SummariseDemographics(df = a2, 
                                                grp = .x, 
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

a2_out_demo_gp <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
# a2_out_demo_gp$T2_1_overall_gp %>%
#   pull(number.carers) %>%
#   sum(.)
# 
# a2_out_demo_gp$T1_2_age_band %>%
#   pull(number.carers) %>%
#   sum(.)

## a2 int ---------------------------------------------------------------------
int <- 
  RcppAlgos::comboGrid(grp_vars[-1], grp_vars[-1], repetition = F) %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T2", as.integer(rowname)+4, Var1, Var2, "gp", sep = "_"))

int <-
  int %>% 
  mutate(summ = map2(.x = Var1, .y = Var2, 
                     .f = ~SummariseDemographics(df = a2,
                                                 grp = c(.x, .y), 
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

a2_out_int_gp <- 
  int %>% 
  pull(out) %>% 
  set_names(x = ., int$nms)

# a2_out_int_gp$T1_5_age_band_gender %>%
#   arrange(desc(number.carers)) %>%
#   pull(number.carers) %>%
#   sum(.)

# collect summaries
a2_out_gp <-
  append(a2_out_demo_gp, a2_out_int_gp)
# names(a2_out)


## a2 plot --------------------------------------------------------------------
# plots
var0 <- demo$Var1
var1 <- int$Var1
var2 <- int$Var2

a2_out_demo_gp_plots <- 
  a2_out_demo_gp %>% 
  map(.x = ., 
      .f = ~pivot_longer(data = ., cols = where(is.numeric), 
                         names_to = "metric", values_to = "values") %>% 
        dplyr::filter(!metric %in% c("lcl", "ucl", "std.na", "unknown.carers")) %>% 
        ggplot(aes(x = .data[[names(.x)[1]]], y = values)) +
        geom_col(position = "dodge") +
        facet_wrap(~metric, scales = "free_y", nrow = 1) +  
        theme_bw() +
        theme(legend.position = "top")
  ) %>% 
  set_names(x = ., nm = names(a2_out_demo_gp))

# a2_out_demo_gp_plots$T2_1_overall_gp

a2_out_int_gp_plots <- 
  a2_out_int_gp %>% 
  map(.x = ., 
      .f = ~ pivot_longer(data = ., 
                          cols = where(is.numeric), 
                          names_to = "metric", 
                          values_to = "values") %>% 
        dplyr::filter(!metric %in% c("lcl", "ucl", "std.na", "unknown.carers",
                                     "pcnt.carers", "total.population")
        ) %>% 
        ggplot(aes(x = .data[[names(.x)[1]]], y = values, fill = .data[[names(.x)[2]]])) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(guide = guide_legend(nrow = 1, title.position = "left")) +
        facet_wrap(~metric, scales = "free_y", nrow = 1) +  
        theme_bw() +
        theme(legend.position = "top")
  ) %>% 
  set_names(x = ., nm = names(a2_out_int_gp))

# a2_out_gp$T1_4_imd_decile %>%
#   pull(number.carers) %>%
#   sum(.)

# a2_out_int_gp_plots$T2_5_age_band_gender_gp
# a2_out_int_gp_plots$T1_6_age_band_imd_decile
# a2_out_int_gp_plots$T1_7_gender_imd_decile


## a2 write ----------------------------------------------------------------


# ignore - from LA analysis -----------------------------------------------


## a2 fig ------------------------------------------------------------------
# a2_int_gp_fig <- 
#   cowplot::plot_grid(plotlist = a2_out_int_plots, 
                     # labels = names(a2_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     # ncol = 1)

# a2_plots_gp <- 
#   append(a2_out_demo_gp_plots, a2_out_int_gp_plots)

# a2_plots_gp$T1_5_age_band_gender
# a2_plots_gp$T1_6_age_band_imd_decile
# a2_plots_gp$T1_7_gender_imd_decile

# aca %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()
# 
# a2 %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()

# a2 %>% count(person_ref)
# a2_plots_gp$T1_5_age_band_gender
# a2_plots_gp$T1_6_age_band_imd_decile_gp
# a2_plots_gp$T1_7_gender_imd_decile

# p1a <- 
#   a2 %>% 
#   filter(gender != "Not Specified") %>% 
#   ggplot(aes(x = age_band, fill = gender)) +
#   geom_bar(position = "fill") +
#   scale_fill_brewer(palette = 6) +
#   scale_y_continuous(labels = scales::percent) +
#   xlab(label = "age band") +
#   ylab(label = "proportion of carers") +
#   theme_minimal() +
#   theme(legend.position = "top", 
#         legend.title = element_blank()
#         )
# p1a
# 
# 
# p1b <- 
#   a2 %>% 
#   filter(gender != "Not Specified") %>% 
#   ggplot(aes(x = imd_decile, fill = gender)) +
#   geom_bar(position = "fill", alpha = 0.5) +
#   scale_fill_viridis_d() +
#   scale_y_continuous(labels = scales::percent) +
#   xlab(label = "IMD decile") +
#   ylab(label = "proportion of carers") +
#   theme_minimal() +
#   theme(legend.position = "none", 
#         legend.title = element_blank()
#   )
# 
# a2_fig <- 
#   cowplot::plot_grid(plotlist = list(p1a, p1b), 
#                      ncol = 1)


## a2 misc -----------------------------------------------------------------
## time from first carer flag
# a2 %>% count(person_ref, sort = T)
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
# a2_plots$T1_2_age_band
# a2_plots$T1_5_age_band_gender
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

# a2_out$T1_1_overall
# a2 %>% 
#   filter(event == "referral") %>% 
#   pull(person_ref) %>% 
#   n_distinct(.)
# 
# glimpse(aca)
# aca2 <- aca %>% 
#   filter(referral_contact_date >= ymd("2016-01-01") & 
#            referral_contact_date <= ymd("2021-12-31") &
#            (age >= 18 | is.na(age))
#   )
# 
# n_distinct(aca2$person_ref)
# n_distinct(aca2$referral_id)-500
# 
# aca2 %>% count(service_desc, sort = T)
# 1350/650000*100
# 
# glimpse(aca2)
# 
# aca2 %>% 
#   distinct(.data = ., person_ref, age) %>% 
#   skimr::skim_without_charts()
# 
# glimpse(aca)
# aca %>% 
#   group_by(imd_decile) %>% 
#   summarise(n = n_distinct(person_ref, na.rm = T)) %>% 
#   arrange(desc(n))
# 
# a2 %>% 
#   group_by(gender, age_band, imd_decile) %>% 
#   summarise(
#     n = n_distinct(person_ref, na.rm = T)
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     pct = n/sum(n)*100
#     ) %>% 
#   View()
#   
# a2_out$T1_1_overall
# a2_out$T1_2_age_band
# a2_plots$T1_2_age_band
# a2_out$T1_3_gender
# a2 %>% skimr::skim_without_charts()
# 290/1361
# range(aca2$age, na.rm = T)
# a2_out$T1_4_imd_decile
# a2_out$T2_1_overall
# 
# a2_out$T1_5_age_band_gender
# a2_plots$T1_5_age_band_gender
# a2_plots$T1_4_imd_decile
# a2_plots$T1_6_age_band_imd_decile
# 
# pop %>% 
#   group_by(gender) %>% 
#   summarise(n = sum(n, na.rm = T)/6) %>% 
#   mutate(pct = n/sum(n)*100)
# 
# pop %>% 
#   group_by(imd_decile) %>% 
#   summarise(n = sum(n, na.rm = T)/6) %>% 
#   mutate(pct = n/sum(n)*100) %>% 
#   pull(n) %>% 
#   sum(.)
# 
# pop %>% 
#   ggplot(aes(x = year, y = n, fill = imd_decile)) +
#   geom_col(position = "dodge")
# 
# pop %>% 
#   group_by(year, age_band, imd_decile) %>% 
#   summarise(tot = sum(n)) %>% 
#   ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
#   geom_col(position = "dodge") +
#   facet_wrap(~year)
# 
# p1d <- 
#   pop %>% 
#   group_by(age_band, imd_decile) %>% 
#   summarise(tot = sum(n)/6) %>%
#   filter(imd_decile %in% c("1", "7")) %>% 
#   ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
#   geom_col(position = "fill")
# 
# a2_out$T1_5_age_band_gender %>% 
#   select(age_band:number.carers) %>% 
#   filter(gender != "Not Specified") %>% 
#   pivot_wider(data = ., names_from = "gender", values_from = "number.carers") %>% 
#   mutate(prop = Female/(Female+Male)*100)
# 
# a5_out$T5_1_overall %>% 
#   pull(number.carers) %>% 
#   sum(.)/6
# 
# a2_out$T1_6_age_band_imd_decile %>% 
#   arrange(imd_decile) %>% 
#   View()
# a2_plots$T1_6_age_band_imd_decile
# a2_plots$T1_7_gender_imd_decile
# a2_out$T1_7_gender_imd_decile %>% 
#   filter(gender != "Not Specified" & imd_decile %in% c(1,7)) %>% 
#   select(gender:number.carers) %>% 
#   pivot_wider(data = ., names_from = "imd_decile", values_from = "number.carers") %>% 
#   mutate(prop = `1`/(`7`+`1`))
# 
# a2 %>% 
#   summarise(
#     mean.tsff = mean(time.since.first.flag),
#     sd.tsff = sd(time.since.first.flag)
#   )
# 1053/365
# 600/365
# 
# # Age vs high IMD / Mid IMD ratio 
# a2_plots$T1_6_age_band_imd_decile
# 
# p1c <- 
#   a2_out$T1_6_age_band_imd_decile %>% 
#   filter(imd_decile != "Unknown") %>% 
#   mutate(imd_quintile = fct_collapse(.f = imd_decile,
#                                      "1" = c("1", "2"),
#                                      "2" = c("3", "4"),
#                                      "3" = c("5", "6"),
#                                      "4" = c("7", "8"),
#                                      "5" = c("9", "10")
#                                      )
#   ) %>% 
#   # filter(imd_quintile %in% c("1","4")) %>%
#   filter(imd_decile %in% c("1","7")) %>%
#   # ggplot(aes(x = age_band, y = std.number, fill = imd_quintile)) +
#   ggplot(aes(x = age_band, y = number.carers, fill = imd_decile)) +
#   geom_col(position = "fill", alpha = 0.5) +
#   scale_fill_viridis_d(option = "D") +
#   scale_y_continuous(labels = scales::percent) +
#   guides(fill = guide_legend(title = "IMD decile")) +
#   xlab("age band") +
#   ylab("proportion of carers") +
#   # facet_wrap(~names) +
#   theme_minimal() +
#   theme(legend.position = "top")
# 
# p1d <- 
#   pop %>% 
#   group_by(age_band, imd_decile) %>% 
#   summarise(tot = sum(n)/6) %>%
#   filter(imd_decile %in% c("1", "7")) %>% 
#   ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
#   geom_col(position = "fill") +
#   scale_fill_viridis_d(option = "D", alpha = 0.5) +
#   scale_y_continuous(labels = scales::percent) +
#   guides(fill = guide_legend(title = "IMD decile")) +
#   xlab("age band") +
#   ylab("proportionof population") +
#   # facet_wrap(~names) +
#   theme_minimal() +
#   theme(legend.position = "top")
# 
# 
# 
# p1a
# p1b
# p1c
# p1d
# 
# ls(pattern = "^p")
# 
# p1_fig <- 
#   cowplot::plot_grid(plotlist = list(p1c, p1d), 
#                      ncol = 2)
# 
# p1_fig_a <- p1a
# 
# p1_fig_b <-
#   ggpubr::ggarrange(plotlist = list(p1c, p1d), 
#                     common.legend = T
#                     )
# 
# 
# # Gender vs high IMD / Mid IMD ratio 
# a2_plots$T1_7_gender_imd_decile
# a2_out$T1_7_gender_imd_decile %>% 
#   filter(imd_decile != "Unknown") %>% 
#   mutate(imdq = fct_collapse(.f = imd_decile,
#                              "1" = c("1", "2"),
#                              "2" = c("3", "4"),
#                              "3" = c("5", "6"),
#                              "4" = c("7", "8"),
#                              "5" = c("9", "10")
#   )
#   ) %>% 
#   # filter(imdq %in% c("1","4")) %>%
#   filter(imd_decile %in% c("1","10")) %>% 
#   # ggplot(aes(x = gender, y = number.carers, fill = fct_rev(imdq))) +
#   ggplot(aes(x = gender, y = number.carers, fill = fct_rev(imd_decile))) +
#   geom_col(position = "fill") +
#   # facet_wrap(~names) +
#   theme(legend.position = "top")
# 
# a5_plots$T5_2_age_band
# a5_out$T5_2_age_band
# 
# a2_out$T1_1_overall
# 1361/622665*100
# 1350/65000*100
# a2_out$T2_1_overall
# 
# 533/65000*100
# 533/622665*100
# 
# 
# replace()
# replace_na()
# 
# a2_out$T1_2_age_band %>% 
#   replace_na(., list(total.population = 0, 
#                      std.number = 0,
#                      lcl = 0,
#                      ucl = 0,
#                      std.na = 0
#                      )
#              )
# 
# a2_out$T2_6_age_band_imd_decile %>% 
#   mutate(
#     number.carers = if_else(number.carers < 5, as.integer(0), number.carers)
#     ) %>% 
#   select(-c(unknown.carers:std.na))
# 
# list(
#   a2_out,
#   a2_out,
#   a4_out,
#   a5_out
# ) %>% 
#   # map_depth(.x = ., .depth = 2, .f = ~names(.x))
#   map_depth(.x = ., .depth = 2, 
#             .f = ~str_subset(string = names(.x), pattern = "number")
#             )
# 
# a2_out %>% 
#   map(.x = ., .f = ~select(.data = .x, -unknown.carers, - ))
# a2_out$T1_1_overall
# a5_out$T5_7_gender_imd_decile
# a5_out$T5_5_age_band_gender
# 
# 97.5/7.5



