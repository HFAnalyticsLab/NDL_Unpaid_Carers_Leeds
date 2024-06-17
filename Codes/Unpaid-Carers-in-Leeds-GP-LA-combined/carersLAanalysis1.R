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
# glimpse(aca)
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


## a1 ----------------------------------------------------------------------
# person level
# use earliest record
# time since first carer flag
# number of entries  

# aca data
# n_distinct(aca$person_ref)
# 1454 pats
# n_distinct(aca$referral_id)
# 2088 refs

# aca %>%
#   group_by(person_ref) %>%
#   summarise(n = n_distinct(referral_id)) %>%
#   arrange(desc(n))
# 500 referral_ids with unknown person_ref

# glimpse(aca)

a1_df <- 
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
# glimpse(a1_df)  
n_distinct(a1_df$person_ref)
# a1_df %>%
#   filter(event == "referral") %>%
#   pull(id) %>%
#   n_distinct()
# 
# a1_df %>%
#   drop_na() %>%
#   # filter(person_ref != "Unknown") %>%
#   filter(person_ref == "1010011436") %>%
#   group_by(person_ref) %>%
#   nest(.data = ., info = !person_ref:jca) %>%
#   mutate(
#     number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
#   )

# n_distinct(a1$person_ref)
# a1 %>%
#   filter(event == "referral") %>%
#   pull(id) %>%
#   n_distinct()

a1 <-
  a1_df %>% 
  filter(person_ref != "Unknown") %>%
  drop_na(data = ., index_date) %>%
  distinct() %>% 
  filter(index_date >= ymd("2016-01-01") & index_date <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  ) %>% 
  group_by(person_ref) %>% 
  nest(.data = ., info = !person_ref:overall) %>% 
  mutate(
    number.of.carer.flags = map_int(.x = info, .f = ~nrow(.x))
    ) %>% 
  unnest(data = ., cols = info) %>% 
    arrange(index_date) %>%
    slice_head(., n = 1) %>% 
    ungroup()

# check
# glimpse(a1)
a1 %>% count(person_ref, sort = T)
# a1 %>% count(event, sort = T)
# a1 %>% filter(event == "referral") %>% pull(id) %>% n_distinct()
# a1 %>% filter(event != "referral") %>% pull(id) %>% n_distinct()
# a1 %>% filter(person_ref == "1010205022")
# a1 %>% count(imd_quintile)
# range(a1$index_date)
# range(a1$age, na.rm = T)

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
# glimpse(a1)
# summ_vars <- c("person_ref")
# grp_vars <- c("overall", "age_band", "gender", "imd_decile", "imd_quintile")
# SummariseDemographics(df = a1, grp = grp_vars[5], summ = summ_vars[1], yearly = F) %>%
#   pull(n.person_ref) %>% 
#   sum(.)

## a1 demo --------------------------------------------------------------------
# def vars
# glimpse(a1)
summ_vars <- c("person_ref")
grp_vars <- c("overall","age_band", "gender", "imd_decile")

demo <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T1", rowname, Var1, sep = "_"),
         summ = map(.x = Var1, 
                    .f = ~SummariseDemographics(df = a1, 
                                                grp = .x, 
                                                summ = summ_vars, 
                                                yearly = F)
         ),  
         std = map(.x = Var1,
                   .f = ~StandardiseDemographics(pop = pop, grp = .x, summ = n, yearly = F)),
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

a1_out_demo <-
  demo %>%
  pull(out) %>% 
  set_names(x = ., demo$nms)

# check
# a1_out_demo$T1_1_overall %>%
#   pull(number.carers) %>%
#   sum(.)

# a1_out_demo$T1_2_age_band %>%
#   pull(number.carers) %>%
#   sum(.)

## a1 int ---------------------------------------------------------------------
int <- 
  RcppAlgos::comboGrid(grp_vars[-1], grp_vars[-1], repetition = F) %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T1", as.integer(rowname)+4, Var1, Var2, sep = "_"))

int <-
  int %>% 
  mutate(summ = map2(.x = Var1, .y = Var2, 
                     .f = ~SummariseDemographics(df = a1,
                                                 grp = c(.x, .y), 
                                                 summ = summ_vars, 
                                                 yearly = F)
  ),
  std = map(.x = Var1,
            .f = ~StandardiseDemographics(pop = pop, 
                                          grp = .x, 
                                          summ = n, 
                                          yearly = F)
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

a1_out_int <- 
  int %>% 
  pull(out) %>% 
  set_names(x = ., int$nms)

# a1_out_int$T1_5_age_band_gender %>%
#   arrange(desc(number.carers)) %>%
#   pull(number.carers) %>%
#   sum(.)

# collect summaries
a1_out <-
  append(a1_out_demo, a1_out_int)
# names(a1_out)


## a1 plot --------------------------------------------------------------------
# plots
var0 <- demo$Var1
var1 <- int$Var1
var2 <- int$Var2

a1_out_demo_plots <- 
  a1_out_demo %>% 
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
  set_names(x = ., nm = names(a1_out_demo))

# a1_out_demo_plots$T1_2_age_band

a1_out_int_plots <- 
  a1_out_int %>% 
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
  set_names(x = ., nm = names(a1_out_int))

# a1_out$T1_4_imd_decile %>% 
#   pull(number.carers) %>% 
#   sum(.)

a1_out_int_plots$T1_5_age_band_gender
# a1_out_int_plots$T1_6_age_band_imd_decile
# a1_out_int_plots$T1_7_gender_imd_decile

# a1 fig ------------------------------------------------------------------
a1_int_fig <- 
  cowplot::plot_grid(plotlist = a1_out_int_plots, 
                     # labels = names(a1_out_demo_plots),
                     # label_size = 10,
                     # label_x = -0.05,
                     # label_y = 0.05,
                     # hjust = -0.8,
                     # vjust = 0.8,
                     # scale = 0.85,
                     ncol = 1)

a1_plots <- 
  append(a1_out_demo_plots, a1_out_int_plots)

# a1_plots$T1_5_age_band_gender
# a1_plots$T1_6_age_band_imd_decile
# a1_plots$T1_7_gender_imd_decile

# aca %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()
# 
# a1 %>% 
#   select(person_ref, age) %>% 
#   distinct() %>% 
#   skimr::skim_without_charts()

a1 %>% count(person_ref)
a1_plots$T1_5_age_band_gender
a1_plots$T1_7_gender_imd_decile

p1a <- 
  a1 %>% 
  filter(gender != "Not Specified") %>% 
  ggplot(aes(x = age_band, fill = gender)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = 6) +
  scale_y_continuous(labels = scales::percent) +
  xlab(label = "age band") +
  ylab(label = "proportion of carers") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank()
        )
p1a


p1b <- 
  a1 %>% 
  filter(gender != "Not Specified") %>% 
  ggplot(aes(x = imd_decile, fill = gender)) +
  geom_bar(position = "fill", alpha = 0.5) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::percent) +
  xlab(label = "IMD decile") +
  ylab(label = "proportion of carers") +
  theme_minimal() +
  theme(legend.position = "none", 
        legend.title = element_blank()
  )

a1_fig <- 
  cowplot::plot_grid(plotlist = list(p1a, p1b), 
                     ncol = 1)


# a1 misc -----------------------------------------------------------------
## time from first carer flag
a1 %>% count(person_ref, sort = T)
a1 %>%
  summarise(
    mean.dt = mean(time.since.first.flag, na.rm = T),
    sd.dt = sd(time.since.first.flag, na.rm = T)
  )
1058/365
598/365
# 
# a1 %>% 
#   ggplot(aes(x = time.since.first.flag)) +
#   geom_histogram(binwidth = 93, color = "white", fill = "red") +
#   # facet_wrap(~gender)
#   # facet_wrap(~age_band, scales = "free_y")
#   facet_wrap(~imd_decile)
# 
# # covid impact?
# a1_plots$T1_2_age_band
# a1_plots$T1_5_age_band_gender
# 
# # number of flags
# a1 %>% 
#   ggplot(aes(x = number.of.carer.flags)) +
#   geom_histogram(binwidth = 1, color = "white", fill = "red") +
#   # facet_wrap(~gender)
#   facet_wrap(~age_band, scales = "free_y")
#   # facet_wrap(~imd_decile)

# check services not YET delivered 
# remove unknown ids / dates

a1_out$T1_1_overall
a1 %>% 
  filter(event == "referral") %>% 
  pull(person_ref) %>% 
  n_distinct(.)

glimpse(aca)
aca1 <- aca %>% 
  filter(referral_contact_date >= ymd("2016-01-01") & 
           referral_contact_date <= ymd("2021-12-31") &
           (age >= 18 | is.na(age))
  )

n_distinct(aca1$person_ref)
n_distinct(aca1$referral_id)-500

aca1 %>% count(service_desc, sort = T)
1350/650000*100

glimpse(aca1)

aca1 %>% 
  distinct(.data = ., person_ref, age) %>% 
  skimr::skim_without_charts()

glimpse(aca)
aca %>% 
  group_by(imd_decile) %>% 
  summarise(n = n_distinct(person_ref, na.rm = T)) %>% 
  arrange(desc(n))

a1 %>% 
  group_by(gender, age_band, imd_decile) %>% 
  summarise(
    n = n_distinct(person_ref, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct = n/sum(n)*100
    ) %>% 
  View()
  
a1_out$T1_1_overall
a1_out$T1_2_age_band
a1_plots$T1_2_age_band
a1_out$T1_3_gender
a1 %>% skimr::skim_without_charts()
290/1361
range(aca1$age, na.rm = T)
a1_out$T1_4_imd_decile
a2_out$T2_1_overall

a1_out$T1_5_age_band_gender
a1_plots$T1_5_age_band_gender
a1_plots$T1_4_imd_decile
a1_plots$T1_6_age_band_imd_decile

pop %>% 
  group_by(gender) %>% 
  summarise(n = sum(n, na.rm = T)/6) %>% 
  mutate(pct = n/sum(n)*100)

pop %>% 
  group_by(imd_decile) %>% 
  summarise(n = sum(n, na.rm = T)/6) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  pull(n) %>% 
  sum(.)

pop %>% 
  ggplot(aes(x = year, y = n, fill = imd_decile)) +
  geom_col(position = "dodge")

pop %>% 
  group_by(year, age_band, imd_decile) %>% 
  summarise(tot = sum(n)) %>% 
  ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
  geom_col(position = "dodge") +
  facet_wrap(~year)

p1d <- 
  pop %>% 
  group_by(age_band, imd_decile) %>% 
  summarise(tot = sum(n)/6) %>%
  filter(imd_decile %in% c("1", "7")) %>% 
  ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
  geom_col(position = "fill")

a1_out$T1_5_age_band_gender %>% 
  select(age_band:number.carers) %>% 
  filter(gender != "Not Specified") %>% 
  pivot_wider(data = ., names_from = "gender", values_from = "number.carers") %>% 
  mutate(prop = Female/(Female+Male)*100)

a5_out$T5_1_overall %>% 
  pull(number.carers) %>% 
  sum(.)/6

a1_out$T1_6_age_band_imd_decile %>% 
  arrange(imd_decile) %>% 
  View()
a1_plots$T1_6_age_band_imd_decile
a1_plots$T1_7_gender_imd_decile
a1_out$T1_7_gender_imd_decile %>% 
  filter(gender != "Not Specified" & imd_decile %in% c(1,7)) %>% 
  select(gender:number.carers) %>% 
  pivot_wider(data = ., names_from = "imd_decile", values_from = "number.carers") %>% 
  mutate(prop = `1`/(`7`+`1`))

a1 %>% 
  summarise(
    mean.tsff = mean(time.since.first.flag),
    sd.tsff = sd(time.since.first.flag)
  )
1053/365
600/365

# Age vs high IMD / Mid IMD ratio 
a1_plots$T1_6_age_band_imd_decile

p1c <- 
  a1_out$T1_6_age_band_imd_decile %>% 
  filter(imd_decile != "Unknown") %>% 
  mutate(imd_quintile = fct_collapse(.f = imd_decile,
                                     "1" = c("1", "2"),
                                     "2" = c("3", "4"),
                                     "3" = c("5", "6"),
                                     "4" = c("7", "8"),
                                     "5" = c("9", "10")
                                     )
  ) %>% 
  # filter(imd_quintile %in% c("1","4")) %>%
  filter(imd_decile %in% c("1","7")) %>%
  # ggplot(aes(x = age_band, y = std.number, fill = imd_quintile)) +
  ggplot(aes(x = age_band, y = number.carers, fill = imd_decile)) +
  geom_col(position = "fill", alpha = 0.5) +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "IMD decile")) +
  xlab("age band") +
  ylab("proportion of carers") +
  # facet_wrap(~names) +
  theme_minimal() +
  theme(legend.position = "top")

p1d <- 
  pop %>% 
  group_by(age_band, imd_decile) %>% 
  summarise(tot = sum(n)/6) %>%
  filter(imd_decile %in% c("1", "7")) %>% 
  ggplot(aes(x = age_band, y = tot, fill = imd_decile)) +
  geom_col(position = "fill") +
  scale_fill_viridis_d(option = "D", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "IMD decile")) +
  xlab("age band") +
  ylab("proportionof population") +
  # facet_wrap(~names) +
  theme_minimal() +
  theme(legend.position = "top")



p1a
p1b
p1c
p1d

ls(pattern = "^p")

p1_fig <- 
  cowplot::plot_grid(plotlist = list(p1c, p1d), 
                     ncol = 2)

p1_fig_a <- p1a

p1_fig_b <-
  ggpubr::ggarrange(plotlist = list(p1c, p1d), 
                    common.legend = T
                    )


# Gender vs high IMD / Mid IMD ratio 
A1_plots$T1_7_gender_imd_decile
a1_out$T1_7_gender_imd_decile %>% 
  filter(imd_decile != "Unknown") %>% 
  mutate(imdq = fct_collapse(.f = imd_decile,
                             "1" = c("1", "2"),
                             "2" = c("3", "4"),
                             "3" = c("5", "6"),
                             "4" = c("7", "8"),
                             "5" = c("9", "10")
  )
  ) %>% 
  # filter(imdq %in% c("1","4")) %>%
  filter(imd_decile %in% c("1","10")) %>% 
  # ggplot(aes(x = gender, y = number.carers, fill = fct_rev(imdq))) +
  ggplot(aes(x = gender, y = number.carers, fill = fct_rev(imd_decile))) +
  geom_col(position = "fill") +
  # facet_wrap(~names) +
  theme(legend.position = "top")

a5_plots$T5_2_age_band
a5_out$T5_2_age_band

a1_out$T1_1_overall
1361/622665*100
1350/65000*100
a2_out$T2_1_overall

533/65000*100
533/622665*100


replace()
replace_na()

a1_out$T1_2_age_band %>% 
  replace_na(., list(total.population = 0, 
                     std.number = 0,
                     lcl = 0,
                     ucl = 0,
                     std.na = 0
                     )
             )

a2_out$T2_6_age_band_imd_decile %>% 
  mutate(
    number.carers = if_else(number.carers < 5, as.integer(0), number.carers)
    ) %>% 
  select(-c(unknown.carers:std.na))

list(
  a1_out,
  a2_out,
  a4_out,
  a5_out
) %>% 
  # map_depth(.x = ., .depth = 2, .f = ~names(.x))
  map_depth(.x = ., .depth = 2, 
            .f = ~str_subset(string = names(.x), pattern = "number")
            )

a1_out %>% 
  map(.x = ., .f = ~select(.data = .x, -unknown.carers, - ))
a1_out$T1_1_overall
a5_out$T5_7_gender_imd_decile
a5_out$T5_5_age_band_gender

97.5/7.5



