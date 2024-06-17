# carers analysis combined

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

config <- get(file = "config.yml")

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

# save(list = ls(), file = "data/carerAnalysis.RData")
load(file = "data/carerAnalysis.RData")

# etl ----------------------------------------------------------------
dir(path = "data/", pattern = "out", full.names = T)

dat <- 
  tibble(
  files = dir(path = "data/", pattern = "out", full.names = T),
  nms = 
    str_remove(string = files, pattern = "data/") %>% 
    str_remove(string = ., pattern = ".rds") 
  ) %>% 
  mutate(
    data = map(.x = files, .f = ~readRDS(file = .x))
  )
  
df <- 
  dat %>% 
  pull(data) %>% 
  set_names(x = ., nm = dat$nms)
glimpse(df)


a1_out_gp <- df$a1_out_gp
a1_out_la <- df$a1_out_la
# etc


# a1 ----------------------------------------------------------------------
a1_out_la$T1_1_overall %>% 
  mutate(cohort = "la")
a1_out_gp$T1_1_overall_gp %>% 
  select(overall, number.carers) %>% 
  mutate(cohort = "gp")

bind_rows(
  a1_out_la$T1_1_overall %>% 
    mutate(cohort = "la"),
  a1_out_gp$T1_1_overall_gp %>% 
    select(overall, number.carers) %>% 
    mutate(cohort = "gp")
)

bind_rows(
  a1_out_la$T1_5_age_band_gender %>% 
    mutate(cohort = "la"),
  a1_out_gp$T1_5_age_band_gender_gp %>% 
    select(-c(unknown.carers:std.na)) %>% 
    mutate(cohort = "gp")
)

map(.x = a1_out_la, .f = ~dim(.x))
map(.x = a1_out_gp, .f = ~dim(.x))

map(.x = a1_out_la, .f = ~names(.x))
map(.x = a1_out_gp, .f = ~names(.x))

a1_comb_gp <- 
  map(.x = a1_out_gp, 
    .f = ~select(.data = .x, -c(unknown.carers:std.na)) %>% 
      mutate(cohort = "GP")
)

a1_comb_la <- 
  map(.x = a1_out_la, 
      .f = ~mutate(.data = .x, cohort = "LA")
  )

a1_comb_gp$T1_1_overall_gp
a1_comb_la$T1_5_age_band_gender

a1_comb_gp$T1_5_age_band_gender_gp
a1_comb_la$T1_5_age_band_gender

map(.x = a1_comb_la, .f = ~dim(.x))
map(.x = a1_comb_gp, .f = ~dim(.x))

a1_comb <- 
  map2(.x = a1_comb_la, .y = a1_comb_gp, .f = ~bind_rows(.x, .y))

# check
a1_comb$T1_1_overall
a1_comb$T1_3_gender
a1_comb$T1_5_age_band_gender %>% 
  group_by(cohort) %>% 
  summarise(tot = sum(number.carers))

# a2 ----------------------------------------------------------------------
a2_out$T2_1_overall %>% 
  summarise(tot = sum(number.carers))
a2_out_gp$T2_1_overall_gp %>% 
  summarise(tot = sum(number.carers))

a2_out_la$T2_1_overall %>% 
  select(-c(unknown.carers:std.na)) %>% 
  mutate(cohort = "la")
a2_out_gp$T2_1_overall_gp %>% 
  select(-c(unknown.carers:std.na)) %>% 
  mutate(cohort = "gp")

bind_rows(
  a2_out_la$T2_1_overall %>% 
    select(-c(unknown.carers:std.na)) %>% 
    mutate(cohort = "la"),
  a2_out_gp$T2_1_overall_gp %>% 
    select(-c(unknown.carers:std.na)) %>% 
    mutate(cohort = "gp")
)

bind_rows(
  a2_out_la$T2_5_age_band_gender %>% 
    select(-c(unknown.carers:std.na)) %>% 
    mutate(cohort = "la"),
  a2_out_gp$T2_5_age_band_gender_gp %>% 
    select(-c(unknown.carers:std.na)) %>% 
    mutate(cohort = "gp")
)

a2_comb_gp <- 
  map(.x = a2_out_gp, 
      .f = ~select(.data = .x, -c(unknown.carers:std.na)) %>% 
        mutate(cohort = "GP")
  )

a2_comb_la <- 
  map(.x = a2_out_la, 
      .f = ~select(.data = .x, -c(unknown.carers:std.na)) %>% 
        mutate(cohort = "LA")
  )

# check
# a2_comb_gp$T2_1_overall_gp
# a2_comb_la$T2_1_overall
# a2_comb_gp$T2_5_age_band_gender_gp
# a2_comb_la$T2_5_age_band_gender

# map(.x = a2_comb_la, .f = ~dim(.x))
# map(.x = a2_comb_gp, .f = ~dim(.x))

a2_comb <- 
  map2(.x = a2_comb_la, .y = a2_comb_gp, .f = ~bind_rows(.x, .y))

# check
# a2_comb$T2_1_overall
# a2_comb$T2_3_gender
# a2_comb$T2_5_age_band_gender %>% 
#   group_by(cohort) %>% 
#   summarise(tot = sum(number.carers))


# a3 ----------------------------------------------------------------------
a3_out_la$T2_1_overall
a3_out_gp$T2_1_overall_gp

a3_out_la$T2_1_overall %>%
  select(-unknown.carers, -std.na) %>% 
  summarise(tot = sum(number.carers))
a3_out_gp$T2_1_overall_gp %>% 
  select(-unknown.carers, -std.na) %>% 
  summarise(tot = sum(number.carers))

a3_out_la$T2_1_overall %>% 
  select(-unknown.carers, -std.na) %>% 
  mutate(cohort = "la")
a3_out_gp$T2_1_overall_gp %>% 
  select(-unknown.carers, -std.na) %>% 
  mutate(cohort = "gp")

bind_rows(
  a3_out_la$T2_1_overall %>% 
    select(-unknown.carers, -std.na) %>% 
    mutate(cohort = "la"),
  a3_out_gp$T2_1_overall_gp %>% 
    select(-unknown.carers, -std.na) %>% 
    mutate(cohort = "gp")
)

bind_rows(
  a3_out_la$T2_5_age_band_gender %>% 
    select(-unknown.carers, -std.na) %>% 
    mutate(cohort = "la"),
  a3_out_gp$T2_5_age_band_gender_gp %>% 
    select(-unknown.carers, -std.na) %>% 
    mutate(cohort = "gp")
) %>% 
  group_by(cohort) %>% 
  summarise(tot = sum(pcnt.carers))

a3_comb_gp <- 
  map(.x = a3_out_gp, 
      .f = ~select(.data = .x, -unknown.carers, -std.na) %>% 
        mutate(cohort = "GP")
  )

a3_comb_la <- 
  map(.x = a3_out_la, 
      .f = ~select(.data = .x, -unknown.carers, -std.na) %>% 
        mutate(cohort = "LA")
  )

# check
# a3_comb_gp$T2_1_overall_gp
# a3_comb_la$T2_1_overall
# a3_comb_gp$T2_5_age_band_gender_gp
# a3_comb_la$T2_5_age_band_gender
# 
# map(.x = a3_comb_la, .f = ~dim(.x))
# map(.x = a3_comb_gp, .f = ~dim(.x))

a3_comb <- 
  map2(.x = a3_comb_la, .y = a3_comb_gp, .f = ~bind_rows(.x, .y))

# check
# a3_comb$T2_1_overall
# a3_comb$T2_3_gender
# a3_comb$T2_5_age_band_gender %>% 
#   group_by(cohort) %>% 
#   summarise(tot = sum(number.carers))
# 
# a3_comb$T2_5_age_band_gender %>% 
#   group_by(cohort) %>% 
#   summarise(tot = sum(pcnt.carers))

# plots -------------------------------------------------------------------


## a1 ----------------------------------------------------------------------
# demo
a1_comb$T1_2_age_band %>%
  group_by(cohort) %>% 
  mutate(pct.carers = number.carers/sum(number.carers)) %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[["cohort"]])) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  # labs(x = "Age-Band", y = "% of cohort") +
  theme_minimal() +
  theme(legend.position = "right")
  # facet_wrap(~.data[["cohort"]], scales = "free") +

a1_pcomb <- a1_comb[1:4] %>%
  map(.x = .,
      .f = ~group_by(.data = .x, cohort) %>% 
        mutate(pct.carers = number.carers/sum(number.carers)) %>% 
        ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[["cohort"]])) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(legend.position = "right")
  )

# a1_pcomb$T1_1_overall

# int
a1_comb$T1_5_age_band_gender %>% 
  group_by(cohort) %>% 
  mutate(pct.carers = number.carers/sum(number.carers)) %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[[names(.)[2]]])) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  # labs(x = "Age-Band", y = "% of cohort") +
  facet_wrap(~cohort, scales = "free") +
  theme_minimal() +
  theme(legend.position = "right")

a1_pcomb_int <- a1_comb[5:7] %>%
  map(.x = .,
      .f = ~group_by(.data = .x, cohort) %>% 
        mutate(pct.carers = number.carers/sum(number.carers)) %>% 
        ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[[names(.)[2]]])) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::percent) +
        facet_wrap(~cohort, scales = "free") +
        theme_minimal() +
        theme(legend.position = "right")
  )

# check
# a1_pcomb$T1_1_overall
# a1_pcomb$T1_2_age_band
# a1_pcomb$T1_3_gender
# a1_pcomb$T1_4_imd_decile
# 
# a1_pcomb_int$T1_5_age_band_gender
# a1_pcomb_int$T1_6_age_band_imd_decile
# a1_pcomb_int$T1_7_gender_imd_decile


p1_gender_age_band <-
  a1_comb$T1_5_age_band_gender %>%
  filter(gender %in% c("Female", "Male") & age_band != "Unknown") %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], 
                       y = number.carers, 
                       fill = .data[[names(.)[2]]])) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age-Band", y = "Cohort [%]") +
  facet_wrap(~.data[["cohort"]], scales = "free") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 0)
  )

ggsave(filename = "../local_files/p1_gender_age_band.png", 
       plot = p1_gender_age_band, 
       device = "png",
       width = 8,
       height = 6)


# a1_comb$T1_1_overall
# a1_comb$T1_2_age_band %>% 
#   filter(age_band == "Unknown") %>% 
#   pivot_wider()

a2_comb$T2_1_overall %>%
  select(cohort, year, number.carers) %>% 
  openxlsx::write.xlsx(x = ., file = "../local_files/t2_overall.xlsx", asTable = TRUE)

# unknown profiles
# a3_out$T2_1_overall
# a3_comb$T2_3_gender %>% 
#   filter(cohort == "GP") %>% 
#   mutate(pct = std.number*100)
# 
# 0.6/100
# 0.006*622000*6
# 0.0004*622000*6

## a2 ----------------------------------------------------------------------
a2_comb$T2_5_age_band_gender %>% 
  group_by(cohort) %>% 
  mutate(pct.carers = number.carers/sum(number.carers)) %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[[names(.)[2]]])) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  # labs(x = "Age-Band", y = "% of cohort") +
  # facet_wrap(~cohort, scales = "free") +
  facet_grid(cohort~year, scales = "free") +
  theme_minimal() +
  theme(legend.position = "right")


a2_comb$T2_1_overall %>%
  group_by(cohort) %>% 
  mutate(pct.carers = number.carers/sum(number.carers)) %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], y = pct.carers, fill = .data[["cohort"]])) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  # labs(x = "Age-Band", y = "% of cohort") +
  theme_minimal() +
  theme(legend.position = "right")
# facet_wrap(~.data[["cohort"]], scales = "free") +


## a3 ----------------------------------------------------------------------
p3_tot <-
  a3_comb$T2_1_overall %>%
  ggplot(data = ., aes(x = .data[[names(.)[1]]], 
                       y = std.number, 
                       fill = .data[["cohort"]])) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Population [%]") +
  facet_wrap(~.data[["cohort"]], scales = "free", ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
        )

p3_tot <-
  a3_comb$T2_1_overall %>%
  ggplot(data = ., aes(x = .data[[names(.)[1]]], 
                       y = std.number 
                       # color = .data[["cohort"]], 
                       # group = .data[["cohort"]]
                       )
         ) +
  geom_col(position = "dodge", fill = "#00BA38") +
  # geom_line(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Population [%]") +
  facet_wrap(~.data[["cohort"]], scales = "free", ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
  )


ggsave(filename = "../local_files/p3_tot.png", 
       plot = p3_tot, 
       device = "png",
       width = 6,
       height = 4)
# 1361/65000*100


p3_gender <- 
  a3_comb$T2_3_gender %>%
  filter(gender %in% c("Female", "Male")) %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], 
                       y = std.number, 
                       fill = .data[[names(.)[2]]])
         ) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Population [%]") +
  facet_wrap(~.data[["cohort"]], scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
  )

ggsave(filename = "../local_files/p3_gender.png", 
       plot = p3_gender, 
       device = "png",
       width = 6,
       height = 4)


## a4 ----------------------------------------------------------------------
a4_out_la$T4_3_gender_paths



## a5 ----------------------------------------------------------------------
a5_out_la$T5_3_gender

p5_support <-
  a5 %>% 
  filter(gender %in% c("Male", "Female")) %>%
  # group_by(gender, age_band, support_provision, .drop = F) %>% 
  group_by(gender, imd_decile, support_provision, .drop = T) %>%
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
  # guides(fill = guide_legend(title = "% support")) +  
  scale_fill_gradient2(low = "#6baed6", 
                       mid = "#2171b5",
                       high = "#08306b", 
                       midpoint = 50, 
                       limits = c(0,100) 
                       ) +  
  labs(y = "Gender", x = "IMD decile") +  
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
  )

ggsave(filename = "../local_files/p5_support.png", 
       plot = p5_support, 
       device = "png",
       width = 6,
       height = 4)


t5_support <- 
  a5_out$T5_2_age_band %>% 
  select(-pct.carers) %>% 
  pivot_wider(data = ., names_from = support_provision, values_from = number.carers) %>% 
  mutate(`% received` = received/(`not received` + received)*100) %>%
  relocate(received, .after = age_band) %>%
  rename(`Age Band` = age_band)
  
openxlsx::write.xlsx(x = t5_support, file = "../local_files/t5_support.xlsx", asTable = T)  

p5_imd_decile <- 
  a5_out_la$T5_4_imd_decile %>% 
  ggplot(data = ., aes(x = .data[[names(.)[1]]], 
                       y = pct.carers, 
                       fill = .data[["support_provision"]])
         ) +
  geom_col(position = "stack") +
  # scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer() +
  labs(x = "IMD decile", y = "% of cohort") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
  )
# facet_wrap(~.data[["cohort"]], scales = "free") +

ggsave(filename = "../local_files/p5_imd_decile.png", 
       plot = p5_imd_decile, 
       device = "png",
       width = 6,
       height = 2)


# pres --------------------------------------------------------------------
# a3_comb$T2_1_overall
# a3_comb$T2_3_gender

a3_comb$T2_7_gender_imd_decile %>% 
  ggplot(data = ., aes(x = .data[[names(.)[3]]], 
                       y = std.number, 
                       fill = .data[["gender"]])
  ) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  # scale_fill_brewer() +
  labs(x = "IMD decile", y = "% of cohort") +
  facet_wrap(~.data[["cohort"]], scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11)
  )


# end ---------------------------------------------------------------------





