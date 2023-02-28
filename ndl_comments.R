# ndl comments


# general comments --------------------------------------------------------

## figs --------------------------------------------------------------------
# labelled figs / save as .csv

# from markdown objects
total_plot$data %>% 
  write.csv(x = ., file = "gp_figure1_a.csv")

sex_plot$data

age_band_plot$data

imd_plot$data

language_plot$data

language_age_band$data

oddsPlot$data

plot$data

# ignore for now
# deprivationTotalPlot %>% class
# deprivationTotalPlotCovid %>% class

nelPlot$data

ip_plot$data

ae_plot$data



# end ---------------------------------------------------------------------



# rdata obj --------------------------------------------------------------
rm(list = ls())
dir("data", pattern = ".RData")

load(file = "data/languageAgeBandPlots.RData")
load(file = "data/languageAgeBandYearPlots.RData")
load(file = "data/compPlots.RData")
load(file = "data/hospitalAdmissionPlots.RData")
load(file = "data/nelPlots.RData")
load(file = "data/oddsPlot.RData")
load(file = "data/compPlots.RData")

rm(list = ls())

language_age_band
comp_plot[[1]]
comp_plot[[1]]$data
comp_plot[[2]]
comp_plot[[2]]$data


## rds to csv -----------------------------------------------------------------
gp_tbls <- 
  dir(path = "data/", pattern = ".rds") %>% 
  str_subset(string = ., pattern = "CarerRegistrations") %>% 
  paste("data/", ., sep = "")

gp_dat <-
  tibble(
    file = gp_tbls[c(5,1,6,3,2,4)],
    names = c("gp_1_overall", "gp_2_age_band", "gp_3_sex", "gp_4_imd", 
              "gp_5_age_band_imd", "gp_6_lsoa"),
    dat = map(.x = file, .f = ~read_rds(file = .x))
)

gp_dat %>% 
  map(.x = .$dat, .f = ~names(.x))

nms <- gp_dat$names
gp_out <- 
  gp_dat %>% 
  pull(dat) %>% 
  set_names(x = ., nm = nms)

# paste("data/", names(gp_out)[1], "_yearly.csv", sep = "")
gp_out %>% 
  iwalk(.x = ., 
        .f = ~write.csv(.x, file = paste("data/", .y, "_yearly.csv", sep = ""))
        )


# source data + misc tables
gp_tbls2 <- 
  dir(path = "data/", pattern = ".rds") %>% 
  str_subset(string = ., pattern = "CarerRegistrations", negate = T) %>% 
  paste("data/", ., sep = "")

gp_dat2 <-
  tibble(
    file = gp_tbls2,
    names = c("carerInfo", "carerLanguage", "carerPeriod"),
    dat = map(.x = file, .f = ~read_rds(file = .x))
  )

gp_dat2$dat[[1]] %>% 
  count(nhs_number, year(gp_date), sort = T)

glimpse(gp_dat2$dat[[1]])

gp_dat2$dat[[1]] %>% 
  count(nhs_number, year(gp_date), sort = T) %>% 
  count(n)

gp_dat2$dat[[1]] %>% 
  filter(nhs_number == "269349308319318308299328325307") %>% 
  View()




# CA1 ---------------------------------------------------------------------
# load
# rename cols
# apply col hearders
# label as gp




