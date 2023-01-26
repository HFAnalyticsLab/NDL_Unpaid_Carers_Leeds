library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(ggvis)
library(plotly)
library(metathis)
# library(formattable)
library(lubridate)
if (!'scrollytell' %in% installed.packages()) devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)
library(shinycssloaders)
library(here)
# library(leaflet)
# library(sf)
# library(geographr)

options(scipen = 999)
options(warn = -1)

theme_set(theme_minimal())

data <- readRDS(here('data/new_carers3.RDS')) %>%
  filter(age_band < 70, year(gp_date) < 2020) %>%
  group_by(gp_date, age_band) %>%
  mutate(
    n = sum(n)
  ) %>%
  group_by(age_band) %>%
  mutate(n_max = if_else(n_max == 0, n_max, max(n))) %>%
  ungroup() %>%
  mutate(
    sex = 'MF',
    reveal = group_indices(., year(gp_date))
  )

covid_data <- readRDS(here('data/new_carers3.RDS')) %>%
  filter(year(gp_date) >= 2020) %>%
  group_by(gp_date, age_band) %>%
  mutate(
    n = sum(n)
  ) %>%
  group_by(age_band) %>%
  mutate(n_max = if_else(n_max == 0, n_max, max(n))) %>%
  ungroup() %>%
  mutate(
    sex = 'MF',
    reveal = group_indices(., year(gp_date))
  )

carer_info <- readRDS(here('data/carer_info.rds')) %>% 
  select(
    carer, 
    sex, 
    age_band, 
    date, 
    n, 
    signal, 
    n_min, 
    n_max, 
    combined_score, 
    deprivation_decile
  )

carer_registrations <- bind_rows(
  readRDS(here('data/overallCarerRegistrations.rds')) %>%
    mutate(group = 'total'),
  readRDS(here('data/sexCarerRegistrations.rds')) %>%
    mutate(group = 'sex'),
  readRDS(here('data/ageBandCarerRegistrations.rds')) %>%
    mutate(group = 'age_band'),
  readRDS(here('data/imdCarerRegistrations.rds')) %>%
    mutate(group = 'imd'),
  readRDS(here('data/ageBandImdCarerRegistrations.rds')) %>%
    mutate(group = 'age_band_imd')
) %>%
  ungroup() %>%
  mutate(
    grouping = coalesce(
      as.character(constant), 
      sex, 
      as.character(age_band), 
      as.character(imd_decile)
    )
  ) %>%
  filter(!is.na(grouping)) %>%
  mutate(grouping = if_else((!is.na(constant)) & (grouping == '1'), 'total', grouping))

carer_lsoa <- readRDS(here('data/lsoaCarerRegistrations.rds')) %>%
  mutate(group = 'lsoa')

carer_language <- readRDS(here('data/carerLanguage.rds'))

carer_period <- readRDS(here('data/carerPeriod.rds'))

load(here('data/languageAgeBandPlots.RData'))

### FUNCTIONS & TEXT

stop_propogation <- FALSE

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(num){
  div(
    text(num), class = "text"
  )
}

null_na <- function(x) {
  if_else(is.null(x), NA_character_, x)
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
           text7,
           text8,
           text9,
           text10,
           text11,
           text12,
           text13,
           text14,
           text15,
           text16,
           text17,
           text18,
           text19,
           text20,
           text21,
           text22,
           text23,
           text24,
           text25,
           text26,
           text27,
           text28,
           text29,
           text30,
           text31
    )
  )
}

neaten_plotly_labels <- function(plotly_obj, limit_type = NULL) {
  df <- data.frame(
    id = seq_along(plotly_obj$x$data), 
    legend_entries = unlist(lapply(plotly_obj$x$data, `[[`, "name"))
  )
  df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
  df$is_first <- !duplicated(df$legend_group)
  
  l <- F
  if (!is.null(limit_type)) {
    as_limit_type <- get(paste0('as.', limit_type))
    l <- T
  }
  
  for (i in df$id) {
    is_first <- df$is_first[[i]]
    plotly_obj$x$data[[i]]$name <- df$legend_group[[i]]
    plotly_obj$x$data[[i]]$legendgroup <- plotly_obj$x$data[[i]]$name
    if (!is_first) plotly_obj$x$data[[i]]$showlegend <- FALSE
    if (l) {
      if (is.na(as_limit_type(df$legend_group[[i]]))) plotly_obj$x$data[[i]]$showlegend <- FALSE
    }
  }
  
  plotly_obj
}

intro_text <- HTML(
  "<span style='font-size:20px'> How much to we know about unpaid carers in 
  Leeds? </span>
  <br>
  <br> 
  <p>The Parliamentary Family Resources Survey (2021) estimated that in 2019/20 
  around 7% of the UK population were providing unpaid care, although their 
  definition is 'informal care'. Similarly, the Carer's Week Research Report 
  (2020) estimated that around 1 in 4 people in the UK could be providing some 
  level of unpaid care, with around 74,000 estimated unpaid carers in the Leeds 
  City area (2011 Census/Carers Leeds, 2020).
  <br>
  <br>
  The needs of unpaid carers are known to be complex and varied, with surveys 
  finding that around 83% of respondents reported a negative impact of caring on
  their physical health, and 87% responding negative impacts on their mental 
  health (In Sickness and in Health, 2012). With so many people providing care 
  across Leeds and such high levels of reported needs nationally, it is 
  important to investigate the full health and social-care needs across Leeds, 
  and identify potential gaps and inequalities in service use.
  <br>
  <br>
  The Leeds NDL team has conducted and commissioned research to establish gaps 
  in the knowledge of both the Leeds population, the ICB in Leeds/Leeds City 
  Council teams regarding the needs of unpaid carers, and the utilisation of 
  services by unpaid carers. A Task and Finish group comprised of professionals 
  (council and third sector) who work with and on behalf of unpaid carers, and 
  unpaid carers from different economic and ethnic backgrounds was set up to 
  find the commonest problems encountered by carers, defining the questions 
  which we investigated.
  <br>
  <br>
  <span style='font-size:15px'> <b>Available Data</b> </span>
  <br>
  <br>
  Our primary sources of data used to identify carers for this investigation 
  were:
  <ul>
    <li><b>Primary Care records</b>, coming from GP appointments or 
    registrations, where a clinician or Practice employee noted that a patient 
    was an unpaid carer,</li> 
    <li><b>Adult Social Care records (from CIS)</b>, marking contacts between 
    people and the council, as well as Carer's Assessments (singular and joint),
    sign-posting to third sector carer's services, and direct packages of 
    aid for both carers and cared-for people.</li>
  </ul>
  <br>
  As well as this, we also had access to <b>Secondary Care records</b> 
  (inpatient, outpatient, A&E) and <b>Mental Health appointments</b>. All of the
  above data sets were linkable at a person-level to some extent via 
  pseudonymised patient identifiers.
  </p>"
)

section1_text <- HTML(
  "<span style='font-size:20px'>
    So what demographic variation do we have in our GP carers data?
  </span>
  <br>
  <br> 
  <p>As mentioned above, in the 2011 census there were around 74,000 people 
  stating that they provided some level of unpaid care for another. If we look
  at data from the past five years, we find around 19,000 patients who have 
  informed their GP of their caring status: <font color='#E8B529'>around 25%
  of the expected number</font>. If we limit our time-window further, and only
  look for carer registrations within one year then we find between 6000-10,000
  patients looking back for each year from 2016-2021: <font color='#EE3E12'>
  an average of around 10% of the expected count</font>.
  <br>
  <br>
  Based on interactions with GP practices, common population health findings, 
  and anecdotal feedback from the Task and Finish group, we would not expect 
  this underrepresentation to be found equally across all demographic groups
  across the city, and so we compared our GP carer data with ONS mid-year 
  population estimates to highlight the specific groups which feature fewer 
  registered carers.
  <br>
  <br>
  In the figures below, please click on the legend items below the y-axis to 
  filter out the option. To highlight one legend option, please double-click the
  item.
  </p>")

section1_conclusion <- HTML(
  "<p>
  Overall, by comparing the demographics of our (Leeds resident) patients to 
  ONS estimates we can see that there are clear variations in carer 
  registrations across different demographic groups. Registered carers are more
  likely to be women than men, and are more likely to be older - with 
  significant weighting towards retirement ages. While there is some variation
  over time, with the gaps closing slightly, by the end of 2021 these were still
  present. Interestingly, while there are some differences by deprivation, these
  were smaller than assumed, and may have been more due to age differences (with
  younger residents tending to be both less likely to register as a carer, and 
  more likely to live in a more deprived areas).
  <br>
  <br>
  To compare ethnicity and language differences we relied purely upon GP data
  due to the lack of yearly ONS estimate data. We saw that patients who don't 
  speak English as a second language were much less likely to register as carers
  than those who speak English as a first language - and when accounting for 
  language differences across age-bands this finding holds at the younger ages.
  When comparing different ethnic groups across Leeds we found that African 
  (and Black British) and Chinese (and Chinese British) patients were 
  <i>less</i> likely to register as carers, while Indian/British Indian and 
  Pakistani/British Pakistani patients were <i>more</i> likely to register as 
  carers. Interestingly, all ethnic groups tended to be on-average younger
  than White British patients (especially Chinese patients, who tended to be 
  primarily University-age), which may explain and under-representation, but
  would not explain why there were significantly more Indian/Pakistani patients
  registered as carers. Further investigation could look into this to find if 
  there were specific successes in campaigns that could be applied to other 
  areas of the city. 
  </p>")

text0 <- HTML("")

text1 <- HTML("<H2> Overall Population Proportion </H2>
              <br> <p> As mentioned above, the population proportion of carers
              falls well short of the expected figure from the census results.
              Prior to 2020 we can see a slight increase in the proportion of 
              registered patients providing care, rising from around
              <font color='#EE3E12'>1.08% in 2016</font> to 
              <font color='#00CCCC'>1.70% in 2019</font>.
              <br>
              <br>
              We see another large increase in <font color='#2B61DF'>2020
              (to 2.2%)</font>, before dropping to around
              <font color='#FF00FF'>1.45% in 2021</font>.
              <br> 
              <br></p>")

text2 <- HTML("<H2> Population Proportion by Sex </H2>
              <br> <p> In all years we see significantly more female carers than
              male carers, with roughly double the proportion of the female 
              population registering as carers than the male population.</p>")

text3 <- HTML("<H2> Population Proportion by Age Band </H2>
              <br> <p> From <font color='#EE3E12'>2016</font> to 
              <font color='#2B61DF'>2020</font> there is a roughly linear 
              increase in the proportion of carers with ten year age-band. 
              However, the population proportion difference is not static, with
              increasingly greater numbers of older carers registering than 
              younger carers. In <font color='#EE3E12'>2016, there were 
              approximately 9 times more elderly (80-89) carers than younger 
              (18-29) carers</font>, but <font color='#00CCCC'>by 2019 this difference had increased 
              to around 14 times</font>. In <font color='#2B61DF'>2020 this difference rose to nearly 19 
              times more elderly than younger carers</font>.
              <br>
              <br>
              However, this difference dropped off in 2021, with fewer elderly
              carers registering, but higher numbers of younger - middle-aged 
              carers.</p>")

text4 <- HTML("<H2> Population Proportion by Deprivation Decile </H2>
              <br><p> Interestingly, there is no consistent strong trend 
              observed with deprivation level - proportionally people who live 
              in the most deprived areas in Leeds are roughly as likely to tell 
              their GP that they are a carer as those who live in the least 
              deprived areas.
              <br>
              <br>
              Based upon the health-needs of the city, it is
              known that people who live in the most deprived areas generally
              feature poorer health outcomes (higher levels of chronic illness,
              lower life expectancy, and lower healthy life expectancy), and so
              it could be assumed that people from these areas would also be 
              more likely to provide care for a family member or friend.
              However, as described above, this is not seen in the GP carer
              registrations - potentially pointing towards a lack of 
              registration among areas of higher deprivation.</p>")

text5 <- HTML("<H2> Population Proportion by Main Language </H2>
              <br><p>Overall in Leeds around 85% of patients have had their 
              primary language recorded by their GP. For the total population
              the proportion of English speakers has dropped from around 77% in
              2016 to 73% in 2021. However, the proportion of carers who speak
              English as their first language is significantly higher at all 
              times: at 87% in 2016 and 85% in 2021.
              <br>
              <br>
              Similarly, the proportion of non-English first language patients
              is significantly lower amongst the carer cohort than the non-carer
              cohort (5% vs 7% in 2016), and the carer cohort experiences little
              overall change compared with the non-carer cohort (5% vs 11% in
              2021).</p>")

text6 <- HTML("<H2> Population Proportion by Main Language (Excluding Unknowns)
              </H2>
              <br>
              If we naively assume that GP practices don't record patient first 
              language when it is English (so count 'Unknown' and 
              'English-speaking' patients together as 'Assumed 
              English-speaking') then we can see that in 2021 around 
              9 in 10 non-carer registered patients probably spoke English as 
              their first language, but around 19 in 20 carer patients: roughly
              doubling the odds of a patient speaking English as their first 
              language.
              </p>")

text7 <- HTML("<H2> Population Proportion by Main Language (Age Banded)
              </H2>
              <br>
              It is likely that a significant portion of the apparent
              under-representation of non-English carer registrations arises as 
              a result of the difference in average age of non-English speakers,
              as across Leeds the older the group, the more likely the person is
              to be a primary English speaker. We know that most carer 
              registrations come from the older age groups, so we need to 
              account for patient age when computing rates of non-English 
              speaking groups.
              <br>
              <br>
              When we split by age-band this assumption is partially confirmed -
              at the higher age-bands we see roughly matching proportions of 
              non-English speaking patients for both carers and non-carers. 
              However, while the younger age-bands broadly matched 
              carer/non-carer proportions in 2016 we can see a divergance over 
              time, with proportionally fewer non-English speaking patients 
              registering as carers over time. At its greatest extend, we can
              see a large difference for 20-40 year olds across Leeds, with 
              far fewer non-English speaking carers than would be expected when
              accounting for age differences.
              </p>")

text8 <- HTML("<H2> Odds Ratios </H2>
              <br>
              To estimate the effect of each demographic variable on carer 
              registration rates, we ran a binomial GLM, predicting whether
              a patient would register as a carer based upon their age band, 
              sex, main language (assuming unknowns were English-speaking), 
              residential deprivation decile, and ethnicity group. From this 
              model, odds ratios were calculated. Note that these were based 
              upon GP data, so outcomes will have slight deviances when compared
              with ONS population estimate data.
              <br>
              <br>
              Of note, we can see that age is a consistantly signicant indicator
              of carer registration, with each year increasing a patient's odds
              of registering by around 3-4%. Similarly, we can see that as we 
              already found, men tended to have 40-50% reduced odds of 
              registering compared with women, although this has been slightly 
              decreasing since 2016. Deprivation has a significant, although 
              relatively smaller, effect, with each deprivation decile featuring
              around a 5% decrease in odds of registration, and as we have seen
              language in 2016 had little effect, although by 2021 this has 
              grown to around a 30-40% decrease in registration odds for 
              non-English first language patients. Finally looking at patient
              ethnicity registration rates (compared to 'White British' rates),
              we can see that, despite having generally younger 
              age-distributions, Indian and Pakistanti patients are around 5-20%
              more likely to register as carers than White British patients, 
              while Chinese and African patients are significantly less likely
              (50% and 40% respectively). In all significant cases, we can see
              an upwards trend - with odds of registering compared with White 
              British patients generally increasing over time, although the 
              specific rate of increase varies greately across different groups.
              </p>")

text9 <- HTML("<H2> Pre-COVID Carer Registrations </H2>
              <br>
              Next, we were interested in looking at seasonal variations in 
              carer registrations for four 'pre-COVID' years (2016-2019), and
              looking into the demographic differences of people who registered
              within those times. We were particularly interested in looking at
              pre-retirement age registrations at first, as these are likely to
              vary significantly when compared with retirement-age patients
              (70+).
              </p>")

text10 <- HTML("<H2> Pre-COVID Carer Registrations - 2016 </H2>
              <br>
              Splitting registrations by 10 year age-band, in 2016 we can see 
              that at the younger ages (<=40) there was an initial peak of 
              registrations at the beginning of the year, while at older ages
              (>=40) the peak came later in the year.
              </p>")

text11 <- HTML("<H2> Pre-COVID Carer Registrations - 2017 </H2>
              <br>
              Looking forward to 2017, we see the peak around October/November
              appears in all age-bands above 20, while the 20s age-band features
              mostly noise.
              </p>")

text12 <- HTML("<H2> Pre-COVID Carer Registrations - 2018 </H2>
              <br>
              In 2018 we can see a definite peak in all age-bands at 
              October/November, with the older ages (50+) exhibiting a much 
              greater spike than in previous years.
              </p>")

text13 <- HTML("<H2> Pre-COVID Carer Registrations - 2019 </H2>
              <br>
              And again, the same trend in 2019, with higher numbers of carer
              registration in 'peak' times.
              </p>")

text14 <- HTML("<H2> Pre-COVID Carer Registrations - Autumn Wave </H2>
              <br>
              Highlighting September-November each year we can clearly see waves
              of carer registrations in all age-bands except the 20-year old 
              band.
              </p>")

text15 <- HTML("<H2> Pre-COVID Carer Registrations - Autumn Wave </H2>
              <br>
              These periods correspond well with usual vaccine drives for flu 
              vaccines, which would usually not be available freely for 
              working-age adults. As part of our engagement with health and
              social care professionals, we discussed the benefits of 
              registering as a carer with a GP practice, and one of the key 
              messages was access to vaccines, such as for flu or COVID. Clearly
              from this it could be inferred that the vaccine drives had a good
              impact on carer registrations - either for encouraging carers to 
              register with their GP, or for encouraging GPs to discuss 
              patients' caring responsibilities in the lead up to vaccinations.
              <br>
              <br>
              However, while the base numbers of carers can be seen to increase 
              seasonally, we were also interested in looking at the groups which
              benefitted from this, to see if these vaccine drives were 
              effective at drawing people from different demographic backgrounds
              to register.
              <br>
              <br>
              To investigate further, we took each registration and added a flag
              if it occured within the marked (signal) times, shown here for 
              registrations from the 60-69 age group. Based upon this we looked
              particularly at the deprivation levels of the areas from which
              carers lived.
              </p>")

text16 <- HTML("<H2> Vaccine Drives - Registrations by Deprivation </H2>
              <br>
              Looking at the whole population, we took the combined IMD score 
              for each area and looked at the change in deprivation level for 
              patients who registered as carers during our signal times to those
              who registered outside these times. Here, a shift to higher scores
              means that a greater number of patients from more deprived areas
              register as carers during the signal periods, and a shift to lower
              scores means the inverse.
              <br>
              <br>
              We observed a statistically significant shift to lower deprivation
              levels, signifying that during our signal periods people from 
              less deprived areas were more likely to register as carers at 
              their GP practices than those from more deprived areas.
              </p>")

text17 <- HTML("<H2> Vaccine Drives - Registrations by Deprivation by Sex </H2>
              <br>
              This finding was consistent looking at both male and female 
              patients, although a slightly greater shift was found for female 
              registrations.
              </p>")

text18 <- HTML("<H2> Vaccine Drives - Registrations by Deprivation by Age Band
              </H2>
              <br>
              Similarly, this finding held across all age-bands. The largest 
              changes in registration level were seen at lower ages, with only 
              minor shifts in demographics occuring in the 60-69 age band. 
              </p>")

text19 <- HTML("<H2> COVID Carer Registrations </H2>
              <br>
              Moving to 2020 and extending out our analysis to cover all adult
              age-bands, we repeated the above method to compare peak times of
              interest to 'usual' background registrations levels. However, as 
              COVID occured in 2020 and the COVID vaccination started at the end
              of 2020 we shift the focus from autumnal vaccinations to focus 
              instead on the period where the first wave of COVID hit the UK
              (around March-May 2020) and the period where vaccinations began
              in earnest (around January-March 2021).
              <br>
              <br>
              Looking at carer registrations COVID had a significant effect,
              with higher rates of registration across all age bands. For 
              working age patients the largest effect was seen around the 
              beginning of 2021, when the vaccine rollout began, with carers
              receiving vaccines sooner than normal for their age band. For 
              older carers there was little-to-no change in registration rates
              in the same period, likely due to 70+ patients receiving vaccines
              early regardless of whether they were carers or not. However, 
              there is a large spike in registrations for these age bands at the
              beginning of the pandemic (March-May 2020), when clinically 
              extremely vulnerable patients (who may have had care-requirements)
              were advised to shield, and an identification programme was rolled
              out nationally and in GP practices.
              </p>")

text20 <- HTML("<H2> COVID Carer Registrations - IMD Shift </H2>
              <br>
              Repeating the same methods to compare the deprivation levels of
              areas in which patients who registered during signal times to
              patients who registered in 'usual' times, we can see the same 
              patterns - during these drives it was more often patients from
              less deprived areas who were identified at GP practices as carers,
              a finding which again holds true over the whole population, and 
              when split by sex and age-band.
              <br><br>
              Please click the buttons above the plot to switch between the 
              overall IMD-shift, the shift split by sex, or split by age-band. 
              </p>")

text21 <- HTML("<H2> CMS Score Comparison </H2>
              <br>
              Looking at the distribution of CMS scores for 60-69 year old 
              carers and non-carers, it is clear that carers have higher risk 
              scores, and hence higher health needs. To compare better, we next
              began artificially increasing the age of carers to find the 
              optimal age increase at which the '60-69' year old (post-increase)
              carer score distribution best matched the 60-69 (true age) 
              non-carers.
              </p>")

text22 <- HTML("<H2> CMS Score Comparison - +5 Years </H2>
              <br>
              Adding five years to the carer age improves the distribution 
              match slightly, with fewer high-score patients (who have moved up
              to the 70-79 age-band) and more low-score patients (who have moved 
              up from the 50-59 age-band).
              </p>")

text23 <- HTML("<H2> CMS Score Comparison - +10 Years </H2>
              <br>
              Similarly, adding another five years to the carer age again 
              improves the match between the two distributions.
              </p>")

text24 <- HTML("<H2> CMS Score Comparison - +15 Years </H2>
              <br>
              Now adding another five years seems to make some small
              improvement, although arguably the fit is around as good as it was
              without adding the five years.
              </p>")

text25 <- HTML("<H2> CMS Score Comparison - +20 Years </H2>
              <br>
              Here, adding five years appears to decrease the match, with too 
              few high-score patients and too many low-score patients.
              </p>")

text26 <- HTML("<H2> CMS Score Comparison - +25 Years </H2>
              <br>
              It is clear that we have passes the 'optimal' age-shift, as again
              the '60-69' carers are too weighted towards low-risk patients. 
              While not specific, the best fit between carers and non-carers 
              ocurred somewhere between a 10-15 year carer age-shift.
              </p>")

text27 <- HTML("<H2> CMS Score Comparison - All Age Bands </H2>
              <br>
              Taking this method, we extended it to all age-bands, and 
              calculated an average goodness-of-fit score between the carer and 
              non-carer risk score distributions. Any age-bands which featured 
              fewer than 100 carers were omitted from the calculation.
              <br><br>
              Here we can see all age-bands comparing carers and non-carers in
              terms of risk score, and it is clear that at all ages carers 
              feature higher levels of health risk (greater numbers of higher 
              need patients and lower numbers of low-need patients).
              </p>")

text28 <- HTML("<H2> CMS Score Comparison - All Age Bands +13 Years </H2>
              <br>
              Using the goodness-of-fit calculation we found that the optimal
              age-shift of carers was 13 years - that is, when comparing 
              different patients by using the CMS risk score carers in Leeds
              appear to match best with patients 13 years older than themselves.
              This method was repeated varying the size of the age-bands (from
              5 years up to 20 years) and roughly the same optimal age-shift was
              found.
              </p>")

text29 <- HTML("<H2> Hospital Admissions - Elective Inpatient </H2>
              <br>
              During our Task and Finish group meetings, many carers brought up
              anecdotes that they and other carers they know had cancelled 
              hospital operations because of the recovery time, because they had
              no way of covering for their caring responsibilities. In the case
              above (where registered carers have more similar health 
              characteristics with people 10-15 years older than themselves) 
              these anecdotes could explain the increased health risk of carers,
              due to a lack of maintainance of known conditions.
              <br>
              <br>
              In the case of 
              our T&F group, many experiences were discussed. One carer 
              had conditions flagged at
              GP appointments which were not resolved with an elective 
              operation, despite steady worsening over a number of years, 
              resulting in a non-elective admission to hospital. Another carer
              discussed cancelling an operation which would have resolved 
              chronic pain issues - resulting in no further hospitalisation but
              decreased quality of life.
              <br>
              <br>
              Within our data set we do not have access to hospital bookings, 
              and so were unable to directly compare the cancellation rates of 
              carers and non-carers. However, as a proxy, we have been able to 
              use SUS to compare population rates of elective inpatient 
              admission to A&E attendances.
              <br>
              <br>
              Interestingly, when we split the proportion of Leeds-registered 
              patients who attended an elective inpatient spell (split by sex, 
              age-band, and carer status), we generally find that registered 
              carers are approximately as likely as non-registered carers.
              </p>")

text30 <- HTML("<H2> Hospital Admissions - A&E </H2>
              <br>
              However, when we look at the percentage of the population who 
              attended A&E, we find that generally registered carers are more 
              likely than non-registered carers to attend A&E, when split by 
              sex and age-band.
              <br>
              <br>
              This increase is generally seen more in female patients than male,
              and for both male and female the biggest differences are seen 
              around 40-59 year olds (although female 18-79 year olds 
              near-consistently show this trend, wheras it is only really seen
              for male 30-59 year old patients).
              </p>")

text31 <- HTML('<p><br> <br> <br> <br> <br> <br> <br> <br></p>')

nel_text <- HTML(
  "<span style='font-size:20px'>
    How does the health of registered carers compare with non-carers?
  </span>
  <br>
  <br> 
  <p>
  As discussed above, in surveys carers have reported that caring has negative
  impacts on both their physical and mental health, and as such it could be 
  expected that the prevelance of chronic health conditions would be 
  significantly higher amongst carers than non-carers, when accounting for 
  age and sex differences.
  <br>
  <br>
  To test this, for each patient in Leeds we combined data from historic GP
  appointments, including information about medication prescribing and clinical
  diagnoses. To estimate each patient's health need, we calculated their 
  Cambridge Multimorbidity Score (CMS; Payne et al. 2020). This was done by searching for 37 
  specific conditions (including many QOF conditions) and, for each condition, 
  if a patient reached a certain criteria (such as '<i>condition ever 
  recorded</i>', or '<i>4 or more specific prescriptions within the past 12 
  months</i>') then a flag for each condition was listed. For each met condition
  we took the 'Unplanned Admission' weights calculated by Payne et al., which
  estimates the risk of a patient having an unplanned hospital admission within
  the next year, and added the risk scores together to estimate a total 
  unplanned admission risk.
  <br>
  <br>
  Generally, the patient risk scores were constant when split by sex and 
  age-band, and so by stratifying carer by sex and age-band, then comparing
  their scores to those of similarly split non-carers we can see how different
  the health needs of the two populations are.
  </p>")

concludingtext <- HTML("")

technicalnotes <- HTML("")

### ALL PLOT OBJECTS

## Intro plot
# Intro static ggplot
introggPlot <- data %>%
  filter(age_band == 40) %>%
  ggplot(aes(text = round(snr, 2), text2 = round(snr_lcl, 2))) +
  geom_rect(
    aes(xmin = ymd('2016-09-01'), xmax = ymd('2016-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2017-09-01'), xmax = ymd('2017-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2018-09-01'), xmax = ymd('2018-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2019-09-01'), xmax = ymd('2019-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_step(aes(gp_date, n, colour = sex), size = 1, position = position_nudge(-30)) +
  theme(legend.position = 'none') +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2016-11-01'), yend = 30),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2017-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2018-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2019-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  annotate(
    'text',
    label = 'Signal (usual vaccine drive)',
    x = ymd('2018-03-01'),
    y = 53
  ) +
  labs(x = '', y = '')

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.65, y = 0.925),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

## Carer deprivation score density
deprivation_density <- carer_info %>%
  drop_na() %>% 
  filter(age_band < 70, date < ymd('2020-01-01')) %>%
  group_by(signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationTotalPlot <- ggplotly(deprivation_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationTotalPlot <- deprivationTotalPlot %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

deprivationTotalPlot <- neaten_plotly_labels(deprivationTotalPlot)

deprivation_male_density <- carer_info %>%
  drop_na() %>% 
  filter(age_band < 70, date < ymd('2020-01-01')) %>%
  filter(sex == 'M') %>%
  group_by(sex, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  facet_wrap(~ sex) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationPlot <- ggplotly(deprivation_male_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlot <- deprivationPlot %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

deprivation_female_density <- carer_info %>%
  drop_na() %>% 
  filter(age_band < 70, date < ymd('2020-01-01')) %>%
  filter(sex == 'F') %>%
  group_by(sex, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) +
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  facet_wrap(~ sex) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationPlotF <- ggplotly(deprivation_female_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlotF <- deprivationPlotF %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

deprivationPlot <- subplot(deprivationPlot, deprivationPlotF)
deprivationPlot <- neaten_plotly_labels(deprivationPlot)

# Age Band
deprivation_age_band_density <- carer_info %>%
  drop_na() %>% 
  filter(age_band < 70, date < ymd('2020-01-01')) %>%
  group_by(age_band, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  xlab('IMD Combined Score') +
  ylab('Density') + 
  facet_wrap(~ age_band)

deprivationAgeBandPlot <- ggplotly(deprivation_age_band_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationAgeBandPlot <- neaten_plotly_labels(deprivationAgeBandPlot)

source('scripts/deprivationPlotsCovid.R')

# NEL Distribution Plots
load(here('data/nelPlots.RData'))
nel_reference <- filter(nel_plots[[1]]$data, group)
load(here('data/compPlots.RData'))
load(here('data/oddsPlot.RData'))
load(here('data/hospitalAdmissionPlots.RData'))
