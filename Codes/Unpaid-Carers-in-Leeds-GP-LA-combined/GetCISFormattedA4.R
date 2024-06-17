get_cis_formatted_a4 <- function(){

library(readxl)
sca <- 
  readxl::read_xlsx(
    path = "C:/Users/BROWNRIGGA01/Documents/R/NDL_SC_Leeds/Carers Data for AB v2.xlsx",
    sheet = "Seperate Carers Assessments",
    col_names = T)

jca <- 
  readxl::read_xlsx(
    path = "C:/Users/BROWNRIGGA01/Documents/R/NDL_SC_Leeds/Carers Data for AB v2.xlsx",
    sheet = "Joint Carers Assessments",
    col_names = T)

# glimpse(jca)  
# glimpse(sca)  
# dim(jca)  
# dim(sca)  
# names(jca)  
# names(sca)  
# intersect(names(jca), names(sca))
# setdiff(names(jca), names(sca))

u <-
  sca %>% 
  rename(
    Unique_Person = `Unique Person`,
    Unique_Referral = `Unique Referral`,
    SU_Person_Ref = `SU Person_Ref`,
    SU_Nhs_No = `SU NHS_No`,
    SU_Age = `SU Age`,
    SU_Gender = `SU Gender`,
    SU_Ethnicity = `SU Ethnicity`,
    Primary_Sup_Reason = `Carer Primary_Sup_Reason`,
    Carer_Postcode = `Carer Postcode`,
    SU_Postcode = `SU Postcode`,
    Assessment_Id = CarerAss_Case_Number,
    Assessment_Start_Date = Carer_Ass_Start_Date,
    Assessment_End_Date = CarerAss_Completed_Date
  ) %>% 
  mutate(
    Decision = NA_character_,
    Joint_Carers_Assessment = NA_character_,
    SU_Postcode = if_else(SU_Postcode == "n/a", NA_character_, SU_Postcode)
    ) %>% 
  relocate(.data = ., c(starts_with("Carer"), Primary_Sup_Reason), .before = "Referral_Id") %>%   
  relocate(.data = ., c(starts_with("Unique"), Count), .before = "SU_Person_Ref")     

# setdiff(names(ca), names(jca))
v <-
  jca %>% 
  rename(
    Unique_Person = `Unique Person`,
    Unique_Referral = `Unique Referral`,
    SU_Person_Ref = Person_Ref,
    SU_Nhs_No = Nhs_No,
    SU_Age = Age,
    SU_Gender = Gender,
    SU_Ethnicity = Ethnicity,
    Carer_Postcode = Postcode...14,
    SU_Postcode = Postcode...8,
    Assessment_Start_Date = Start_Date,
    Assessment_End_Date = End_Date
  ) %>% 
  mutate(
    Carer_NHS_No = as.character(Carer_NHS_No),
    Carer_Postcode = if_else(Carer_Postcode == "n/a", NA_character_, Carer_Postcode)
  ) %>% 
  relocate(.data = ., c(starts_with("Carer"), Primary_Sup_Reason), .before = "Referral_Id") %>%   
  relocate(.data = ., c(starts_with("Unique"), Count), .before = "SU_Person_Ref")     
  # dim()
  # glimpse()

# check
# setdiff(names(u), names(v))
# setdiff(names(v), names(u))
# dim(sca)
# dim(u)
# dim(jca)
# dim(v)

ca <- 
  bind_rows(list(sca = u, jca = v), .id = "Sheet", ) %>% 
  rename_all(.tbl = ., .funs = tolower)

# dim(sca)
# dim(jca)
# 
# ca %>% 
#   # select(-Sheet) %>%
#   distinct() %>%
#   dim()
# 
# sca %>% 
#   distinct() %>%
#   dim()
# 
# jca %>% 
#   distinct() %>% 
#   dim()

# def postcode to imd link
# install.packages("remotes")
# remotes::install_github("nhs-r-community/LSOApop")
# glimpse(NHSRpopulation::imd)

pc_imd <- 
  left_join(geographr::lookup_postcode_oa11_lsoa11_msoa11_ltla20 %>% 
              select(postcode, lsoa11_code),
            # LSOApop::imd %>% 
            NHSRpopulation::imd %>% 
              select(lsoa_code, imd_score, imd_decile),
            by = c("lsoa11_code" = "lsoa_code")
  ) 

# band and format
aca <-
  ca %>% 
  select(-starts_with("su_")) %>%
  rename_with(.data = ., 
              .fn = ~if_else(str_detect(., "^carer"), str_remove(., "carer_"), .)) %>% 
  rename(jca = joint_carers_assessment,
         sup_decision = decision,
         sup_plan_id = support_plan_id,
         referral_decision = referrals_decision,
         referral_outcome = outcome,
         referral_reason = reason,
         referral_source = source,
         referral_contact_date = contact_date
         ) %>%
  mutate(postcode = str_remove(postcode, "[:blank:]")) %>%
  left_join(., pc_imd) %>%
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
        TRUE ~ NA_real_)
    ) %>% 
  mutate(.data = ., across(.cols = ends_with("date"), .fns = ymd)) %>%
  mutate(
    dt_t0_ref = referral_contact_date - dmy("01-01-2016"),
    dt_ref_ass = assessment_start_date - referral_contact_date,
    dt_ass = assessment_end_date - assessment_start_date,
    dt_ref_sup = sup_start_date - referral_contact_date,
    dt_sup = sup_end_date - sup_start_date,
    dt_ref_serv = service_start_date - referral_contact_date,
    dt_serv = service_end_date - service_start_date
  ) %>% 
  mutate(
    imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = T), na_level = "Unknown"),
    imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = T), na_level = "Unknown")
    ) %>%   
  mutate(.data = ., across(.cols = c(where(is.numeric), -age), 
                           .fns = as.character)
         ) %>% 
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
         ) %>%
  # group_by(person_ref, referral_id) %>% 
  # arrange(desc(contact_date)) %>% 
  # slice_head(.data = ., n = 1) %>% 
  filter(referral_contact_date >= ymd("2016-01-01") & referral_contact_date <= ymd("2021-12-31") & 
           (age >= 18 | is.na(age))
         ) %>%
  relocate(.data = ., c(referral_id, referral_contact_date, jca), .after = person_ref)

  return(aca)
}



