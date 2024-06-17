# GP Appointment cancelled and operation cancelled
cancel_codes <- read_csv('codes/attendanceReadCodes.csv')

cancellations <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV2.sql'),
    c(
      '<READCODEV2>' = paste0("'", paste(cancel_codes$read_code_v2, collapse = "', '"), "'")
    )
  )
) %>%
  mutate(gp_date = as_date(gp_date)) %>%
  distinct()

cancellations <- cancellations %>% 
  mutate(read_code_v2 = str_remove(read_code, '\\.')) %>% 
  left_join(
    cancel_codes %>% 
      mutate(read_code_v2 = str_remove(read_code_v2, '\\.')) , 
    by = 'read_code_v2'
  )

# Cancellations from SUS
for (fy in c('1617', '1718', '1819', '1920', '2021', '2122')) {
  cat('\nGetting FY', fy, '...')
  
  if (fy == '1617') {
    op_attendance <- tibble()
    ip_attendance <- tibble()
    ae_attendance <- tibble()
  }
  
  op_attendance <- get_query(
    str_replace_all(
      read_file("sql/getOPAttendanceStatus.sql"),
      c(
        "<XYXY>" = fy
      )
    )
  ) %>%
    mutate(
      attendance_date = ymd(attendance_date),
      appointment_request_date = ymd(appointment_request_date)
    ) %>%
    bind_rows(op_attendance)
  
  ip_attendance <- get_query(
    str_replace_all(
      read_file("sql/getIPAttendanceStatus.sql"),
      c(
        "<XYXY>" = fy
      )
    )
  ) %>%
    mutate(
      admission_date = ymd(admission_date),
      discharge_date = ymd(discharge_date)
    ) %>%
    bind_rows(ip_attendance)
  
  ae_attendance <- get_query(
    str_replace_all(
      'SELECT DISTINCT 
      	 NHS_Number AS nhs_number,
         Att_Date_Arrival AS ae_date
       FROM 
         [Warehouse_LDM].[dbo].[Pseudo_AE_Data_FY<XYXY>];',
      c(
        '<XYXY>' = fy
      )
    )
  )  %>%
    mutate(
      ae_date = ymd(ae_date)
    ) %>%
    bind_rows(ae_attendance)
  
  cat(' Done!')
}

op_attendance <- op_attendance %>%
  distinct(spell_id, .keep_all = TRUE) %>%
  filter(!is.na(nhs_number))

first_care_flag <- all_carers %>%
  group_by(nhs_number) %>%
  filter(gp_date == min(gp_date)) %>%
  ungroup() %>%
  distinct()

op_attendance <- op_attendance %>% 
  left_join(
    all_carers %>% 
      filter(!is.na(nhs_number)) %>% 
      distinct(nhs_number) %>%
      mutate(carer = TRUE),
    by = 'nhs_number'
  ) %>%
  mutate(carer = coalesce(carer, FALSE))

ip_attendance <- ip_attendance %>%
  distinct(spell_id, .keep_all = TRUE) %>%
  filter(!is.na(nhs_number)) %>%
  mutate(
    los = (admission_date %--% discharge_date) / ddays()
  )

patient_full <- readRDS('patient_df.RDS') %>%
  filter(age > 17)

ae_count <- ae_attendance %>%
  mutate(cambridge_date = floor_date(ae_date, 'month')) %>%
  count(nhs_number, cambridge_date, name = 'ae')

patient_full <- patient_full %>% 
  left_join(ae_count, by = c('nhs_number', 'cambridge_date')) 

ae_plot <- patient_full %>% 
  mutate(age_band = pmax(pmin(10 * floor(age / 10), 90), 20)) %>% 
  group_by(
    sex, 
    cambridge_date = floor_date(cambridge_date, 'year'), 
    carer, 
    age_band
  ) %>% 
  summarise(
    n = n(), 
    ae_prop = 100 * sum(ae, na.rm = TRUE) / n(), 
    lcl = ae_prop - qnorm(1 - 0.05/2) * sqrt(ae_prop * (100 - ae_prop) / sum(n)),
    ucl = ae_prop + qnorm(1 - 0.05/2) * sqrt(ae_prop * (100 - ae_prop) / sum(n))
  ) %>% 
  ggplot() + 
    geom_ribbon(
      aes(cambridge_date, ymin = lcl, ymax = ucl, fill = carer), 
      alpha = 0.4, 
      size = 1
    ) + 
    facet_wrap(~ sex + age_band, nrow = 2) + 
    xlab('') + 
    ylab('Population Attending A&E [%]')        

ae_plot

ip_count <- ip_attendance %>%
  mutate(cambridge_date = floor_date(admission_date, 'month')) %>%
  count(nhs_number, cambridge_date, name = 'ip')

patient_full <- patient_full %>% 
  left_join(ip_count, by = c('nhs_number', 'cambridge_date')) 

ip_plot <- patient_full %>% 
  mutate(age_band = pmax(pmin(10 * floor(age / 10), 90), 20)) %>% 
  group_by(
    sex, 
    cambridge_date = floor_date(cambridge_date, 'year'), 
    carer, 
    age_band
  ) %>% 
  summarise(
    n = n(), 
    ip_prop = 100 * sum(ip, na.rm = TRUE) / n(), 
    lcl = ip_prop - qnorm(1 - 0.05/2) * sqrt(ip_prop * (100 - ip_prop) / sum(n)),
    ucl = ip_prop + qnorm(1 - 0.05/2) * sqrt(ip_prop * (100 - ip_prop) / sum(n))
  ) %>% 
  ggplot() + 
  geom_ribbon(
    aes(cambridge_date, ymin = lcl, ymax = ucl, fill = carer), 
    alpha = 0.4, 
    size = 1
  ) + 
  facet_wrap(~ sex + age_band, nrow = 2) + 
  xlab('') + 
  ylab('Population Inpatient Admissions [%]')        

ip_plot

# patients2 <- patients %>%
#   left_join(
#     all_carers %>% 
#       filter(!is.na(nhs_number)) %>% 
#       distinct(nhs_number) %>%
#       mutate(carer = TRUE),
#     by = 'nhs_number'
#   ) %>%
#   mutate(carer = coalesce(carer, FALSE))

ip_attendance2 <- ip_attendance %>%
  mutate(cambridge_date = map2(admission_date, discharge_date, seq, by = 'day')) %>%
  unnest(cambridge_date) %>%
  mutate(cambridge_date = round_date(cambridge_date, 'month')) %>% 
  distinct(nhs_number, cambridge_date, spell_id, pod = pod_code, .keep_all = TRUE)

patient_full <- patient_full %>%
  left_join(
    ip_attendance2 %>% 
      filter(between(year(cambridge_date), 2016, 2021), str_detect(pod, 'Non')) %>% 
      count(nhs_number, cambridge_date, name = 'n_nel_spells'), 
    by = c('nhs_number', 'cambridge_date')
  ) %>% 
  mutate(n_nel_spells = coalesce(n_nel_spells, 0))

patient_full <- patient_full %>%
  left_join(
    ip_attendance2 %>% 
      filter(between(year(cambridge_date), 2016, 2021), !str_detect(pod, 'Non')) %>% 
      count(nhs_number, cambridge_date, name = 'n_spells'), 
    by = c('nhs_number', 'cambridge_date')
  ) %>% 
  mutate(n_spells = coalesce(n_spells, 0))

patient_full %>% 
  filter(age > 17) %>% 
  mutate(
    age_band = case_when(
      age < 50 ~ '18-49',
      age < 60 ~ '50-59',
      age < 70 ~ '60-69',
      age < 80 ~ '70-79',
      TRUE ~ '80+'
    )
  ) %>% 
  group_by(cambridge_date, carer, age_band, sex) %>% 
  summarise(
    n_nel_spells = sum(n_nel_spells), 
    perct_spell = 100 * sum(n_nel_spells) / n_distinct(nhs_number)
  ) %>% 
  ggplot() + 
    geom_line(aes(cambridge_date, perct_spell, colour = carer), size = 1) + 
    facet_wrap(~ age_band + sex)

# Do carers have significantly higher non-elective to elective appointments than
#   non-carers? Specifically the ones we know about... Obvs
calculate_ratio_z <- function(y1, y2, n1, n2) {
  p <- (y1 + y2) / (n1 + n2)
  p1 <- y1 / n1
  p2 <- y2 / n2
  
  z <- ((p1 - p2) - 0) / sqrt(p * abs(1 - p) * (1 / n1 + 1 / n2))
  
  return(z)
}

patient_acute_df <- patient_df %>% 
  mutate(
    age_band = pmin(pmax(floor(age / 10) * 10, 20), 80)
  ) %>%
  group_by(nhs_number, year = year(cambridge_date)) %>% 
  summarise(
    carer = sum(carer) > 0, 
    nel = sum(n_nel_spells) > 0,
    el = sum(n_spells) > 0,
    age_band = mode(age_band)
  ) %>% 
  group_by(year, age_band, carer) %>%
  summarise(
    el = sum(el),
    nel = sum(nel),
    n = n_distinct(nhs_number),
    perct_el = 100 * el / n,
    perct_nel = 100 * nel / n
  ) %>%
  ungroup() %>%
  mutate(ratio = perct_el / perct_nel)

patient_acute_df %>% 
  group_by(year, age_band) %>% 
  summarise(
    z = calculate_ratio_z(nel[carer], nel[!carer], el[carer], el[!carer])
  ) %>% 
  mutate(
    significant = case_when(
      z > 1.96 ~ 'Carer NEL/EL >> Non-Carer NEL/EL', 
      z < -1.96 ~ 'Carer NEL/EL << Non-Carer NEL/EL', 
      TRUE ~ 'Not Significant'
    )
  )



#---- Do carers have more DNAs than non-carers? ----
cancellations2 <- cancellations %>%
  mutate(
    cambridge_date = round_date(gp_date, 'month'),
    dna = description == 'DNA no reason'
  ) %>% 
  group_by(nhs_number, cambridge_date) %>%
  summarise(
    n_dna = dna
  )


#---- Match carers and non-carers every month ----
# 
# ip_attendance <- ip_attendance %>% 
#   semi_join(patients, by = 'nhs_number') %>%
#   left_join(
#     all_carers %>% 
#       filter(!is.na(nhs_number)) %>% 
#       distinct(nhs_number) %>%
#       mutate(carer = TRUE),
#     by = 'nhs_number'
#   ) %>%
#   mutate(carer = coalesce(carer, FALSE))
# 
# op_attendance %>% 
#   mutate(
#     age_band = 5 * floor(age / 5)
#   ) %>% 
#   count(
#     carer, 
#     age_band, 
#     attendance_status = if_else(
#       attendance_status %in% c(
#         'Did not attend - no advance warning given', 
#         'Patient arrived late and could not be seen', 
#         'Appointment cancelled by, or on behalf of, the patient'
#       ), 
#       'dna', 
#       'attended'
#     )
#   ) %>% 
#   group_by(carer, age_band) %>% 
#   mutate(n = round(100 * n / sum(n), digits = 2)) %>% 
#   ungroup() %>% 
#   filter(attendance_status == 'dna') %>% 
#   ggplot() + 
#   geom_line(aes(age_band, n, colour = carer), size = 1) + 
#   ylim(0, NA)
# 
# ip_attendance %>% 
#   mutate(
#     age_band = 10 * floor(age / 10)
#   ) %>% 
#   count(
#     carer, 
#     age_band
#   ) %>% 
#   # group_by(carer, age_band) %>% 
#   # mutate(n = round(100 * n / sum(n), digits = 2)) %>% 
#   # ungroup() %>% 
#   # filter(str_detect(attendance_status, 'Procedure Not Carried Out, for Medical')) %>%
#   ggplot() + 
#   geom_line(
#     aes(age_band, n, colour = carer, group = carer), 
#     size = 1
#   ) + 
#   ylim(0, NA)
