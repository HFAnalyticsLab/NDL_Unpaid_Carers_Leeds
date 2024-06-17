need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'readxl',
  'odbc'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)

# config <- get(file = "../config.yml")

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

#---- Get unpaid carers in GP records ----
gp_carers <- get_query(read_file('sql/getCarerStatus.sql'))

first_carer_flag <- get_query(read_file('sql/getCarerDates.sql')) %>% 
  filter(carer_flag == 'carer') %>% 
  semi_join(gp_carers, by = 'patient_id') %>% 
  group_by(patient_id) %>% 
  filter(gp_date == min(gp_date)) %>% 
  ungroup()

patient_df <- get_query(read_file('sql/getPrimaryPatients.sql')) %>%
  rename(
    patient_id = Leeds_CCG_Patient_ID,
    nhs_number = Patient_Pseudonym,
    lsoa = Patient_LSOA
  ) %>%
  mutate(
    age = as.numeric(age),
    bmi = as.numeric(bmi)
  )

carer_info <- patient_df %>% 
  mutate(age = as.numeric(age)) %>%
  inner_join(gp_carers, by = 'patient_id') 

leeds_patient_info <- patient_df %>% 
  mutate(age = as.numeric(age)) %>%
  left_join(gp_carers, by = 'patient_id') 

#---- Get missing LSOA info and MH referrals (MHSDS, IAPT, and GP) ----

referrals <- get_query(read_file('sql/GetReferrals.sql')) %>%
  mutate(
    referral_date = ymd(referral_date),
    discharge_date = ymd(discharge_date),
    referral_closed_date = ymd(referral_closed_date),
    referral_rejection_date = ymd(referral_rejection_date)
  )

# Load indices of deprivation and remove health component to calculate imd
iod <- read_excel('data/IoD2019.xlsx') %>%
  select(
    lsoa = 1,
    income_score = 5,
    employment_score = 6,
    education_score = 7,
    health_score = 8,
    crime_score = 9,
    barriers_score = 10,
    living_environment_score = 11
  )

imd <- iod %>% 
  transmute(
    lsoa,
    imd_score = (0.225 * income_score +
                   0.225 * employment_score +
                   0.135 * education_score +
                   		0.135 * health_score +
                   0.093 * crime_score +
                   0.093 * barriers_score + 
                   0.093 * living_environment_score),
    imd_decile = 11 - ntile(imd_score, 10)
  )

patients_of_interest <- carer_info %>% 
  filter(is.na(lsoa), !is.na(nhs_number)) %>%
  distinct(nhs_number) %>%
  pull(nhs_number)

for (ac in c('AE', 'IP', 'OP', 'COM_MH', 'AtA')) {
  post <- case_when(
    ac == 'AE' ~ '_Usual_Address',
    ac == 'COM_MH' ~ '',
    TRUE ~ '_of_Usual_Address'
  )
  
  for (fy in c('1617', '1718', '1819', '1920', '2021')) {
    cat(ac, fy, '\n')
    if (ac == 'AE' & fy == '1617') patient_lsoa <- tibble()
    if (ac == 'COM_MH' & fy == '2021') next
    patient_lsoa <- get_query(
      str_replace_all(
        read_file("sql/GetOutpatientAppointments.sql"),
        c(
          "<XYXY>" = fy,
          "<nhs_number>" = paste(patients_of_interest, collapse = "', '"),
          "<AEIPOP>" = ac,
          "<_of_Usual_Address>" = post
        )
      )
    ) %>%
      bind_rows(patient_lsoa)
  }
}

asc_lsoa <- get_query(
  str_replace_all(
    "SELECT NHS_Number AS nhs_number, LSOA_Code AS lsoa
    FROM [Warehouse_LDM].[dbo].[Pseudo_ASC_Register]
    WHERE NHS_Number is not null
    AND NHS_Number IN ('<nhs_number>')",
    "<nhs_number>",
    paste(patients_of_interest, collapse = "', '")
  )
)

all_lsoa <- patient_lsoa %>%
  bind_rows(asc_lsoa) %>%
  drop_na() %>%
  distinct()

iapt_referrals <- get_query("
  WITH bridging AS (
  	SELECT Person_ID AS patient_id, PSEUDO_NHSNumber AS nhs_number FROM [Warehouse_LDM].[iapt].[Bridging]
  	UNION
  	SELECT Person_ID AS patient_id, Pseudo_NHS_Number AS nhs_number FROM [Warehouse_LDM].[iapt_v1.5].[Bridging]
  ),
  referrals AS (
  	(SELECT DISTINCT
  		COALESCE(Person_ID, LocalPatientId) AS patient_id,
  		ReferralRequestReceivedDate AS referral_date
  	FROM
  		[Warehouse_LDM].[iapt].[IDS101Referral])
  	UNION
  	(SELECT DISTINCT
  		IAPT_PERSON_ID AS patient_id,
  		REFRECDATE AS referral_date
  	FROM
  		[Warehouse_LDM].[iapt_v1.5].[Referral])
  )
  SELECT 
  	nhs_number,
  	referral_date
  FROM
  	referrals r
  LEFT JOIN bridging b ON	
  	r.patient_id = b.patient_id;
") %>%
  mutate(referral_date = as_date(referral_date)) %>%
  distinct()

gp_mh <- get_query("
  WITH emis_bridge AS (
  	SELECT * FROM [Warehouse_LDMPC].[dbo].[EMIS_Patients]
  ),
  tpp_bridge AS (
  	SELECT * FROM [Warehouse_LDMPC].[dbo].[TPP_Patients]
  )
  SELECT eb.Patient_Pseudonym AS nhs_number, ee.EffectiveDate AS referral_date
  FROM [Warehouse_LDMPC].[dbo].[EMIS_Event] ee
  JOIN emis_bridge eb ON ee.Leeds_CCG_Patient_ID = eb.Leeds_CCG_Patient_ID
  WHERE ReadCode LIKE 'E%'
  UNION
  SELECT tb.Patient_Pseudonym AS nhs_number, te.EventDate AS referral_date
  FROM [Warehouse_LDMPC].[dbo].[TPP_SRCodes_Extract] te
  JOIN tpp_bridge tb ON te.Leeds_CCG_Patient_ID = tb.Leeds_CCG_Patient_ID
  WHERE Version3Code LIKE 'E%';
") %>%
  mutate(referral_date = as_date(referral_date)) %>%
  distinct()

mhsds_iapt_gp <- bind_rows(
  select(referrals, nhs_number, referral_date),
  iapt_referrals,
  gp_mh
)

#---- Process ----
carer_imd <- carer_info %>%
  rownames_to_column('id') %>%
  left_join(all_lsoa, by = 'nhs_number') %>%
  mutate(lsoa = coalesce(lsoa.x, lsoa.y)) %>%
  left_join(imd, by = 'lsoa') %>%
  group_by(nhs_number) %>%
  summarise(imd_decile = coalesce(median(imd_decile, na.rm = TRUE), -1))

carer_info <- carer_info %>% left_join(carer_imd, by = 'nhs_number')

carer_info %>%
  ggplot() +
  geom_histogram(
    aes(age, fill = sex), 
    binwidth = 1, 
    position = 'identity', 
    alpha = 0.4
  )

carer_info %>%
  ggplot() +
    geom_histogram(
      aes(imd_decile, fill = sex),
      binwidth = 1,
      position = 'identity',
      alpha = 0.4
    )

leeds_patient_info %>%
  ggplot() +
  geom_density(
    aes(age, fill = carer_flag), 
  #  binwidth = 1, 
  #  position = 'identity', 
    alpha = 0.4
  )

leeds_patient_info %>%
  ggplot(aes(x = imd_decile, fill = carer_flag, group = carer_flag)) +
  geom_histogram(
    aes(y = c(..count..[..group..==1]/sum(..count..[..group..==1]), ..count..[..group..==2]/sum(..count..[..group..==2]))),
    alpha = 0.4,
    position = 'identity',
    binwidth = 1
  )


carer_mh <- first_carer_flag %>%
  inner_join(select(carer_info, patient_id, age, sex, nhs_number), by = 'patient_id') %>%
  left_join(mhsds_iapt_gp, by = 'nhs_number') %>%
  distinct()

carer_match <- carer_info %>%
  select(
    nhs_number,
    age,
    sex,
    imd_decile
  )

#---- Get GP events for stress related appointments ----
stress_codes <- read_csv('codes/stressReadCodes.csv')

gp_stress <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV2.sql'),
    c(
      '<READCODEV2>' = paste0("'", paste(stress_codes$code, collapse = "', '"), "'")
    )
  )
)

carer_codes <- read_csv('codes/carerReadCodes.csv') #%>%
  # filter(description != 'No longer a carer')

all_carers <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV2.sql'),
    c(
      '<READCODEV2>' = paste0("'", paste(carer_codes$read_code_v2, collapse = "', '"), "'")
    )
  )
)

care_ended <- all_carers %>% 
  filter(str_detect(read_code, '918f')) %>% 
  distinct(nhs_number)

all_carers <- all_carers %>% 
  anti_join(care_ended, by = 'nhs_number')

#---- Get Non-elective Inpatient Spells ----

for (fy in c('1617', '1718', '1819', '1920', '2021', '2122')) {
  if (fy == '1617') nel_episodes <- tibble()
  
  nel_episodes <- get_query(
    str_replace_all(
      read_file("sql/getInpatients.sql"),
      c(
        "<XYXY>" = fy
      )
    )
  ) %>%
    mutate(
      admission_date = ymd(admission_date),
      discharge_date = ymd(discharge_date),
      episode_start_date = ymd(episode_start_date),
      episode_end_date = ymd(episode_end_date),
      bame = !coalesce(ethnic_group, '') %in% c('White', '')
    ) %>%
    bind_rows(nel_episodes)
}

nel_spells <- nel_episodes %>% distinct(.keep_all = TRUE)


#---- Carer's Allowances data from DWP ----
carer_allowance <- read_csv('data/carer_allowance_2018_present.csv') %>%
    select(-contains('- Annotations')) %>%
    pivot_longer(!Quarter, names_to = 'quarter', values_to = 'count') %>%
    rename(area = Quarter) %>%
    mutate(
      quarter = my(quarter),
      count = as.numeric(count)
    )

carer_allowance_2003 <- read_csv('data/carer_allowance_2003_2018.csv') %>%
  select(-contains('- Annotations')) %>%
  pivot_longer(!Quarter, names_to = 'quarter', values_to = 'count') %>%
  rename(area = Quarter) %>%
  mutate(
    quarter = my(quarter),
    count = as.numeric(count)
  )

carer_allowance <- carer_allowance %>% bind_rows(carer_allowance_2003)

#---- Carer's Assessments ----
carer_assessment <- get_query(read_file('sql/getCarerAssessments.sql')) %>%
  mutate(assessment_date = dmy(assessment_date))

carer_assessment2 <- get_query(
  "WITH carers_assessment AS (
	SELECT *
	FROM [Warehouse_LDM].[dbo].[CIS_Referrals_Hist]
	WHERE Referrals_Decision LIKE 'Carer%'
)
SELECT
	NHS_Number AS nhs_number,
	Year_of_Birth AS yob,
	Gender AS sex,
	LSOA_Code AS lsoa,
	Ethnic_Type AS ethnic_group,
	Ethnic_Sub_Type as ethnic_sub_group,
	Start_Date AS assessment_date
FROM
	Warehouse_LDM.dbo.Pseudo_ASC_Register r
JOIN carers_assessment ON 
	r.User_ID = carers_assessment.Person_Ref
 WHERE 
	NHS_Number IS NOT NULL;"
) %>%
  group_by(nhs_number) %>%
  filter(assessment_date == min(assessment_date)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(
    assessment_date = dmy(assessment_date),
    yob = as.numeric(yob),
    age = year(assessment_date) - yob
  )

carer_services <- get_query(
  "SELECT
  	Sup_Start_Date,
    Sup_End_Date,
    SupEndReasonDesc,
    Frequency,
    Unit1,
    ChargePeriod,
    Primary_Sup_Reason,
	  NHS_Number AS nhs_number,
	  Person_Ref AS asc_id,
  	Year_of_Birth AS yob,
  	Gender AS sex,
  	LSOA_Code AS lsoa,
  	Ethnic_Type AS ethnic_group,
  	Ethnic_Sub_Type as ethnic_sub_group,
  	Referral_ID AS referral_id
  FROM
  	[Warehouse_LDM].[dbo].[CIS_Service_Provision] sp
  JOIN [Warehouse_LDM].[dbo].[Pseudo_ASC_Register] r ON
    sp.Person_Ref = r.User_ID
  WHERE
--  	UPPER(Primary_Sup_Reason) LIKE '%Carer%'
    Client_Group LIKE '%Carer%';"
) %>%
  mutate(
    Frequency = as.numeric(Frequency),
    Sup_Start_Date = dmy(Sup_Start_Date),
    Sup_End_Date = dmy(Sup_End_Date),
    Sup_End_Date = coalesce(
      if_else(
        ChargePeriod == 'One Off Charge',
        Sup_Start_Date,
        Sup_End_Date
      ), 
      ymd('2025-01-01')
    )
  )

glimpse(carer_services)

carer_services2 <- carer_services %>%
  filter(!is.na(Unit1), Unit1 != 'Each') %>%
  mutate(
    Frequency = case_when(
      (Unit1 == 'Hour') & (ChargePeriod == 'Week') ~ Frequency / 7,
      (Unit1 == 'Hour') & (ChargePeriod == 'Year') ~ Frequency / 52 / 7,
      (Unit1 == 'Day') & (ChargePeriod == 'Week') ~ Frequency * 7.5 / 7,
      (Unit1 == 'Day') & (ChargePeriod == 'Year') ~ Frequency * 7.5 / 52 / 7,
      (Unit1 == 'Week') & (ChargePeriod == 'Year') ~ Frequency * 7.5 * 5 / 52 / 7,
      # Unit1 == 'Week' ~ Frequency * 7 * 7.5 / 52,
      # Unit1 == 'Day' ~ Frequency * 7.5,
      # Unit1 == 'Hour' ~ Frequency,
      Unit1 == 'Pounds' & ChargePeriod == 'Year' ~ Frequency / 365.25,
      Unit1 == 'Pounds' & ChargePeriod == 'Week' ~ Frequency / 7,
      Unit1 == 'Pounds' & ChargePeriod == 'One Off Charge' ~ Frequency
    ),
    Unit1 = case_when(
      Unit1 %in% c('Week', 'Day', 'Hour') ~ 'Hour',
      TRUE ~ 'Pounds'
    )
  )

carer_services3 <- carer_services2 %>%
  filter(Unit1 == 'Pounds') %>%
  mutate(day = map2(Sup_Start_Date, Sup_End_Date, seq, by = '1 day')) %>%
  unnest(day) %>%
  distinct()

# Stretch each service into one "unit" per day for start date to end date.
#   Then round to `date_rounder` (e.g. round to month) and count total hours and 
#   pounds.
date_rounder <- 'year'
imd_2019 <- calculate_deprivation_ntile()

services_by_date <- 
  carer_services2 %>%
  left_join(imd_2019, by = 'lsoa') %>%
  rowwise() %>%
  do(
    data.frame(
      date = seq(.$Sup_Start_Date, .$Sup_End_Date, by = '1 day'),
      hours = if_else(.$Unit1 == 'Hour', .$Frequency, 0),
      pounds = if_else(.$Unit1 == 'Pounds', .$Frequency, 0),
      imd = .$deprivation_decile,
      n_people = 1,
      one_off = .$ChargePeriod == 'One Off Charge'
    )
  ) %>%
  ungroup() %>%
  # mutate(date = round_date(date, date_rounder)) %>%
  group_by(date, imd) %>%
  summarise(
    n_people = sum(n_people),
    hours = sum(hours, na.rm = TRUE),
    pounds = sum(pounds, na.rm = TRUE),
    one_off = sum(one_off, na.rm = TRUE)
  )

# Transformation constant for second axis scaling
b <- max(services_by_date$pounds) / max(services_by_date$hours)

# Plot hours of ASC worker time per `date_rounder`, pounds per `date_rounder`, 
#   and number of first time GP appointments mentioning unpaid carer per
#   `date_rounder`. Left axis is hours, right axis is pounds. Green is GP
#   appointments
services_by_date %>%
  ggplot(aes(date, hours)) + 
    geom_line(size = 2, colour = 'red') + 
    geom_line(aes(date, pounds / b), size = 2, colour = 'blue') + 
    # geom_line(
    #   aes(date, n),
    #   size = 2,
    #   colour = 'green',
    #   data = all_carers %>%
    #     filter(!is.na(nhs_number)) %>%
    #     group_by(nhs_number) %>%
    #     filter(gp_date == min(gp_date)) %>%
    #     ungroup() %>%
    #     transmute(
    #       nhs_number,
    #       date = round_date(as_date(gp_date), date_rounder)
    #     ) %>%
    #     distinct() %>%
    #     count(date)
    # ) +
    # geom_line(
    #   aes(quarter, count),
    #   size = 2, 
    #   colour = 'purple',
    #   data = carer_allowance %>%
    #     filter(area == 'Leeds') %>%
    #     mutate(count = count / max(count, na.rm = TRUE) * max(services_by_date$hours))
    # ) +
    scale_x_date('', limits = c(ymd('2015-01-01'), ymd('2022-01-01'))) +
    scale_y_continuous('Hours', sec.axis = sec_axis(~ . * b, name = 'Pounds')) +
    theme(
      axis.line.y.left = element_line(colour = 'red'), 
      axis.ticks.y.left = element_line(colour = 'red'),
      axis.text.y.left = element_text(colour = 'red'),
      axis.title.y.left = element_text(colour = 'red', face = 'bold'),
      axis.line.y.right = element_line(colour = 'blue'), 
      axis.ticks.y.right = element_line(colour = 'blue'),
      axis.text.y.right = element_text(colour = 'blue'),
      axis.title.y.right = element_text(colour = 'blue', face = 'bold')
    )

#---- Appointment DNA/cancellation proportions ----

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

patients <- get_query(
  'SELECT 
      Patient_ID AS nhs_number,
      Gender AS sex,
      Year_of_Birth AS yob,
      Practice_Code AS gpp_code,
      Registration_Date AS reg_date,
      Deregistration_Date AS dereg_date,
      Date_of_Death AS dod
  FROM 
      [Warehouse_IHSC].[dbo].[GP_Patient_Register];'
)

patient_info <- get_query(
  'SELECT 
       * 
   FROM 
       Warehouse_IHSC.dbo.GP_All 
   WHERE 
       Patient_LSOA IS NOT NULL
   ORDER BY
       Attendance_Date;'
)

# *Should* get min/max dates for any LSOA for each patient
#    may take a while to run...
patient_info2 <- patient_info %>%
  group_by(Patient_ID, Patient_LSOA) %>%
  filter(Attendance_Date %in% range(Attendance_Date)) %>%
  ungroup() %>%
  distinct(
    nhs_number = Patient_ID,
    date = Attendance_Date,
    lsoa = Patient_LSOA
  ) %>%
  mutate(
    date = ymd(date)
  )

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

patient_info3 <- patient_info2 %>%
  group_by(nhs_number) %>%
  summarise(
    lsoa = mode(lsoa)
  ) %>%
  ungroup()

allc <- all_carers %>%
  filter(!is.na(nhs_number)) %>%
  transmute(
    nhs_number,
    gp_date = as_date(gp_date),
    carer = !str_detect(read_code, '918f')
  ) %>%
  left_join(patient_info3, by = 'nhs_number') %>%
  left_join(patients, by = 'nhs_number') %>%
  mutate(age = year(gp_date) - yob)

#### from stackoverflow - how to find nearest element of two vectors efficiently
# a <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) #has > 2 mil elements
# b <- c(4,6,10,16) # 200000 elements
# 
# cuts <- c(-Inf, b[-1]-diff(b)/2, Inf)
# # Will yield: c(-Inf, 5, 8, 13, Inf)
# 
# cut(a, breaks=cuts, labels=b)
# # [1] 4  4  4  4  4  6  6  6  10 10 10 10 10 16 16
# # Levels: 4 6 10 16
# This is even faster using a lower-level function like findInterval (which, again, assumes that breakpoints are non-decreasing).
# 
# findInterval(a, cuts)
# [1] 1 1 1 1 2 2 2 3 3 3 3 3 4 4 4
# So of course you can do something like:
#   
#   index = findInterval(a, cuts)
# b[index]
# # [1]  4  4  4  4  6  6  6 10 10 10 10 10 16 16 16
# Note that you can choose what happens to elements of a that are equidistant to an element of b by passing the relevant arguments to cut (or findInterval), see their help page.

all_carers <- all_carers %>%
  mutate(gp_date = as_date(gp_date)) %>%
  arrange(gp_date) %>%
  distinct()

# Testing
test_peep <- all_carers %>%
  semi_join(
    all_carers %>%
      filter(!is.na(nhs_number)) %>%
      count(nhs_number) %>%
      arrange(-n) %>%
      head(1),
    by = 'nhs_number'
  )

test_peep <- all_carers %>% filter(nhs_number == '')

tpd <- test_peep %>% pull(gp_date) %>% as.numeric()

pi2 <- patient_info2 %>% filter(nhs_number == '') %>%
  select(date, lsoa)

pi <- as.numeric(pi2$date)

cuts <- c(-Inf, pi[-1] - diff(pi) / 2, Inf)

interv <- findInterval(tpd, cuts)

test_peep$lsoa <- pi2$lsoa[interv]
test_peep$lsoa_date <- pi2$date[interv]

lsoa_match <- all_carers %>%
  left_join(patient_info2, by = 'nhs_number') %>%
  filter(!is.na(lsoa)) %>%
  mutate(
    gp_date_num = as.numeric(gp_date),
    date_num = as.numeric(date)
  ) %>%
  group_by(nhs_number) %>%
  nest(data = c(date_num, date, lsoa)) %>%
  group_by(nhs_number, gp_date) %>%
  mutate(
    interv = map(
      data,
      ~ findInterval(
        gp_date_num, 
        c(-Inf, .x$date_num[-1] - diff(.x$date_num) / 2, Inf)
      )
    ),
    lsoa_closest = map2(data, interv, ~ .x$lsoa[.y]),
    date_closest = map2(data, interv, ~ .x$date[.y])
  ) %>%
  select(-data, -interv) %>%
  unnest(c(lsoa_closest, date_closest)) %>%
  ungroup() %>%
  arrange(desc(date_closest)) %>%
  distinct(nhs_number, gp_date, read_code, .keep_all = TRUE) %>%
  filter(between(gp_date, ymd('2016-01-01'), ymd('2021-12-31')))

leeds_cancellations <- leeds_patient_info %>%
  select(nhs_number, patient_id, age, sex, carer_flag) %>%
  mutate(carer_flag = if_else(is.na(carer_flag), FALSE, TRUE)) %>%
  left_join(cancellations, by = 'patient_id') %>%
  group_by(nhs_number) %>%
  summarise(
    age = first(age),
    sex = first(sex),
    carer_flag = first(carer_flag),
    cancellations = sum(!is.na(read_code))
  )


#---- To be honest can just calculate mode of LSOA for now and add snazzy stuff later ----

# Cancellations from SUS

for (fy in c('1617', '1718', '1819', '1920', '2021', '2122')) {
  if (fy == '1617') {
    op_attendance <- tibble()
    ip_attendance <- tibble()
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
      admission_date = ymd(admission_date)
    ) %>%
    bind_rows(ip_attendance)
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
  filter(!is.na(nhs_number))

ip_attendance <- ip_attendance %>% 
  left_join(
    all_carers %>% 
      filter(!is.na(nhs_number)) %>% 
      distinct(nhs_number) %>%
      mutate(carer = TRUE),
    by = 'nhs_number'
  ) %>%
  mutate(carer = coalesce(carer, FALSE))

op_attendance %>% 
  mutate(
    age_band = 5 * floor(age / 5)
  ) %>% 
  count(
    carer, 
    age_band, 
    attendance_status = if_else(
      attendance_status %in% c(
        'Did not attend - no advance warning given', 
        'Patient arrived late and could not be seen', 
        'Appointment cancelled by, or on behalf of, the patient'
      ), 
      'dna', 
      'attended'
    )
  ) %>% 
  group_by(carer, age_band) %>% 
  mutate(n = round(100 * n / sum(n), digits = 2)) %>% 
  ungroup() %>% 
  filter(attendance_status == 'dna') %>% 
  ggplot() + 
    geom_line(aes(age_band, n, colour = carer), size = 1) + 
    ylim(0, NA)

ip_attendance %>% 
  mutate(
    age_band = 10 * floor(age / 10)
  ) %>% 
  count(carer, age_band = age, attendance_status = !str_detect(attendance_status, 'Procedure Carried Out')) %>% 
  group_by(carer, age_band) %>% 
  mutate(n = round(100 * n / sum(n), digits = 2)) %>% 
  ungroup() %>% 
  # filter(str_detect(attendance_status, 'Procedure Not Carried Out, for Medical')) %>%
  ggplot() + 
    geom_line(
      aes(age_band, n, colour = interaction(carer, attendance_status), group = interaction(carer, attendance_status)), 
      size = 1
    ) + 
    ylim(0, NA)

