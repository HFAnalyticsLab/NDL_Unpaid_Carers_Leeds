SELECT 
  Patient_ID AS nhs_number,
  Gender AS sex,
  Year_of_Birth AS birth_year,
  Practice_Code AS gpp_code,
  Registration_Date AS registration_date,
  Deregistration_Date AS deregistration_date,
  Date_of_Death AS date_of_death
FROM 
  [Warehouse_IHSC].[dbo].[GP_Patient_Register];