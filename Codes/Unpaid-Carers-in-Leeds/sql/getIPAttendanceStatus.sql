WITH ethnic_lookup AS (
	SELECT 
		Main_Code_Text AS code, 
		Category AS ethnic_group
	FROM 
		[UK_Health_Dimensions].[Data_Dictionary].[Ethnic_Category_Code_SCD]
	WHERE 
		Is_Latest = 1
),
sex_lookup AS (
  SELECT 
		Main_Code_Text AS code, 
		Main_Description AS sex
	FROM 
		[UK_Health_Dimensions].[Data_Dictionary].[Sex_of_Patients_SCD]
	WHERE 
		Is_Latest = 1
)
SELECT
	Spell_ID AS spell_id,
	NHS_Number AS nhs_number,
	Age_At_Start_Of_Episode AS age,
	ethnic_group,
	sl.sex,
	Postcode_of_Usual_Address AS lsoa,
	ADMISSION_DATE AS admission_date,
	DISCHARGE_DATE AS discharge_date,
  CASE 
    WHEN HRG4_Code = 'WH50A' THEN 'Procedure Not Carried Out, for Medical or Patient Reasons'
    WHEN HRG4_Code = 'WH50B' THEN 'Procedure Not Carried Out, for Other or Unspecified Reasons'
    ELSE 'Procedure Carried Out'
  END AS attendance_status,
  CASE
    WHEN Method_Of_Admission_Code IN ('11', '12', '13') THEN 'Elective'
    WHEN Method_Of_Admission_Code IN ('21', '22', '23', '24', '25') THEN 'Non-Elective'
  END AS pod_code
FROM
	[Warehouse_LDM].[dbo].[Pseudo_IP_Data_FY<XYXY>] ip
LEFT JOIN ethnic_lookup el ON
  ip.Ethnic_Origin = el.code
LEFT JOIN sex_lookup sl ON
  ip.Sex = sl.code
WHERE 
  Method_Of_Admission_Code IN ('11', '12', '13', '21', '22', '23', '24', '25');