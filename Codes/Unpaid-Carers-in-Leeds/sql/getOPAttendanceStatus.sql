WITH dna_lookup AS (
	SELECT 
		Main_Code_Text AS code, 
		Main_Description AS attendance_status
	FROM 
		[UK_Health_Dimensions].[Data_Dictionary].[Attended_Or_Did_Not_Attend_SCD]
	WHERE 
		Is_Latest = 1
),
ethnic_lookup AS (
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
	Date_of_Attendance AS attendance_date,
	Date_Appointment_Request AS appointment_request_date,
  attendance_status
FROM
	[Warehouse_LDM].[dbo].[Pseudo_OP_Data_FY<XYXY>] op
LEFT JOIN dna_lookup dl ON
  op.Attended_or_DNAd = dl.code
LEFT JOIN ethnic_lookup el ON
  op.Ethnic_Origin = el.code
LEFT JOIN sex_lookup sl ON
  op.Sex = sl.code;