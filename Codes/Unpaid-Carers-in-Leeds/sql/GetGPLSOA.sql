(SELECT
      Patient_Pseudonym AS nhs_number,
      Patient_LSOA AS lsoa
FROM 
	[Warehouse_LDMPC].[dbo].[EMIS_Patients]
WHERE 
	Patient_LSOA IS NOT NULL
	AND Patient_Pseudonym IN ('<nhs_number>'))
	
UNION

(SELECT
      Patient_Pseudonym AS nhs_number,
      Patient_LSOA AS lsoa
FROM 
	[Warehouse_LDMPC].[dbo].[TPP_Patients]
WHERE 
	Patient_LSOA IS NOT NULL
	AND Patient_Pseudonym IN ('<nhs_number>'));