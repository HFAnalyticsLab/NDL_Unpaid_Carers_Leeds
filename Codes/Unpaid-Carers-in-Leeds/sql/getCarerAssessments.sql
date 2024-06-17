WITH carers_assessment AS (
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
	MIN(Start_Date) AS assessment_date
FROM
	Warehouse_LDM.dbo.Pseudo_ASC_Register r
JOIN carers_assessment ON 
	r.User_ID = carers_assessment.Person_Ref
-- WHERE 
--	NHS_Number IS NOT NULL
GROUP BY
	NHS_Number
	,Year_of_Birth
	,Gender
	,LSOA_Code
	,Ethnic_Type
	,Ethnic_Sub_Type
;