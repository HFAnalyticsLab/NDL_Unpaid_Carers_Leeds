WITH code_map AS (
  SELECT DISTINCT
    Read_Code_Now,
    Read_Code_Prev
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Prev COLLATE Latin1_General_CS_AS IN (<ReadOrDmdCode>)
),
emis AS (
	SELECT DISTINCT
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		ReadCode AS code,
		NumericValue AS value
	FROM
		Warehouse_IHSC.dbo.GP_Events_Data e
	JOIN code_map ON
	  e.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Prev
	WHERE 
	  Patient_ID IS NOT NULL
),
tpp AS (
	SELECT DISTINCT
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		ReadCode AS code,
		NumericValue AS value
	FROM
		Warehouse_IHSC.dbo.GP_Events_Data t
	JOIN code_map ON 
	  t.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Now
	WHERE 
	  Patient_ID IS NOT NULL
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;