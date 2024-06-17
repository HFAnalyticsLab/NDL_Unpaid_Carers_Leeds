WITH code_map AS (
  SELECT DISTINCT
    Read_Code_Now COLLATE Latin1_General_CS_AS AS read_code_ctv3,
    Read_Code_Prev COLLATE Latin1_General_CS_AS AS read_code_v2
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Prev COLLATE Latin1_General_CS_AS IN (<READCODECTV3>)
    OR Read_Code_Now COLLATE Latin1_General_CS_AS IN (<READCODECTV3>)
),
emis AS (
	SELECT 
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		read_code_v2,
		read_code_ctv3
	FROM
		Warehouse_IHSC.dbo.GP_Events_Data e
	JOIN code_map ON
	  e.ReadCode COLLATE Latin1_General_CS_AS = code_map.read_code_v2
),
tpp AS (
	SELECT 
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		read_code_v2,
		read_code_ctv3
	FROM
		Warehouse_IHSC.dbo.GP_Events_Data t
	JOIN code_map ON 
	  t.ReadCode COLLATE Latin1_General_CS_AS = code_map.read_code_ctv3
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;