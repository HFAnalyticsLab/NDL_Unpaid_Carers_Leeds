WITH code_map AS (
  SELECT 
    DISTINCT [Read_Code_Prev]
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Now COLLATE Latin1_General_CS_AS IN (<READCODEV3>)
),
emis AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EffectiveDate AS gp_date
	FROM
		Warehouse_LDMPC.dbo.EMIS_Event e
	JOIN code_map ON 
	  e.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Prev
),
tpp AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EventDate AS gp_date
	FROM
		Warehouse_LDMPC.dbo.TPP_SRCodes_Extract
	WHERE
    Version3Code COLLATE Latin1_General_CS_AS IN (<READCODEV3>)
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;