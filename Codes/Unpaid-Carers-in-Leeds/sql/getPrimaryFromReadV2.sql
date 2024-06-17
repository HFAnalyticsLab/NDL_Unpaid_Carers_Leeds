WITH code_map AS (
  SELECT DISTINCT 
    Read_Code_Prev,
    Read_Code_Now COLLATE Latin1_General_CS_AS AS Read_Code_Now
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Prev COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
),
emis AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EffectiveDate AS gp_date,
		ReadCode COLLATE Latin1_General_CS_AS AS read_code
	FROM
		Warehouse_LDMPC.dbo.EMIS_Event e
	WHERE
    ReadCode COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
),
tpp AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EventDate AS gp_date,
		Version3Code COLLATE Latin1_General_CS_AS AS read_code
	FROM
		Warehouse_LDMPC.dbo.TPP_SRCodes_Extract t
	JOIN code_map ON 
	  t.Version3Code COLLATE Latin1_General_CS_AS = code_map.Read_Code_Now
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;