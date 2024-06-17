WITH code_map AS (
  SELECT DISTINCT
    Read_Code_Now,
    Read_Code_Prev
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Prev COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
    OR Read_Code_Now COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
),
emis AS (
	SELECT 
		Patient_Pseudonym AS nhs_number,
		EffectiveDate AS gp_date,
		ReadCode COLLATE Latin1_General_CS_AS AS read_code
	FROM
		Warehouse_LDMPC.dbo.EMIS_Event_Extended e
	JOIN code_map ON
	  e.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Prev
),
tpp AS (
	SELECT 
		Patient_Pseudonym AS nhs_number,
		EventDate AS gp_date,
		Read_Code_Prev AS read_code
	FROM
		Warehouse_LDMPC.dbo.TPP_SRCodes_Extract_Extended t
	JOIN code_map ON 
	  t.Version3Code COLLATE Latin1_General_CS_AS = code_map.Read_Code_Now
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;