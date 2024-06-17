WITH code_map AS (
  SELECT DISTINCT
    Read_Code_Now,
    Read_Code_Prev
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Prev COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
),
emis AS (
	SELECT 
		CAST(PatientGuid AS varchar(36)) AS patient_id,
		EffectiveDateTime AS gp_date,
		ReadCode COLLATE Latin1_General_CS_AS AS read_code,
		Term AS read_text
	FROM
		Warehouse_BI.emis.bi_emis_Event e
	WHERE
    ReadCode COLLATE Latin1_General_CS_AS IN (<READCODEV2>)
),
tpp AS (
	SELECT 
		CAST(IDPatient AS varchar(36)) AS patient_id,
		DateEvent AS gp_date,
		Read_Code_Prev AS read_code,
		CTV3Text AS read_text
	FROM
		Warehouse_BI.sys1.bi_sys1_Code t
	JOIN code_map ON 
	  t.CTV3Code COLLATE Latin1_General_CS_AS = code_map.Read_Code_Now
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;