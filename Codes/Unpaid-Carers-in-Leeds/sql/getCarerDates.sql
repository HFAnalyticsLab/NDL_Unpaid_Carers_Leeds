WITH no_longer_carers_emis AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		DATEADD(ss, 1, EffectiveDate) AS gp_date,
		'no_longer_carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.EMIS_Event
	WHERE
		ReadCode COLLATE Latin1_General_CS_AS = '918f.'
),
no_longer_carers_tpp AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		DATEADD(ss, 1, EventDate) AS gp_date,
		'no_longer_carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.TPP_SRCodes_Extract
	WHERE
		Version3Code COLLATE Latin1_General_CS_AS = 'XaL1Y'
),
code_map AS (
  SELECT 
    DISTINCT [Read_Code_Prev]
  FROM 
    [UK_Health_Dimensions].[Read_Codes].[CV3_Description_Change_SCD]
  WHERE 
    Read_Code_Now COLLATE Latin1_General_CS_AS IN (
  	  'Ua0VL',
      'Xa96e',
      'Ua0VN',
      'Ua0VO',
      'Ua0bD',
      'Ua0bE',
  	--'Ub1ju',
      'Xm1Zl',
      'Xm1Zm',
      'Xm1Zs',
      'Xm1Zt',
      'XaKBe',
      'XaKBf',
      'XaKBg',
      'XaKBh',
      'XaKBi',
      'XaKBj',
      'XaKBl',
      'XaMHz'
    )
),
carers_emis AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EffectiveDate AS gp_date,
		'carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.EMIS_Event e
	JOIN code_map ON e.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Prev
--	WHERE
--		ReadCode COLLATE Latin1_General_CS_AS = '918G.'
),
carers_tpp AS (
	SELECT 
		Leeds_CCG_Patient_ID AS patient_id,
		EventDate AS gp_date,
		'carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.TPP_SRCodes_Extract
	WHERE
--		Version3Code COLLATE Latin1_General_CS_AS = 'Ua0VL'
    Version3Code COLLATE Latin1_General_CS_AS IN (
  	  'Ua0VL',
      'Xa96e',
      'Ua0VN',
      'Ua0VO',
      'Ua0bD',
      'Ua0bE',
  	--'Ub1ju',
      'Xm1Zl',
      'Xm1Zm',
      'Xm1Zs',
      'Xm1Zt',
      'XaKBe',
      'XaKBf',
      'XaKBg',
      'XaKBh',
      'XaKBi',
      'XaKBj',
      'XaKBl',
      'XaMHz'
    )
)
  SELECT * FROM no_longer_carers_emis
  UNION
  SELECT * FROM no_longer_carers_tpp
  UNION
  SELECT * FROM carers_tpp
  UNION
  SELECT * FROM carers_emis;