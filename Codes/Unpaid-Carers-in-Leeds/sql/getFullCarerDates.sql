WITH no_longer_carers_emis AS (
	SELECT 
		Patient_ID AS nhs_number,
		DATEADD(ss, 1, Event_Date) AS gp_date,
		'no_longer_carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.GP_Events_Data
	WHERE
		ReadCode COLLATE Latin1_General_CS_AS = '918f.'
),
no_longer_carers_tpp AS (
	SELECT 
		Patient_ID AS nhs_number,
		DATEADD(ss, 1, Event_Date) AS gp_date,
		'no_longer_carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.GP_Events_Data
	WHERE
		ReadCode COLLATE Latin1_General_CS_AS = 'XaL1Y'
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
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		'carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.GP_Events_Data e
	JOIN code_map ON 
	  e.ReadCode COLLATE Latin1_General_CS_AS = code_map.Read_Code_Prev
),
carers_tpp AS (
	SELECT 
		Patient_ID AS nhs_number,
		Event_Date AS gp_date,
		'carer' AS carer_flag
	FROM
		Warehouse_LDMPC.dbo.GP_Events_Data
	WHERE
    ReadCode COLLATE Latin1_General_CS_AS IN (
  	  'Ua0VL',
      'Xa96e',
      'Ua0VN',
      'Ua0VO',
      'Ua0bD',
      'Ua0bE',
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