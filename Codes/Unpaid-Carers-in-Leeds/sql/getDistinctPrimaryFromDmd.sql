WITH emis AS (
  SELECT DISTINCT
    Patient_Pseudonym AS nhs_number,
    EffectiveDate AS gp_date,
    DmdCode AS code,
    0 AS NumericValue
  FROM
    Warehouse_LDMPC.dbo.EMIS_Medication m
  JOIN Warehouse_LDMPC.dbo.EMIS_Patients p ON
    m.Leeds_CCG_Patient_ID = p.Leeds_CCG_Patient_ID
  WHERE 
    Patient_Pseudonym IS NOT NULL
    AND DmdCode IN (<ReadOrDmdCode>)
),
tpp AS (
  SELECT DISTINCT
    Patient_Pseudonym AS nhs_number,
    EventDate AS gp_date,
    DMDId AS code,
    0 AS NumericValue
  FROM
    Warehouse_LDMPC.dbo.TPP_Medication_Details m
  JOIN Warehouse_LDMPC.dbo.TPP_Patients p ON
    m.Leeds_CCG_Patient_ID = p.Leeds_CCG_Patient_ID
  WHERE 
    Patient_Pseudonym IS NOT NULL
    AND DMDId IN (<ReadOrDmdCode>)
)
SELECT * FROM emis
UNION
SELECT * FROM tpp;