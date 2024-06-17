SELECT DISTINCT
  NHS_Number AS nhs_number,
  Postcode<_of_Usual_Address> AS lsoa
FROM
  [Warehouse_LDM].[dbo].[Pseudo_<AEIPOP>_Data_FY<XYXY>]
WHERE 
  NHS_Number IN ('<nhs_number>');