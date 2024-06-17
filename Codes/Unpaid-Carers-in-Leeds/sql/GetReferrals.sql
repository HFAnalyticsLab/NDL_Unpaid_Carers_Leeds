WITH referral_source_lookup AS (
	SELECT
		Main_Code_Text AS code,
		Main_Description AS referral_source
	FROM
		[UK_Health_Dimensions].[Data_Dictionary].[Source_Of_Referral_For_Mental_Health_SCD]
	WHERE
		Is_Latest = 1
),
referral_reason_lookup AS (
	SELECT
		Main_Code_Text AS code,
		Main_Description AS referral_reason
	FROM
		[UK_Health_Dimensions].[Data_Dictionary].[Reason_For_Referral_To_Mental_Health_SCD]
	WHERE
		Is_Latest = 1
),
referral_closure_lookup AS (
	SELECT
		REPLACE(Main_Code_Text , '0', '') AS code,
		Main_Description AS referral_closure_reason
	FROM
		[UK_Health_Dimensions].[Data_Dictionary].[Referral_Closure_Reason_SCD] --[Discharge_From_Mental_Health_Service_Reason_SCD]
	WHERE
		Is_Latest = 1
),
referral_team_type AS (
  SELECT
		Main_Code_Text AS code,
		Main_Description AS referral_team_type
	FROM
		[UK_Health_Dimensions].[Data_Dictionary].[Service_Or_Team_Type_For_Mental_Health_SCD]
	WHERE
		Is_Latest = 1
),
referral_info AS (
	SELECT
		ServiceRequestId AS service_id,
		COALESCE(Person_ID, UniqMHSDSPersID) AS patient_id,
		referral_team_type,
		CareProfTeamLocalId AS referral_team_id,
		rc.referral_closure_reason,
		ReferClosureDate AS referral_closed_date,
		ReferRejectionDate AS referral_rejection_date,
		rc2.referral_closure_reason AS referral_rejection_reason,
		CASE
			WHEN ReferClosReason IN ('05', '07', '09', '5', '7', '9') THEN 'Dropout'
			ELSE 'Scheduled closure'
		END AS referral_closure_group
	FROM
		[Warehouse_LDM].[dbo].[v_SF_Latest_ServiceTypeReferredTo] s
	LEFT JOIN referral_closure_lookup rc ON
		REPLACE(s.ReferClosReason, '0', '') = rc.code
	LEFT JOIN referral_closure_lookup rc2 ON
		REPLACE(s.ReferRejectReason, '0', '') = rc2.code
	LEFT JOIN referral_team_type rt ON 
	  ServTeamTypeRefToMH = rt.code
),
p1 AS (
	SELECT 
		Organisation_Code,
		Organisation_Name
	FROM
		[UK_Health_Dimensions].[ODS].[All_Providers_and_purchasers_SCD]
	WHERE
		Is_Latest = 1
),
p2 AS (
	SELECT 
		Organisation_Code_5_Char,
		Organisation_Name
	FROM
		[UK_Health_Dimensions].[ODS].[All_Providers_SCD]
	WHERE
		Is_Latest = 1
),
p3 AS (
	SELECT 
		Organisation_Code,
		Organisation_Name
	FROM
		[UK_Health_Dimensions].[ODS].[NHS_Trusts_And_Trust_Sites_SCD]
	WHERE
		Is_Latest = 1
),
p4 AS (
	SELECT 
		Organisation_Code,
		Organisation_Name
	FROM
		[UK_Health_Dimensions].[ODS].[NonNHS_Organisations_SCD]
	WHERE
		Is_Latest = 1
),
p5 AS (
	SELECT 
		Organisation_Code,
		Organisation_Name
	FROM
		[UK_Health_Dimensions].[ODS].[Ind_Healthcare_Provider_Sites_SCD]
	WHERE
		Is_Latest = 1
),
p6 AS (
	SELECT 
		Organisation_Code,
		LA_Name
	FROM
		[UK_Health_Dimensions].[ODS].[Local_Authorities_In_England_And_Wales_SCD]
	WHERE
		Is_Latest = 1
),
organisation_lookup AS (
	SELECT
		OrgIDProv,
		COALESCE(p1.Organisation_Name,p2.Organisation_Name,p3.Organisation_Name,p4.Organisation_Name,p5.Organisation_Name,p6.LA_Name) AS provider_name
	FROM
		[Warehouse_LDM].[dbo].[v_SF_Latest_Referrals] r
	LEFT JOIN p1
		ON p1.Organisation_Code = r.OrgIDProv
	LEFT JOIN p2
		ON p2.Organisation_Code_5_Char = r.OrgIDProv
	LEFT JOIN p3
		ON p3.Organisation_Code = r.OrgIDProv
	LEFT JOIN p4
		ON p4.Organisation_Code = r.OrgIDProv
	LEFT JOIN p5
		ON p5.Organisation_Code = r.OrgIDProv
	LEFT JOIN p6
		ON p6.Organisation_Code = r.OrgIDProv
	GROUP BY
		OrgIDProv,
		COALESCE(p1.Organisation_Name, p2.Organisation_Name, p3.Organisation_Name, p4.Organisation_Name, p5.Organisation_Name, p6.LA_Name)
)
SELECT DISTINCT
	ServiceRequestId AS service_id,
	ReferralRequestReceivedDate AS referral_date,
  referral_source,
  referral_reason,
  ServDischDate AS discharge_date,
	COALESCE(Person_ID, UniqMHSDSPersID) AS patient_id,
	Pseudo_NHS_Number AS nhs_number,
	UniqMonthID AS month_id,
	AgeServReferRecDate AS age_at_referral,
	AgeServReferDischDate AS age_at_discharge,
	referral_team_id,
	referral_team_type,
	referral_closure_reason,
	referral_closure_group,
	referral_closed_date,
	referral_rejection_date,
	referral_rejection_reason,
	r.OrgIDProv AS provider_id,
	provider_name
FROM
	[Warehouse_LDM].[dbo].[v_SF_Latest_Referrals] r
LEFT JOIN referral_source_lookup rs ON
	r.SourceOfReferralMH = rs.code
LEFT JOIN referral_reason_lookup rr ON
	r.PrimReasonReferralMH = rr.code
LEFT JOIN referral_info ri ON
	COALESCE(r.Person_ID, r.UniqMHSDSPersID) = ri.patient_id AND
	r.ServiceRequestId = ri.service_id
LEFT JOIN organisation_lookup ol ON
	r.OrgIDProv = ol.OrgIDProv;