/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [Patient_ID],
      [Age],
      [Sex]
	    ,[LTC_AF_Register]
      ,[LTC_Angina]
      ,[LTC_Asthma]
      ,[LTC_Cancer]
      ,[LTC_CHD]
      ,[LTC_CKD]
      ,[LTC_COPD]
      ,[LTC_Dementia]
      ,[LTC_Depression]
      ,[LTC_Diabetes]
      ,[LTC_DS]
      ,[LTC_Epilepsy]
      ,[LTC_HeartFailure]
      ,[LTC_Hypertension]
      ,[LTC_Hypothyroidism]
      ,[LTC_IHD]
      ,[LTC_LD]
      ,[LTC_LVD]
      ,[LTC_MH]
      ,[LTC_MI]
      ,[LTC_Osteoporosis]
      ,[LTC_PAD]
      ,[LTC_Stroke]
      ,[LTC_TIA]
      ,[LTC_Autism]
      ,[LTC_PersonalityDisorder]
      ,[LTC_CMHI_excl_Depression]
      ,[LTC_Liver_Disease_Severe]
      ,[LTC_Liver_Disease_Mid]
      ,[LTC_Inflammatory_Bowel_Disease]
      ,[LTC_Fibromyalgia]
      ,[LTC_Osteoarthritis]
      ,[LTC_Cystic_Fibrosis]
      ,[LTC_Cerebral_Palsy_Other_Paral]
      ,[LTC_Sleep_Apnoea]
      ,[LTC_Inflammatory_Arthritis]
      ,[LTC_Extreme_Obesity]
      ,[LTC_Multiple_Sclerosis]
      ,[LTC_Endometriosis]
      ,[LTC_Macular_Degeneration]
      ,[LTC_Iron_Metabolism_Disorder]
      ,[LTC_Dialysis]
      ,[RF_Alcohol_Use]
      ,[RF_Smoking]
      ,[RF_Obesity_Non_Extreme]
      ,[RF_Low_Depression]
      ,[RF_Self_Harm]
      ,[RF_Eating_Disorder]
      ,[RF_CMHI_no_Depression]
  FROM [Warehouse_IHSC].[dbo].[JL_AGEM_segmentation_activity2]