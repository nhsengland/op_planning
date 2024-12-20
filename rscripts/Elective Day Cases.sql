WITH LatestDate AS (
    SELECT 
        MAX(CAST(Admission_Date AS DATE)) AS Most_Recent_Date
    FROM 
        [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS]
    WHERE 
        Der_Financial_Year = '2024/25'
        AND LEFT(DER_provider_code, 1) = 'R'
)

SELECT 
    LEFT(APC.Der_Provider_Code, 3) AS icb_code,                        
    LEFT(APC.Der_Provider_Code, 3) AS org_code,                         
    (SELECT Most_Recent_Date FROM LatestDate) AS dimension_name,        
    'Measure' AS measure_type,                                          
    'E.M.10.A.B' AS planning_ref,                                           

    SUM(CASE WHEN Patient_Classification = 2 AND Der_Spell_LoS = 0 THEN 1 ELSE 0 END) AS metric_value_daycase

FROM 
    [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] AS APC
LEFT OUTER JOIN 
    [NHSE_Reference].[dbo].[vw_Ref_ODS_Provider_Hierarchies] AS ODS 
    ON LEFT(APC.Der_Provider_Code, 3) = ODS.Organisation_Code 
    AND Effective_To IS NULL

WHERE 
    Der_Financial_Year = '2024/25'
    AND Patient_Classification = 2 -- Elective day case spells
    AND Der_Spell_LoS = 0 -- Day cases must have zero length of stay
    AND ODS.Region_Code = 'y59'
    AND ODS.NHSE_Organisation_Type LIKE 'Acute%'
    AND LEFT(DER_provider_code, 1) = 'R'
    AND Admission_Method IN ('11', '12', '13')

GROUP BY 
    LEFT(APC.Der_Provider_Code, 3), 
    ODS.Organisation_Name;
