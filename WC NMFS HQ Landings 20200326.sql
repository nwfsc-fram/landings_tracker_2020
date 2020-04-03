--********************************************************************************
--Script: NMFSHQ_WOC_20200324
--Created by: Camille Kohler, neXus
--Created on: Mar 24, 2020
--Purpose:    Provide a non-confidential report of landings by year, month, agency, and species group
--            to NMFS HQ to provide a baseline of data for 2014 and later that will allow for the analysis
--            of possible effects on fishery revenue due to the coronavirus.
--Notes:      1) There is drawback of using just management group...may need to include complex for conf species group.
--            2) removed un-priced records from Comp norpac and comp ft...this eliminates at-sea non-whiting
--            3) Added fisher license num count for shrimp where vessel is NULL
--********************************************************************************

--Detail View
CREATE OR REPLACE VIEW nmfshq_landings_woc_detail_v AS
    WITH speciesgroup AS (
        SELECT
            psp.pacfin_species_code,
            psp.pacfin_species_common_name,
            psp.management_group_code,
            CASE
                WHEN pacfin_species_common_name like '%TUNA%' OR pacfin_species_common_name = 'ALBACORE' THEN 'TUNA'
                WHEN management_group_code = 'GRND'             THEN CASE
                    WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                    ELSE 'NON-WHITING GROUNDFISH'
                END
                WHEN management_group_code = 'CPEL'             THEN 
                     CASE WHEN pacfin_species_code IN ('CMCK','JMCK','MSQD','NANC','PSDN','UMCK') THEN 'COASTAL PELAGIC'
                          ELSE 'OTHER'
                          END --CDK - Changind CPEL to Coastal Pelagic from Other and included only species specified by Steve Stohs as CPS
                WHEN management_group_code = 'HMSP'             THEN 'OTHER'
                WHEN management_group_code = 'SAMN'             THEN 'SALMON'
                WHEN management_group_code = 'CRAB'             THEN 'CRAB'
                WHEN management_group_code = 'SRMP'             THEN 'SHRIMP'
                WHEN management_group_code = 'SHLL'             THEN 'SHELLFISH'
                WHEN nvl(management_group_code,'OTHR') = 'OTHR' THEN 'OTHER'
                ELSE 'CHECK'
            END species_group
        FROM
            pacfin_foundation.pacfin_species_codes psp
    ),merge_data AS (
 --Pull FT data
        SELECT
            landing_year,
            landing_month,
            agency_code,
            CASE
                WHEN ft.management_group_code IN (
                    'SHLL',
                    'SRMP'
                ) THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                ELSE TO_CHAR(vessel_id)
            END AS vessel_num,
            agency_code || dealer_id AS dealer_num,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END AS conf_group,
            spg.species_group,
            SUM(round_weight_mtons) AS round_weight_mtons,
            SUM(exvessel_revenue) AS exvessel_revenue
        FROM
            pacfin_marts.comprehensive_ft ft
            LEFT JOIN speciesgroup spg ON ft.pacfin_species_code = spg.pacfin_species_code
        WHERE
            participation_group_code IN (
                'C',
                'I'
            )
 /* Excluding ETIX, only want "hard" data */
            AND is_etix_data = 'F'
 /* Excluding Roe as in original script */
            AND condition_code != 'E'
            AND council_code IN (
                'P',
                '*'
            )
            AND inpfc_area_type_code NOT IN (
                'CT',
                'VC',
                'GS'
            )
            AND landing_year >= 2014 
            AND exvessel_revenue > 0
        GROUP BY
            landing_year,
            landing_month,
            agency_code,
            CASE
                WHEN ft.management_group_code IN (
                    'SHLL',
                    'SRMP'
                ) THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                ELSE TO_CHAR(vessel_id)
            END,
            agency_code || dealer_id,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END,
            spg.species_group
        UNION ALL 
 --Pull NORPAC Data 
        SELECT
            landing_year,
            landing_month,
            'F' AS agency_code, 
            TO_CHAR(nvl2(cnp.catcher_vessel_id,cnp.catcher_vessel_id,cnp.processor_vessel_id) ) AS vessel_num,
            TO_CHAR(cnp.processor_vessel_id) AS dealer_num,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END AS conf_group,
            spg.species_group,
            SUM(cnp.retained_weight_mtons) AS round_weight_mtons,
            SUM(cnp.exvessel_revenue) AS exvessel_revenue
        FROM
            pacfin_marts.comprehensive_npac cnp
            LEFT JOIN speciesgroup spg ON cnp.pacfin_species_code = spg.pacfin_species_code
        WHERE
            cnp.pacfin_species_code != 'XXXX'
            AND cnp.retained_weight_lbs != 0
            AND cnp.landing_year >= 2014
            AND cnp.exvessel_revenue > 0
        GROUP BY
            landing_year,
            landing_month,
            nvl2(cnp.catcher_vessel_id,cnp.catcher_vessel_id,cnp.processor_vessel_id),
            cnp.processor_vessel_id,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END,
            spg.species_group
    ),specgrp_count AS (--Get counts by year, month, agency, species group and flag for confidentiality
        SELECT
            landing_year,
            landing_month,
            agency_code,
            species_group,
            COUNT(DISTINCT vessel_num) AS vessel_count,
            COUNT(DISTINCT dealer_num) AS dealer_count,
            CASE
                WHEN COUNT(DISTINCT vessel_num) < 3
                     OR COUNT(DISTINCT dealer_num) < 3 THEN 1
                ELSE 0
            END conf_flag
        FROM
            merge_data
        GROUP BY
            landing_year,
            landing_month,
            agency_code,
            species_group
    ),confgrp_count AS (--Get counts by confidential group (other/shellfish) where year, month, agency, species group is confidential
        SELECT
            landing_year,
            landing_month,
            agency_code,
            conf_group,
            COUNT(DISTINCT vessel_num) AS vessel_count,
            COUNT(DISTINCT dealer_num) AS dealer_count,
            CASE
                WHEN COUNT(DISTINCT vessel_num) < 3
                     OR COUNT(DISTINCT dealer_num) < 3 THEN 1
                ELSE 0
            END conf_flag
        FROM
            merge_data md
        WHERE
            NOT EXISTS (
                SELECT
                    1
                FROM
                    specgrp_count sc
                WHERE
                    md.landing_year = sc.landing_year
                    AND md.landing_month = sc.landing_month
                    AND md.agency_code = sc.agency_code
                    AND md.species_group = sc.species_group
                    AND sc.conf_flag = 0
            )
        GROUP BY
            landing_year,
            landing_month,
            agency_code,
            conf_group
    ),allagency_count AS (--Get counts for all agencies by year, month where species group and confidential counts are confidential
        SELECT
            landing_year,
            landing_month,
            conf_group,
            COUNT(DISTINCT vessel_num) AS vessel_count,
            COUNT(DISTINCT dealer_num) AS dealer_count
        FROM
            merge_data md
        WHERE
            NOT EXISTS (
                SELECT
                    1
                FROM
                    specgrp_count sc
                WHERE
                    md.landing_year = sc.landing_year
                    AND md.landing_month = sc.landing_month
                    AND md.agency_code = sc.agency_code
                    AND md.species_group = sc.species_group
                    AND sc.conf_flag = 0
            )
                AND NOT EXISTS (
                SELECT
                    1
                FROM
                    confgrp_count cf
                WHERE
                    md.landing_year = cf.landing_year
                    AND md.landing_month = cf.landing_month
                    AND md.agency_code = cf.agency_code
                    AND md.conf_group = cf.conf_group
                    AND cf.conf_flag = 0
            )
        GROUP BY
            landing_year,
            landing_month,
            conf_group
    )
    SELECT
        md.landing_year,
        md.landing_month,
        md.agency_code,
        md.conf_group,
        md.species_group,
        sp_conf.vessel_count    sp_vess,
        sp_conf.dealer_count    sp_deal,
        grp_conf.vessel_count   grp_vess,
        grp_conf.dealer_count   grp_deal,
        ag_conf.vessel_count    ag_vess,
        ag_conf.dealer_count    ag_deal,
       CASE WHEN sp_conf.vessel_count >= 3
                 AND sp_conf.dealer_count >= 3 THEN 0
            WHEN grp_conf.vessel_count >= 3
                 AND grp_conf.dealer_count >= 3 THEN 0
            ELSE 1
        END AS conf_flag,
        CASE --Automatically group CPS coastwide for confidential reporting
             WHEN md.species_group = 'COASTAL PELAGIC' THEN 'WOC'
             --Automatically group W shrimp with other for confidential reporting
             WHEN md.agency_code = 'W' AND md.species_group = 'SHRIMP' AND md.landing_month IN (1, 2, 3, 11, 12) THEN 'W'
             --Automatically group WOF whiting for confidential reporting
             WHEN md.agency_code in ('W','O','F') AND md.species_group = 'WHITING' THEN 'WO'
             --Automatically roll C whiting into other for confidential reporting
             WHEN md.agency_code in ('C') AND md.species_group = 'WHITING' THEN 'C'
             WHEN sp_conf.vessel_count >= 3
                 AND sp_conf.dealer_count >= 3 THEN md.agency_code
             WHEN grp_conf.vessel_count >= 3
                 AND grp_conf.dealer_count >= 3 THEN md.agency_code
            ELSE 'WOC'
        END AS conf_agency_code,
        CASE --Automatically group CPS coastwide for confidential reporting
             WHEN md.species_group = 'COASTAL PELAGIC' THEN 'COASTAL PELAGIC'
             --Automatically group W shrimp with other for confidential reporting
             WHEN md.agency_code = 'W' AND md.species_group = 'SHRIMP' AND md.landing_month IN (1, 2, 3, 11, 12) THEN 'OTHER'
             --Automatically group WOF whiting for confidential reporting
             WHEN md.agency_code in ('W','O','F') AND md.species_group = 'WHITING' THEN 'WHITING'
             --Automatically roll C whiting into other for confidential reporting
             WHEN md.agency_code in ('C') AND md.species_group = 'WHITING' THEN 'OTHER'
             WHEN sp_conf.vessel_count >= 3
                  AND sp_conf.dealer_count >= 3 THEN md.species_group
             WHEN grp_conf.vessel_count >= 3
                  AND grp_conf.dealer_count >= 3 THEN md.conf_group
             ELSE '***CONFIDENTIAL '
                  || md.conf_group
                  || '***'
        END AS conf_species_group,
        SUM(md.round_weight_mtons) AS round_weight_mtons,
        SUM(md.exvessel_revenue) AS exvessel_revenue
    FROM
        merge_data md
        LEFT JOIN specgrp_count sp_conf ON md.landing_year = sp_conf.landing_year
                                           AND md.landing_month = sp_conf.landing_month
                                           AND md.agency_code = sp_conf.agency_code
                                           AND md.species_group = sp_conf.species_group
        LEFT JOIN confgrp_count grp_conf ON md.landing_year = grp_conf.landing_year
                                            AND md.landing_month = grp_conf.landing_month
                                            AND md.agency_code = grp_conf.agency_code
                                            AND md.conf_group = grp_conf.conf_group
        LEFT JOIN allagency_count ag_conf ON md.landing_year = ag_conf.landing_year
                                             AND md.landing_month = ag_conf.landing_month
                                             AND md.conf_group = ag_conf.conf_group
    GROUP BY
        md.landing_year,
        md.landing_month,
        md.agency_code,
        md.conf_group,
        md.species_group,
        sp_conf.vessel_count,
        sp_conf.dealer_count,
        grp_conf.vessel_count,
        grp_conf.dealer_count,
        ag_conf.vessel_count,
        ag_conf.dealer_count,
        CASE
            WHEN sp_conf.vessel_count >= 3
                 AND sp_conf.dealer_count >= 3 THEN 0
            WHEN grp_conf.vessel_count >= 3
                 AND grp_conf.dealer_count >= 3 THEN 0
            ELSE 1
        END,
        CASE --Automatically group CPS coastwide for confidential reporting
             WHEN md.species_group = 'COASTAL PELAGIC' THEN 'WOC'
             --Automatically group W shrimp in winter months with other for confidential reporting
             WHEN md.agency_code = 'W' AND md.species_group = 'SHRIMP' AND md.landing_month IN (1, 2, 3, 11, 12) THEN 'W'
             --Automatically group WOF whiting for confidential reporting
             WHEN md.agency_code in ('W','O','F') AND md.species_group = 'WHITING' THEN 'WO'
             --Automatically roll C whiting into other for confidential reporting
             WHEN md.agency_code in ('C') AND md.species_group = 'WHITING' THEN 'C'
             WHEN sp_conf.vessel_count >= 3
                 AND sp_conf.dealer_count >= 3 THEN md.agency_code
             WHEN grp_conf.vessel_count >= 3
                 AND grp_conf.dealer_count >= 3 THEN md.agency_code
            ELSE 'WOC'
        END,
        CASE --Automatically group CPS coastwide for confidential reporting
             WHEN md.species_group = 'COASTAL PELAGIC' THEN 'COASTAL PELAGIC'
             --Automatically group W shrimp in winter months with other for confidential reporting
             WHEN md.agency_code = 'W' AND md.species_group = 'SHRIMP' AND md.landing_month IN (1, 2, 3, 11, 12) THEN 'OTHER'
             --Automatically group WOF whiting for confidential reporting
             WHEN md.agency_code in ('W','O','F') AND md.species_group = 'WHITING' THEN 'WHITING'
             --Automatically roll C whiting into other for confidential reporting
             WHEN md.agency_code in ('C') AND md.species_group = 'WHITING' THEN 'OTHER'
             WHEN sp_conf.vessel_count >= 3
                  AND sp_conf.dealer_count >= 3 THEN md.species_group
             WHEN grp_conf.vessel_count >= 3
                  AND grp_conf.dealer_count >= 3 THEN md.conf_group
             ELSE '***CONFIDENTIAL '
                  || md.conf_group
                  || '***'
        END;

--Final view
CREATE OR REPLACE VIEW nmfshq_landings_woc_v AS
    SELECT
        landing_year,
        landing_month,
        conf_agency_code,
        conf_species_group,
        round(SUM(round_weight_mtons),2) AS round_weight_mtons,
        round(SUM(exvessel_revenue) ) AS exvessel_revenue
    FROM
        nmfshq_landings_woc_detail_v
    GROUP BY
        landing_year,
        landing_month,
        conf_agency_code,
        conf_species_group
    ORDER BY
        1,
        2,
        3,
        4;

--*****VALIDATIONS*****

--Check FT totals
WITH src AS (
    SELECT
        pacfin_year,
        SUM(round_weight_mtons) AS mtons,
        SUM(exvessel_revenue) AS rev
    FROM
        pacfin_marts.comprehensive_ft ft
    WHERE
        pacfin_year >= 2014
        AND participation_group_code IN (
            'C',
            'I'
        )
 /* Excluding ETIX, only want "hard" data */
        AND is_etix_data = 'F'
 /* Excluding Roe as in original script */
        AND condition_code != 'E'
        AND council_code IN (
            'P',
            '*'
        )
        AND inpfc_area_type_code NOT IN (
            'CT',
            'VC',
            'GS'
        )
        AND landing_year >= 2014
        AND exvessel_revenue > 0
    GROUP BY
        pacfin_year
),vw AS (
    SELECT
        landing_year,
        SUM(round_weight_mtons) mtons,
        SUM(exvessel_revenue) rev
    FROM
        nmfshq_landings_woc_detail_v
    WHERE
        agency_code <> 'F'
    GROUP BY
        landing_year
)
SELECT
    src.*,
    vw.*
FROM
    src
    FULL OUTER JOIN vw ON src.pacfin_year = vw.landing_year;

--Check Npac Totals
WITH src AS (
    SELECT
        landing_year,
        SUM(cnp.retained_weight_mtons) AS mtons,
        SUM(cnp.exvessel_revenue) AS rev
    FROM
        pacfin_marts.comprehensive_npac cnp
    WHERE
        cnp.pacfin_species_code != 'XXXX'
        AND cnp.retained_weight_lbs != 0
        AND cnp.landing_year >= 2014
        AND cnp.exvessel_revenue > 0
    GROUP BY
        landing_year
),vw AS (
    SELECT
        landing_year,
        SUM(round_weight_mtons) mtons,
        SUM(exvessel_revenue) rev
    FROM
        nmfshq_landings_woc_detail_v
    WHERE
        agency_code = 'F'
    GROUP BY
        landing_year
)
SELECT
    src.*,
    vw.*
FROM
    src
    FULL OUTER JOIN vw ON src.landing_year = vw.landing_year;

--Check final view totals...a little off because of rounding
WITH src_merge AS (
    SELECT
        pacfin_year,
        SUM(round_weight_mtons) AS mtons,
        SUM(exvessel_revenue) AS rev
    FROM
        pacfin_marts.comprehensive_ft ft
    WHERE
        pacfin_year >= 2014
        AND participation_group_code IN (
            'C',
            'I'
        )
 /* Excluding ETIX, only want "hard" data */
        AND is_etix_data = 'F'
 /* Excluding Roe as in original script */
        AND condition_code != 'E'
        AND council_code IN (
            'P',
            '*'
        )
        AND inpfc_area_type_code NOT IN (
            'CT',
            'VC',
            'GS'
        )
        AND landing_year > = 2014
        AND exvessel_revenue > 0
    GROUP BY
        pacfin_year
    UNION ALL
    SELECT
        landing_year,
        SUM(cnp.retained_weight_mtons) AS mtons,
        SUM(cnp.exvessel_revenue) AS rev
    FROM
        pacfin_marts.comprehensive_npac cnp
    WHERE
        cnp.pacfin_species_code != 'XXXX'
        AND cnp.retained_weight_lbs != 0
        AND cnp.landing_year >= 2014
        AND cnp.exvessel_revenue > 0
    GROUP BY
        landing_year
),src AS (
    SELECT
        pacfin_year,
        SUM(mtons) mtons,
        SUM(rev) rev
    FROM
        src_merge
    GROUP BY
        pacfin_year
),vw AS (
    SELECT
        landing_year,
        SUM(round_weight_mtons) mtons,
        SUM(exvessel_revenue) rev
    FROM
        nmfshq_landings_woc_v
    GROUP BY
        landing_year
)
SELECT
    src.*,
    vw.*
FROM
    src
    FULL OUTER JOIN vw ON src.pacfin_year = vw.landing_year;

--Select for workbook

SELECT
    *
FROM
    nmfshq_landings_woc_detail_v
ORDER BY
    1,
    2,
    3,
    5;

SELECT
    *
FROM
    nmfshq_landings_woc_detail_v
    where species_group = 'WHITING'
    AND AGENCY_CODE <> 'C'
ORDER BY
    1,
    2,
    3,
    5;

SELECT
    *
FROM
    nmfshq_landings_woc_v
ORDER BY
    1,
    2,
    3,
    4;

--Get Table Layouts
SELECT
    column_id,
    column_name,
    data_type,
    data_length
FROM
    all_tab_columns
WHERE
    owner = 'PACFIN_MARTS'
    AND table_name = 'NMFSHQ_LANDINGS_WOC_V'
ORDER BY
    1;

--Get Species Translations
SELECT
    psp.pacfin_species_code,
    psp.pacfin_species_common_name,
    psp.management_group_code,
    CASE
        WHEN management_group_code = 'GRND'             THEN CASE
            WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
            ELSE 'NON-WHITING GROUNDFISH'
        END
        WHEN management_group_code = 'CPEL'             THEN 'OTHER'
        WHEN management_group_code = 'HMSP'             THEN 'OTHER'
        WHEN management_group_code = 'SAMN'             THEN 'SALMON'
        WHEN management_group_code = 'CRAB'             THEN 'CRAB'
        WHEN management_group_code = 'SRMP'             THEN 'SHRIMP'
        WHEN management_group_code = 'SHLL'             THEN 'SHELLFISH'
        WHEN nvl(management_group_code,'OTHR') = 'OTHR' THEN 'OTHER'
        ELSE 'CHECK'
    END species_group
FROM
    pacfin_foundation.pacfin_species_codes psp;
    
SELECT * FROM nmfshq_landings_woc_detail_v 
--WHERE  CONF_SPECIES_GROUP LIKE '***%SHELL%';
--WHERE SPECIES_GROUP = 'COASTAL PELAGIC'
WHERE CONF_GROUP = 'OTHER'
ORDER BY 1, 2, 3;

SELECT * FROM nmfshq_landings_woc_v 
WHERE  CONF_SPECIES_GROUP LIKE '***%SHELL%';

--Query on Merge data with F/W combined
    
        WITH speciesgroup AS (
        SELECT
            psp.pacfin_species_code,
            psp.pacfin_species_common_name,
            psp.management_group_code,
            CASE
                WHEN management_group_code = 'GRND'             THEN CASE
                    WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                    ELSE 'NON-WHITING GROUNDFISH'
                END
                WHEN management_group_code = 'CPEL'             THEN 'COASTAL PELAGIC'
                WHEN management_group_code = 'HMSP'             THEN 'OTHER'
                WHEN management_group_code = 'SAMN'             THEN 'SALMON'
                WHEN management_group_code = 'CRAB'             THEN 'CRAB'
                WHEN management_group_code = 'SRMP'             THEN 'SHRIMP'
                WHEN management_group_code = 'SHLL'             THEN 'SHELLFISH'
                WHEN nvl(management_group_code,'OTHR') = 'OTHR' THEN 'OTHER'
                ELSE 'CHECK'
            END species_group
        FROM
            pacfin_foundation.pacfin_species_codes psp
    ),
    merge_data AS (
 --Pull FT data
        SELECT
            landing_year,
            landing_month,
            agency_code,
            CASE
                WHEN ft.management_group_code IN (
                    'SHLL',
                    'SRMP'
                ) THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                ELSE TO_CHAR(vessel_id)
            END AS vessel_num,
            agency_code || dealer_id AS dealer_num,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END AS conf_group,
            spg.species_group,
            SUM(round_weight_mtons) AS round_weight_mtons,
            SUM(exvessel_revenue) AS exvessel_revenue
        FROM
            pacfin_marts.comprehensive_ft ft
            LEFT JOIN speciesgroup spg ON ft.pacfin_species_code = spg.pacfin_species_code
        WHERE
            participation_group_code IN (
                'C',
                'I'
            )
 /* Excluding ETIX, only want "hard" data */
            AND is_etix_data = 'F'
 /* Excluding Roe as in original script */
            AND condition_code != 'E'
            AND council_code IN (
                'P',
                '*'
            )
            AND inpfc_area_type_code NOT IN (
                'CT',
                'VC',
                'GS'
            )
            AND landing_year >= 2014
            AND exvessel_revenue > 0
        GROUP BY
            landing_year,
            landing_month,
            agency_code,
            CASE
                WHEN ft.management_group_code IN (
                    'SHLL',
                    'SRMP'
                ) THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                ELSE TO_CHAR(vessel_id)
            END,
            agency_code || dealer_id,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END,
            spg.species_group
        UNION ALL 
 --Pull NORPAC Data 
        SELECT
            landing_year,
            landing_month,
            'W' AS agency_code,
            TO_CHAR(nvl2(cnp.catcher_vessel_id,cnp.catcher_vessel_id,cnp.processor_vessel_id) ) AS vessel_num,
            TO_CHAR(cnp.processor_vessel_id) AS dealer_num,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END AS conf_group,
            spg.species_group,
            SUM(cnp.retained_weight_mtons) AS round_weight_mtons,
            SUM(cnp.exvessel_revenue) AS exvessel_revenue
        FROM
            pacfin_marts.comprehensive_npac cnp
            LEFT JOIN speciesgroup spg ON cnp.pacfin_species_code = spg.pacfin_species_code
        WHERE
            cnp.pacfin_species_code != 'XXXX'
            AND cnp.retained_weight_lbs != 0
            AND cnp.landing_year >= 2014
            AND cnp.exvessel_revenue > 0
        GROUP BY
            landing_year,
            landing_month,
            nvl2(cnp.catcher_vessel_id,cnp.catcher_vessel_id,cnp.processor_vessel_id),
            cnp.processor_vessel_id,
            CASE
                WHEN spg.management_group_code IN (
                    'CRAB',
                    'SHLL',
                    'SRMP'
                ) THEN 'SHELLFISH'
                ELSE 'OTHER'
            END,
            spg.species_group
    )
    select * from merge_data
    where  landing_year = 2016
    and     species_group like 'COASTAL PELAGIC'
    and    landing_month = 6
    and agency_code = 'O';
    
    