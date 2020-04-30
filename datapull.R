library(dplyr)
# Insert database connection information here #
library(odbc)
library(getPass)

pacfin <- DBI::dbConnect(odbc::odbc(),
  host   = "pacfindb.psmfc.org",
  UID    = getPass('pacfin username'),
  PWD    = getPass('pacfin password'),
  dsn    = 'pacfin',
  port   = 2045)

preventingprintingtoconsole <- dbSendQuery(pacfin, "alter session set current_schema=PACFIN_MARTS")

pacfin_dat_raw <- dbGetQuery(pacfin, "select landing_year,
                                      landing_month,
                                      landing_date,
                                      agency_code,
                                      ticket_source_code,
                                      CASE WHEN
                                        management_group_code IN ('SHLL','SRMP') 
                                        THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                                        ELSE TO_CHAR(vessel_id) 
                                        END AS vessel_num,
                                      agency_code || dealer_id AS dealer_num,
                         -- Formatting species groups
                                      CASE WHEN
                                      inpfc_area_type_code = 'PG' THEN 'PUGET SOUND FISHERIES'
                                      WHEN pacfin_species_common_name like '%TUNA%' OR pacfin_species_common_name = 'ALBACORE' 
                                        THEN 'TUNA'
                                        WHEN management_group_code = 'GRND' THEN 
                                         CASE WHEN IS_IFQ_LANDING = 'T' THEN 
                                            CASE WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                                            WHEN PACFIN_GEAR_CODE = 'MDT' THEN 'MIDWATER'
                                            WHEN DAHL_GROUNDFISH_CODE = '14' THEN 'MIDWATER'
                                              ELSE 'NON-WHITING GROUNDFISH IFQ' END
                                         WHEN DAHL_GROUNDFISH_CODE IN ('05','06') THEN 'NEARSHORE GROUNDFISH'
                                         WHEN DAHL_GROUNDFISH_CODE IN ('07','08','09','10','20') THEN 'OFFSHORE GROUNDFISH'
                                         WHEN DAHL_GROUNDFISH_CODE IN ('11','12','13','14') THEN 'OTHER'
                                         ELSE 'NON-WHITING GROUNDFISH NON-IFQ' END 
                                        WHEN 
                                        management_group_code = 'CPEL'
                                        THEN CASE WHEN 
                                                pacfin_species_code = 'PSDN' THEN 'SARDINE'
                                                WHEN
                                                pacfin_species_code = 'NANC' THEN 'ANCHOVY'
                                                WHEN
                                                pacfin_species_code = 'MSQD' THEN 'MARKET SQUID'
                                                WHEN
                                                pacfin_species_code IN ('CMCK','JMCK','UMCK') THEN 'OTHER COASTAL PELAGIC'
                                                ELSE 'OTHER'
                                                END 
                                        WHEN
                                        management_group_code = 'CRAB'
                                        THEN CASE WHEN 
                                        pacfin_species_code = 'DCRB' THEN 'DUNGENESS CRAB'
                                        ELSE 'OTHER CRAB'
                                        END
                           -- CDK - Changind CPEL to Coastal Pelagic from Other and included only species specified by Steve Stohs as CPS
                                        WHEN management_group_code = 'HMSP'             
                                        THEN 'OTHER'
                                        WHEN management_group_code = 'SAMN'             
                                        THEN 'SALMON'
                                        WHEN management_group_code = 'SRMP'             
                                        THEN 'SHRIMP'
                                        WHEN management_group_code = 'SHLL'             
                                        THEN 'SHELLFISH'
                                        WHEN nvl(management_group_code,'OTHR') = 'OTHR' 
                                        THEN 'OTHER'
                                        ELSE 'CHECK'
                                        END species_group,
                                      SUM(round_weight_mtons) AS round_weight_mtons,
                                      SUM(exvessel_revenue) AS exvessel_revenue
                                      FROM comprehensive_ft
                                          WHERE
                                          participation_group_code in ('C')
                            -- Excluding ETIX, only want hard data --
                                         -- AND is_etix_data = 'F'
                            -- Excluding Roe as in original script --
                                          AND condition_code != 'E'
                                          AND council_code IN ('P','*')
                                          AND inpfc_area_type_code NOT IN ('CT','VC','GS')
                                          AND landing_year >= 2014 
                                          AND exvessel_revenue > 0
                            -- Excluding tribal -- 
                                          AND FLEET_CODE <> 'TI'
                                     GROUP BY
                                        landing_year,
                                        landing_month,
                                        landing_date,
                                        agency_code,
                                        ticket_source_code,

                                        CASE WHEN 
                                          management_group_code IN ('SHLL','SRMP') 
                                          THEN nvl(TO_CHAR(vessel_id),fisher_license_num)
                                          ELSE TO_CHAR(vessel_id)
                                          END,
                                        agency_code || dealer_id,
                                        CASE WHEN 
                                        inpfc_area_type_code = 'PG' THEN 'PUGET SOUND FISHERIES'
                                      WHEN pacfin_species_common_name like '%TUNA%' OR pacfin_species_common_name = 'ALBACORE' 
                                        THEN 'TUNA'
                                        WHEN management_group_code = 'GRND' THEN 
                                         CASE WHEN IS_IFQ_LANDING = 'T' THEN 
                                            CASE WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                                            WHEN PACFIN_GEAR_CODE = 'MDT' THEN 'MIDWATER'
                                            WHEN DAHL_GROUNDFISH_CODE = '14' THEN 'MIDWATER'
                                              ELSE 'NON-WHITING GROUNDFISH IFQ' END
                                         WHEN DAHL_GROUNDFISH_CODE IN ('05','06') THEN 'NEARSHORE GROUNDFISH'
                                         WHEN DAHL_GROUNDFISH_CODE IN ('07','08','09','10','20') THEN 'OFFSHORE GROUNDFISH'
                                         WHEN DAHL_GROUNDFISH_CODE IN ('11','12','13','14') THEN 'OTHER'
                                         ELSE 'NON-WHITING GROUNDFISH NON-IFQ' END 
                                         WHEN 
                                        management_group_code = 'CPEL'
                                        THEN CASE WHEN 
                                                pacfin_species_code = 'PSDN' THEN 'SARDINE'
                                                WHEN
                                                pacfin_species_code = 'NANC' THEN 'ANCHOVY'
                                                WHEN
                                                pacfin_species_code = 'MSQD' THEN 'MARKET SQUID'
                                                WHEN
                                                pacfin_species_code IN ('CMCK','JMCK','UMCK') THEN 'OTHER COASTAL PELAGIC'
                                                ELSE 'OTHER'
                                                END 
                                        WHEN
                                        management_group_code = 'CRAB'
                                        THEN CASE WHEN 
                                        pacfin_species_code = 'DCRB' THEN 'DUNGENESS CRAB'
                                        ELSE 'OTHER CRAB'
                                        END
                           -- CDK - Changind CPEL to Coastal Pelagic from Other and included only species specified by Steve Stohs as CPS
                                        WHEN management_group_code = 'HMSP'             
                                        THEN 'OTHER'
                                        WHEN management_group_code = 'SAMN'             
                                        THEN 'SALMON'
                                        WHEN management_group_code = 'SRMP'             
                                        THEN 'SHRIMP'
                                        WHEN management_group_code = 'SHLL'             
                                        THEN 'SHELLFISH'
                                        WHEN nvl(management_group_code,'OTHR') = 'OTHR' 
                                        THEN 'OTHER'
                                        ELSE 'CHECK'
                                        END")

norpac_dat_raw <- dbGetQuery(pacfin, "SELECT landing_year,
                                      landing_month,
                                      landing_day,
                                      'F' AS agency_code, 
                                      TO_CHAR(nvl2(catcher_vessel_id,catcher_vessel_id, processor_vessel_id)) AS vessel_num,
                                      TO_CHAR(processor_vessel_id) AS dealer_num,
                          -- Formatting species groups
                                      CASE WHEN 
                                        pacfin_species_common_name like '%TUNA%' OR pacfin_species_common_name = 'ALBACORE' 
                                        THEN 'TUNA'
                                        WHEN management_group_code = 'GRND' THEN 
                                            CASE WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                                              ELSE 'NON-WHITING GROUNDFISH IFQ' END
                                        WHEN 
                                        management_group_code = 'CPEL'
                                        THEN CASE WHEN 
                                                pacfin_species_code = 'PSDN' THEN 'SARDINE'
                                                WHEN
                                                pacfin_species_code = 'NANC' THEN 'ANCHOVY'
                                                WHEN
                                                pacfin_species_code = 'MSQD' THEN 'MARKET SQUID'
                                                WHEN
                                                pacfin_species_code IN ('CMCK','JMCK','UMCK') THEN 'OTHER COASTAL PELAGIC'
                                                ELSE 'OTHER'
                                                END 
                                        WHEN
                                        management_group_code = 'CRAB'
                                        THEN CASE WHEN 
                                        pacfin_species_code = 'DCRB' THEN 'DUNGENESS CRAB'
                                        ELSE 'OTHER CRAB'
                                        END
                           -- CDK - Changind CPEL to Coastal Pelagic from Other and included only species specified by Steve Stohs as CPS
                                        WHEN management_group_code = 'HMSP'             
                                        THEN 'OTHER'
                                        WHEN management_group_code = 'SAMN'             
                                        THEN 'SALMON'
                                        WHEN management_group_code = 'SRMP'             
                                        THEN 'SHRIMP'
                                        WHEN management_group_code = 'SHLL'             
                                        THEN 'SHELLFISH'
                                        WHEN nvl(management_group_code,'OTHR') = 'OTHR' 
                                        THEN 'OTHER'
                                        ELSE 'CHECK'
                                        END species_group,
                                   SUM(retained_weight_mtons) AS round_weight_mtons,
                                   SUM(exvessel_revenue) AS exvessel_revenue
                                   FROM pacfin_marts.comprehensive_npac
                                   WHERE pacfin_species_code != 'XXXX' AND retained_weight_lbs != 0 AND landing_year >= 2014 AND exvessel_revenue > 0
                                     and sector <> 'TRIBAL'
                                   GROUP BY landing_year, landing_month, landing_day, nvl2(catcher_vessel_id,catcher_vessel_id,processor_vessel_id),
                                   processor_vessel_id,
                                   -- Formatting species groups --
                                      CASE WHEN 
                                        pacfin_species_common_name like '%TUNA%' OR pacfin_species_common_name = 'ALBACORE' 
                                        THEN 'TUNA'
                                        WHEN management_group_code = 'GRND' THEN
                                            CASE WHEN pacfin_species_code = 'PWHT' THEN 'WHITING'
                                              ELSE 'NON-WHITING GROUNDFISH IFQ' END
                                        WHEN 
                                        management_group_code = 'CPEL'
                                        THEN CASE WHEN 
                                                pacfin_species_code = 'PSDN' THEN 'SARDINE'
                                                WHEN
                                                pacfin_species_code = 'NANC' THEN 'ANCHOVY'
                                                WHEN
                                                pacfin_species_code = 'MSQD' THEN 'MARKET SQUID'
                                                WHEN
                                                pacfin_species_code IN ('CMCK','JMCK','UMCK') THEN 'OTHER COASTAL PELAGIC'
                                                ELSE 'OTHER'
                                                END 
                                        WHEN
                                        management_group_code = 'CRAB'
                                        THEN CASE WHEN 
                                        pacfin_species_code = 'DCRB' THEN 'DUNGENESS CRAB'
                                        ELSE 'OTHER CRAB'
                                        END
                           -- CDK - Changind CPEL to Coastal Pelagic from Other and included only species specified by Steve Stohs as CPS
                                        WHEN management_group_code = 'HMSP'             
                                        THEN 'OTHER'
                                        WHEN management_group_code = 'SAMN'             
                                        THEN 'SALMON'
                                        WHEN management_group_code = 'SRMP'             
                                        THEN 'SHRIMP'
                                        WHEN management_group_code = 'SHLL'             
                                        THEN 'SHELLFISH'
                                        WHEN nvl(management_group_code,'OTHR') = 'OTHR' 
                                        THEN 'OTHER'
                                        ELSE 'CHECK'
                                        END") %>%
        mutate(LANDING_DATE = as.POSIXct(paste0(LANDING_YEAR, "-", LANDING_MONTH,"-", LANDING_DAY),"%Y-%m-%d")) %>%
        select(-LANDING_DAY)

comp_dat_raw <- rbind(pacfin_dat_raw, 
                      norpac_dat_raw %>%
                        mutate(TICKET_SOURCE_CODE = NA_character_)) %>%
        data.frame()
saveRDS(comp_dat_raw, paste0("comp_dat_raw", Sys.Date(), ".RDS"))

saveRDS(comp_dat_raw, "comp_dat_raw.RDS")
