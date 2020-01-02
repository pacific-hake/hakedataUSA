REM Get pacfin catch from PACFIN_MARTS.COMPREHENSIVE_FT table, which includes research catch
SELECT   cft.PACFIN_YEAR YEAR,
         cft.FLEET_CODE FLEET,
         cft.AGENCY_CODE AGID,
         cft.PACFIN_GEAR_CODE GRID,
         cft.LANDING_DATE TDATE,
         cft.PACFIN_CATCH_AREA_CODE ARID,
         cft.PACFIN_PORT_CODE PCID,
         cft.PORT_CODE PORT,
         cft.IS_IFQ_LANDING IFQ_LANDING,
         cft.IS_OVERAGE OVERAGE,
         cft.DEALER_NUM PROC,
         cft.DAHL_GROUNDFISH_CODE DAHL_SECTOR,
         cft.FTID,
         cft.VESSEL_NUM DRVID,
         cft.COUNT_LE_PERMITS,
         cft.PARTICIPATION_GROUP_CODE,
         SUM(landed_weight_lbs) AS LBS,
         SUM(landed_weight_mtons) as MT,
         SUM(round_weight_mtons) as rMT
FROM    pacfin_marts.comprehensive_ft cft 
WHERE
        cft.PACFIN_YEAR >= &beginyr AND
        cft.PACFIN_YEAR <= &endyr AND
        cft.PACFIN_SPECIES_CODE = upper(&sp) AND
        council_code = 'P'
 GROUP BY  cft.PACFIN_YEAR,
           cft.FLEET_CODE,
           cft.AGENCY_CODE,
           cft.PACFIN_GEAR_CODE,
           cft.LANDING_DATE,
           cft.PACFIN_CATCH_AREA_CODE,
           cft.PACFIN_PORT_CODE,
           cft.FTID,
           cft.VESSEL_NUM,
           cft.COUNT_LE_PERMITS,
           cft.PORT_CODE,
           cft.IS_OVERAGE,
           cft.DEALER_NUM,
           cft.DAHL_GROUNDFISH_CODE,
           cft.PARTICIPATION_GROUP_CODE,
           cft.IS_IFQ_LANDING;
REM  cft.PRODUCT_FORM, cft.LEGAL_REMOVAL;
