SELECT 
         LANDING_YEAR as Year,
         LANDING_MONTH as Month,
         NORPAC_SPECIES_CODE,
         SECTOR,
         SUM(CATCH_WEIGHT_MTONS) as mt
FROM     PACFIN_MARTS.Comprehensive_npac
WHERE
         LANDING_YEAR >= &beginyr AND
         LANDING_YEAR <= &endyr AND
         NORPAC_SPECIES_CODE = upper(&sp)
GROUP BY LANDING_YEAR, LANDING_MONTH, NORPAC_SPECIES_CODE, SECTOR
ORDER BY SECTOR, LANDING_YEAR, LANDING_MONTH;
