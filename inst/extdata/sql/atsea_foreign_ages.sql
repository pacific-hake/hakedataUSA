SELECT FOREIGN_HAUL.*,
  FOREIGN_AGE.AGE,
  FOREIGN_AGE.SPECIMEN_NUMBER,
  FOREIGN_AGE.SEX,
  FOREIGN_AGE.LENGTH,
  FOREIGN_AGE.INDIV_WEIGHT,
  FOREIGN_AGE.MATURITY_CODE
FROM FOREIGN_HAUL
INNER JOIN FOREIGN_AGE
ON FOREIGN_HAUL.HAUL_JOIN = FOREIGN_AGE.HAUL_JOIN
WHERE 
  FOREIGN_HAUL.LATITUDE < 4900 AND
  FOREIGN_AGE.AGE IS NOT NULL AND
  FOREIGN_AGE.SPECIES = 206;