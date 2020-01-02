REM ***********************************************************************
REM
REM  Queries obsint tables for calculating domestic catch.  From Vanessa
REM  Tuttle, modified by Andi Stephens, used by Allan Hicks in 2011
REM
REM Additional columns added for location
REM
REM ***********************************************************************
REM
REM  Removed these lines, which do not work as intended.
REM
REM  obsint.DEBRIEFED_HAUL.HAUL > 0 and
REM  obsint.DEBRIEFED_SPCOMP.SPECIES = &sp;
REM
REM ***********************************************************************

SELECT
  to_char(obsint.DEBRIEFED_HAUL.HAUL_JOIN),
  obsint.DEBRIEFED_HAUL.CRUISE,
  obsint.DEBRIEFED_HAUL.PERMIT,
  obsint.DEBRIEFED_HAUL.VESSEL,
  obsint.DEBRIEFED_HAUL.VESSEL_TYPE,
  obsint.DEBRIEFED_HAUL.HAUL_DATE,
  obsint.DEBRIEFED_HAUL.HAUL,
  obsint.DEBRIEFED_HAUL.DEPLOYMENT_DATE,
  obsint.DEBRIEFED_HAUL.RETRIEVAL_DATE,
  obsint.DEBRIEFED_HAUL.DURATION_IN_MIN,
  obsint.DEBRIEFED_HAUL.CDQ_CODE,
  obsint.DEBRIEFED_HAUL.OFFICIAL_TOTAL_CATCH,
  obsint.DEBRIEFED_HAUL.HAUL_SAMPLED_BY,
  obsint.DEBRIEFED_SPCOMP.SPECIES,
  obsint.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT,
  obsint.DEBRIEFED_HAUL.RETRV_LATITUDE,
  obsint.DEBRIEFED_HAUL.RETRV_LONGITUDE,
  obsint.DEBRIEFED_HAUL.LATDD_START,
  obsint.DEBRIEFED_HAUL.LONDD_START,
  obsint.DEBRIEFED_HAUL.LATDD_END,
  obsint.DEBRIEFED_HAUL.LONDD_END,
  obsint.DEBRIEFED_HAUL.FISHING_DEPTH_FATHOMS,
  obsint.DEBRIEFED_HAUL.BOTTOM_DEPTH_FATHOMS,
  obsint.DEBRIEFED_HAUL.CATCHER_BOAT_ADFG

FROM
  obsint.DEBRIEFED_HAUL LEFT JOIN obsint.DEBRIEFED_SPCOMP

ON
  obsint.DEBRIEFED_HAUL.HAUL_JOIN = obsint.DEBRIEFED_SPCOMP.HAUL_JOIN

WHERE
  obsint.DEBRIEFED_HAUL.RETRV_LATITUDE < 4900 and
  obsint.DEBRIEFED_HAUL.HAUL_DATE >=  to_date('01-01-&beginyr','dd-mm-yyyy') and
  obsint.DEBRIEFED_HAUL.HAUL_DATE <= to_date('31-12-&endyr','dd-mm-yyyy')
  ;

