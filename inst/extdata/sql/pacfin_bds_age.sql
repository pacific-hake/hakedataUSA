REM*****************************************************************************
REM
REM Each column format must be set individually or else the names are truncated.
REM
REM If values for a particular variable come back as "#####" then delete the
REM column format setting for that variable.
REM
REM Modified by Allan Hicks, Nov 2011, with edits by Brad Stenberg, Nov 2016
REM     Commented col commands so that can import directly into R using RODBC
REM*****************************************************************************

REM col depth_avg format a25
REM col depth_min format a25
REM col depth_max format a25
REM col sample_year format a25
REM col age_years format a25
REM col sample_month format a25
REM col sample_day format a25

REM col spid format a25
REM col source_agid format a25
REM col sample_no format a25
REM col cluster_no format a25
REM col fish_no format a25
REM col age_struct_agcode format a25
REM col age_method format a25
REM col age_readability format a25
REM col aged_by format a25
REM col date_aged format a25
REM col data_type format a25
REM col inpfc_area format a25
REM col psmfc_area format a25
REM col psmfc_arid format a25
REM col sample_agid format a25
REM col drvid format a25
REM col gear format a25
REM col grid format a25
REM col sample_method format a25
REM col sample_type format a25
REM col pcid format a25
REM col port format a25
REM col ftid format a25

select
   a.spid,
   a.sample_year,
   a.source_agid,
   a.sample_no,
   a.cluster_no,
   a.fish_no,
   age_struct_agcode,
   age_method,
   TO_NUMBER(age_no) as agenum,
   age_years,
   age_readability,
   aged_by,
   date_aged,
   data_type,
   depth_avg,
   depth_min,
   depth_max,
   inpfc_area,
   psmfc_area,
   psmfc_arid,
   sample_agid,
   drvid,
   gear,
   grid,
   sample_month,
   sample_day,
   sample_method,
   sample_type,
   pcid,
   port,
   ftid
FROM pacfin.bds_age a, pacfin.bds_sample s
WHERE spid LIKE upper (&sp)
  and s.sample_no = a.sample_no(+)
  and s.sample_year = a.sample_year(+)
  and s.sample_year between &beginyr and &endyr
ORDER BY sample_year,
   source_agid,
   sample_no,
   cluster_no,
   fish_no,
   age_no;

