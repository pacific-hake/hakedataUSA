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

REM col sample_month format a25
REM col sample_day format a25
REM col sample_year format a25
REM col depth_avg format a25
REM col depth_min format a25
REM col depth_max format a25
REM col freq format a25
REM col males_wgt format a25
REM col males_num format a25
REM col females_num format a25
REM col females_wgt format a25
REM col total_wgt format a25
REM col fish_age_years_final format a25
REM col fish_age_code_final format a25
REM col fish_length format a25
REM col fish_weight format a25
REM col fork_length format a25
REM col exp_wt format a25

REM col spid format a25
REM col sample_no format a25
REM col source_agid format a25
REM col sample_agency format a25
REM col cluster_no format a25
REM col fish_no format a25
REM col fish_length_type format a25
REM col fork_length_estimated format a25
REM col maturity format a25
REM col maturity_agcode format a25
REM col sex format a25
REM col data_type format a25
REM col drvid format a25
REM col gear format a25
REM col grid format a25
REM col inpfc_area format a25
REM col psmfc_area format a25
REM col psmfc_arid format a25
REM col sample_agid format a25
REM col sample_method format a25
REM col sample_type format a25
REM col pcid format a25
REM col port format a25
REM col ftid format a25
REM col cond format a25
REM col cond_agcode format a25

select
 f.spid,
 f.sample_no,
 f.sample_year,
 f.source_agid,
 s.sample_agency,
 f.cluster_no,
 f.fish_age_years_final,
 f.fish_age_code_final,
 f.fish_no,
 f.freq,
 f.fish_length,
 f.fish_length_type,
 f.fork_length_estimated,
 f.fork_length,
 f.maturity,
 f.maturity_agcode,
 f.fish_weight,
 f.sex,
 data_type,
 depth_avg,
 depth_min,
 depth_max,
 drvid,
 gear,
 grid,
 inpfc_area,
 psmfc_area,
 psmfc_arid,
 sample_agid,
 sample_month,
 sample_day,
 sample_method,
 sample_type,
 males_wgt,
 males_num,
 females_num,
 females_wgt,
 total_wgt,
 o.exp_wt,
 pcid,
 port,
 ftid,
 cond,
 cond_agcode
from
 pacfin.bds_fish f,
 pacfin.bds_sample s,
 pacfin.bds_sample_odfw o
where spid = upper(&sp)
 and s.sample_no = f.sample_no(+)
 and s.sample_no = o.sample_no(+)
 and s.sample_year = f.sample_year(+)
 and s.sample_year between &beginyr and &endyr
 and s.sample_year = o.sample_year(+)
order by sample_year, source_agid, sample_no, fish_no, cluster_no;
