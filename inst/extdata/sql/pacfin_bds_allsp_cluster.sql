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

REM col cluster_wgt format a25

REM col sample_no format a25
REM col cluster_no format a25

select
 sample_no,
 cluster_wgt,
 cluster_no 
from pacfin.bds_cluster
where sample_year between &beginyr and &endyr;
