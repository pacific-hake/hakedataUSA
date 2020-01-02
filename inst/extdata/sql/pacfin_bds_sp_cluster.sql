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

REM col sample_year format a25
REM col cluster_wgt format a25
REM col species_wgt format a25

REM col spid format a25
REM col source_agid format a25
REM col sample_no format a25
REM col cluster_no format a25
REM col frame_clwt format a25
REM col adj_clwt format a25

select
 spid,
 sample_year,
 source_agid,
 sample_no,
 cluster_no,
 species_wgt,
 cluster_wgt,
 frame_clwt,
 adj_clwt
from pacfin.bds_cluster
where spid LIKE upper (&sp)
 and sample_year between &beginyr and &endyr;
