# hakedataUSA
Code to extract and workup the US data for the Pacific hake assessment.

# Current assessment
To extract code for this year's assessment open a new R session and run the following:
```
local.assess <- file.path("c:", "stockAssessment", "hake-assessment")
local.model <- "2020.03.00_base_model"

library(devtools)
devtools::install_github("pacific-hake/hakedataUSA")
hakedata <- pulldatabase()
norpaccatches(hakedata$ncatch, writecsv = TRUE, colour = TRUE, nyears = 4)
catch_pacfin <- pacfincatches(hakedata$pcatch)
UScatchPlots(doPNG = TRUE, nyears = 5)
age_norpac <- atseacomps(hakedata$atsea.ages, hakedata$ncatch)
age_shore <- shorecomps(verbose = TRUE)
age_yearlyweights <- mappingagesamples(hakedata$atsea.ages, hakedata$ncatch, savepng = TRUE)
jtcdecotoliths <- agedotoliths(hakedata$atsea.ages)
datatoassessment(dirout = file.path(local.assess, "data"), year = hakedata_year(), 
  filedat = file.path(local.assess, "models", local.model, "hake_data.ss"))
datatocomps(dirdata = file.path(local.assess, "data"),
  dirmod = file.path(local.assess, "models", local.model))
```

# Issues
Please contact kelli.johnson@noaa.gov if there are any issues in running the code, though the databases will only be accessible to members of the US JTC.