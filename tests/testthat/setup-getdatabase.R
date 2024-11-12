# Get the data bases from a saved file
test_dir_data <- file.path(hakedata_wd(), "extractedData")

load(file.path(test_dir_data, "pcatch.Rdat"))
load(file.path(test_dir_data, "NORPACdomesticCatch.Rdat"))
load(file.path(test_dir_data, "atsea.ages.Rdat"))
load(file.path(test_dir_data, "nweight.Rdat"))
load(file.path(test_dir_data, "page.Rdat"))
