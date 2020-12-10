# Get the data bases from a saved file
test_dir_data <- file.path(hakedatawd(), "extractedData")

load(file.path(test_dir_data, "pcatchatsea.Rdat"))
load(file.path(test_dir_data, "Pacfincomp_ft_taylorCatch.Rdat"))
load(file.path(test_dir_data, "NORPACdomesticCatch.Rdat"))
load(file.path(test_dir_data, "atsea.ages.Rdat"))
load(file.path(test_dir_data, "atsea.ageWt.Rdat"))
load(file.path(test_dir_data, "page.Rdat"))
