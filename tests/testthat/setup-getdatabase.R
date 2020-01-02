# Get the data bases from a saved file
test_dir_data <- file.path(hakedatawd(), "extractedData")

load(file.path(test_dir_data, "pcatchatsea.Rdat"))
load(file.path(test_dir_data, "NORPACdomesticCatch.Rdat"))
