raw_dict <- read.csv(fileEncoding = "UTF8", file = "./data_raw/groovescale_dict.csv", stringsAsFactors = FALSE, header = TRUE)

groovescale_dict <- psychTestR::i18n_dict$new(raw_dict)

usethis::use_data(groovescale_dict, overwrite = TRUE)
