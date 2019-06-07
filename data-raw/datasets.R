data_test_cmp_bin_ind <- read.table(file = "data-raw/data_for_test_cmp_bin_indicators.csv",
                                        na.strings = " ",
                                        header = TRUE,
                                        sep = ",",
                                        fileEncoding = "cp1251")

data_test_cmp_categories <- read.table(file = "data-raw/data_for_test_cmp_categories.csv",
                                        na.strings = " ",
                                        header = TRUE,
                                        sep = ",",
                                        fileEncoding = "cp1251")

binary_indicators <- read.csv(file = "data-raw/binary_indicators.csv", header = TRUE, sep = ",", fileEncoding = "cp1251")

categories <- read.csv(file = "data-raw/categories.csv", header = TRUE, sep = ",", fileEncoding = "cp1251")

usethis::use_data(data_test_cmp_bin_ind,
                  data_test_cmp_categories, internal = TRUE, overwrite = TRUE)
usethis::use_data(binary_indicators, overwrite = TRUE)
usethis::use_data(categories, overwrite = TRUE)
