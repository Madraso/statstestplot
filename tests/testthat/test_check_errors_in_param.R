context("Тестирование функции проверки на корректность аргументов функций построения графиков")

test_that("data не является датафреймом", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param("dataframe", group_id, group_labels,
		columns, path_pics_out, pic_name, "all"), "Аргумент data не является датафреймом")
})

test_that("Датафрейм data пуст", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data.frame(), group_id, group_labels,
		columns, path_pics_out, pic_name, "all"), "Датафрейм data пуст")
})

test_that("group_id не является числом", {
	group_id <- "group_id"
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"), "Аргумент group_id не является числом")
})

test_that("group_id является вектором не единичной длины", {
	group_id <- c(1, 2, 3)
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"), "Аргумент group_id не является числом")
})

test_that("group_id не входит в заданный диапазон чисел", {
	group_id <- 100
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент group_id должен находиться в диапазоне допустимых значений (1 - 8)",
		fixed = TRUE)
})

test_that("group_labels не является строковым вектором", {
	group_id <- 1
	group_labels <- 1000
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент group_labels не является строковым вектором")
})

test_that("group_labels нулевой длины", {
	group_id <- 1
	group_labels <- c()
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент group_labels не является строковым вектором")
})

test_that("group_labels содержит пустые строки", {
	group_id <- 1
	group_labels <- c("Группа_1", "")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Вектор group_labels содержит пустые строки")
})

test_that("column_* не является числовым вектором", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- "columns"
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент column_* не является числовым вектором", fixed = TRUE)
})

test_that("column_* содержит числа не из диапазона допустимых значений", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(2, 3, 50, 100)
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Вектор column_* содержит номера столбцов, находящихся не в диапазоне допустимых значений (1 - 8), или содержит номер столбца, совпадающий с group_id", fixed = TRUE)
})

test_that("column_* содержит в себе group_id", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(1, 2, 3)
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Вектор column_* содержит номера столбцов, находящихся не в диапазоне допустимых значений (1 - 8), или содержит номер столбца, совпадающий с group_id", fixed = TRUE)
})

test_that("path_pics_out не является строкой", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- 1000
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент path_pics_out не является строкой")
})

test_that("path_pics_out - пустая строка", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- ""
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент path_pics_out является пустой строкой")
})

test_that("Директория в path_pics_out не существует", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- "directory"
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Директории directory не существует")
})

test_that("pic_name не является строкой", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- 500
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент pic_name не является строкой")
})

test_that("pic_name - пустая строка", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- ""
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all"),
		"Аргумент pic_name является пустой строкой")
})

test_that("p_display не является строкой", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, 100),
		"Аргумент p_display не является строковым вектором")
})

test_that("p_display - строка с неверным содержимым", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "string"),
		"Аргумент p_display может принимать следующие значения - s, ns, all")
})

test_that("p_adjust_method не является строкой", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all", 100),
		"Аргумент p_adjust_method не является строковым вектором")
})

test_that("p_adjust_method - строка с неверным содержимым", {
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- 2
	path_pics_out <- paste0(tempdir(), "/")
	pic_name <- "График"
	expect_error(check_errors_in_param(data_test_cmp_bin_ind, group_id,
		group_labels, columns, path_pics_out, pic_name, "all", "string"),
		paste0("Аргумент p_adjust_method может принимать следующие значения - ",
                stats::p.adjust.methods))
})
