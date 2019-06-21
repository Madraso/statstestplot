context("Тестирование функции построения графика сравнения бинарных показателей")

test_that("Нормальное поведение", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(2, 3, 4)
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_BI_1"
	#Построение графика
	graphic <- cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(67, 33, 56, 44, 59, 41)
	expect_number_peoples <- c(32, 16, 28, 22, 34, 24)
	expect_lower_borders_ci <- c(53, 22, 42, 31, 46, 30)
	expect_upper_borders_ci <- c(78, 47, 69, 58, 70, 54)
	expect_p_values <- c("p = 0.472", "p > 0.999", "p > 0.999")
	expect_x_min_pos <- c(0.775, 1.775, 2.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225)
	expect_y_pos <- c(88, 79, 80)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_equal(expect_p_values, graphic$layers[[4]]$stat_params$annotations)
	expect_equal(expect_x_min_pos, graphic$layers[[4]]$stat_params$xmin)
	expect_equal(expect_x_max_pos, graphic$layers[[4]]$stat_params$xmax)
	expect_equal(expect_y_pos, graphic$layers[[4]]$stat_params$y_position)
})

test_that("В группе все 0", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(2, 3, 4, 5)
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_BI_2"
	#Построение графика
	graphic <- cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(67, 33, 56, 44, 59, 41, 0, 100)
	expect_number_peoples <- c(32, 16, 28, 22, 34, 24, 0, 21)
	expect_lower_borders_ci <- c(53, 22, 42, 31, 46, 30, 0, 85)
	expect_upper_borders_ci <- c(78, 47, 69, 58, 70, 54, 15, 100)
	expect_p_values <- c("p = 0.315", "p = 0.913", "p > 0.999", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(88, 79, 80, 110)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_equal(expect_p_values, graphic$layers[[4]]$stat_params$annotations)
	expect_equal(expect_x_min_pos, graphic$layers[[4]]$stat_params$xmin)
	expect_equal(expect_x_max_pos, graphic$layers[[4]]$stat_params$xmax)
	expect_equal(expect_y_pos, graphic$layers[[4]]$stat_params$y_position)
})

test_that("В группе все 1", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(6, 2, 3, 4)
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_BI_3"
	#Построение графика
	graphic <- cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(40, 60, 67, 33, 56, 44, 59, 41)
	expect_number_peoples <- c(27, 41, 32, 16, 28, 22, 34, 24)
	expect_lower_borders_ci <- c(29, 48, 53, 22, 42, 31, 46, 30)
	expect_upper_borders_ci <- c(52, 71, 78, 47, 69, 58, 70, 54)
	expect_p_values <- c("p < 0.001*", "p = 0.315", "p = 0.913", "p > 0.999")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(81, 88, 79, 80)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_equal(expect_p_values, graphic$layers[[4]]$stat_params$annotations)
	expect_equal(expect_x_min_pos, graphic$layers[[4]]$stat_params$xmin)
	expect_equal(expect_x_max_pos, graphic$layers[[4]]$stat_params$xmax)
	expect_equal(expect_y_pos, graphic$layers[[4]]$stat_params$y_position)
})

test_that("В группе имеются пропущенные значения", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(2, 3, 7, 4)
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_BI_4"
	#Построение графика
	graphic <- cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(67, 33, 56, 44, 0, 100, 59, 41)
	expect_number_peoples <- c(32, 16, 28, 22, 0, 19, 34, 24)
	expect_lower_borders_ci <- c(53, 22, 42, 31, NaN, 83, 46, 30)
	expect_upper_borders_ci <- c(78, 47, 69, 58, NaN, 100, 70, 54)
	expect_p_values <- c("p = 0.472", "p > 0.999", "p > 0.999")
	expect_x_min_pos <- c(0.775, 1.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 4.225)
	expect_y_pos <- c(88, 79, 80)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_equal(expect_p_values, graphic$layers[[4]]$stat_params$annotations)
	expect_equal(expect_x_min_pos, graphic$layers[[4]]$stat_params$xmin)
	expect_equal(expect_x_max_pos, graphic$layers[[4]]$stat_params$xmax)
	expect_equal(expect_y_pos, graphic$layers[[4]]$stat_params$y_position)
	expect_warning(cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name),
		"Группа Группа_1 в показателе Показатель_6 содержит пустые значения")
})

test_that("В группе имеются не бинарные значения", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	columns <- c(2, 3, 4, 8)
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_BI_5"
	#Построение графика
	graphic <- cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(67, 33, 56, 44, 59, 41, 0, 100)
	expect_number_peoples <- c(32, 16, 28, 22, 34, 24, 0, 25)
	expect_lower_borders_ci <- c(53, 22, 42, 31, 46, 30, NaN, 87)
	expect_upper_borders_ci <- c(78, 47, 69, 58, 70, 54, NaN, 100)
	expect_p_values <- c("p = 0.472", "p > 0.999", "p > 0.999")
	expect_x_min_pos <- c(0.775, 1.775, 2.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225)
	expect_y_pos <- c(88, 79, 80)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_equal(expect_p_values, graphic$layers[[4]]$stat_params$annotations)
	expect_equal(expect_x_min_pos, graphic$layers[[4]]$stat_params$xmin)
	expect_equal(expect_x_max_pos, graphic$layers[[4]]$stat_params$xmax)
	expect_equal(expect_y_pos, graphic$layers[[4]]$stat_params$y_position)
	expect_warning(cmp.bin.indicators(data_test_cmp_bin_ind, group_id,
		group_labels, columns, pic_path, pic_name),
		"Группа Группа_1 в показателе Показатель_7 содержит не бинарные значения")
})
