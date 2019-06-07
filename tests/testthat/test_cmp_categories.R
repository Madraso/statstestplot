context("Тестирование функции построения графика категорий (например, функциональные классы)")

test_that("Нормальное поведение", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 2
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_1"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(24, 27, 24, 22, 27, 17, 25, 34)
	expect_number_peoples <- c(14, 11, 14, 9, 16, 7, 15, 14)
	expect_lower_borders_ci <- c(15, 16, 15, 12, 17, 9, 16, 22)
	expect_upper_borders_ci <- c(36, 42, 36, 37, 40, 31, 38, 49)
	expect_p_values <- c("p > 0.999", "p > 0.999", "p = 0.753", "p = 0.753")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(52, 47, 50, 59)
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
	column <- 3
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_2"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(100, 24, 0, 22, 0, 17, 0, 37)
	expect_number_peoples <- c(59, 10, 0, 9, 0, 7, 0, 15)
	expect_lower_borders_ci <- c(94, 14, 0, 12, 0, 9, 0, 24)
	expect_upper_borders_ci <- c(100, 39, 6, 37, 6, 31, 6, 52)
	expect_p_values <- c("p < 0.001*", "p < 0.001*", "p = 0.001*", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(110, 47, 41, 62)
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

test_that("В группе все 2", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 4
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_3"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(34, 0, 24, 100, 20, 0, 22, 0)
	expect_number_peoples <- c(20, 0, 14, 41, 12, 0, 13, 0)
	expect_lower_borders_ci <- c(23, 0, 15, 91, 12, 0, 13, 0)
	expect_upper_borders_ci <- c(47, 9, 36, 100, 32, 9, 34, 9)
	expect_p_values <- c("p < 0.001*", "p < 0.001*", "p = 0.001*", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(57, 110, 42, 44)
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

test_that("В группе все 3", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 5
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_4"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(0, 37, 0, 22, 100, 22, 0, 20)
	expect_number_peoples <- c(0, 15, 0, 9, 59, 9, 0, 8)
	expect_lower_borders_ci <- c(0, 24, 0, 12, 94, 12, 0, 10)
	expect_upper_borders_ci <- c(6, 52, 6, 37, 100, 37, 6, 34)
	expect_p_values <- c("p < 0.001*", "p < 0.001*", "p < 0.001*", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(62, 47, 110, 44)
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

test_that("В группе все 4", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 6
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_5"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(22, 0, 27, 0, 31, 0, 20, 100)
	expect_number_peoples <- c(13, 0, 16, 0, 18, 0, 12, 41)
	expect_lower_borders_ci <- c(13, 0, 17, 0, 20, 0, 12, 91)
	expect_upper_borders_ci <- c(34, 9, 40, 9, 43, 9, 32, 100)
	expect_p_values <- c("p < 0.001*", "p < 0.001*", "p < 0.001*", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775, 3.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225, 4.225)
	expect_y_pos <- c(44, 50, 53, 110)
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

test_that("В группе имеются пустые значения", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 7
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_6"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(0, 24, 0, 27, 0, 27, 0, 22)
	expect_number_peoples <- c(0, 10, 0, 11, 0, 11, 0, 9)
	expect_lower_borders_ci <- c(NaN, 14, NaN, 16, NaN, 16, NaN, 12)
	expect_upper_borders_ci <- c(NaN, 39, NaN, 42, NaN, 42, NaN, 37)
	#Сравнение ожидаемых значений с фактическими
	expect_equal(expect_percents, graphic$data$mean)
	expect_equal(expect_number_peoples, graphic$data$x)
	expect_equal(expect_lower_borders_ci, graphic$data$lower)
	expect_equal(expect_upper_borders_ci, graphic$data$upper)
	expect_warning(
		cmp.categories(data_test_cmp_categories, group_id, group_labels, column, pic_path, pic_name),
		"Группа Группа_1 в показателе Функциональный.класс.5 содержит пустые значения"
	)
})

test_that("Категории в группах не полностью пересекаются", {
	#Определение параметров
	group_id <- 1
	group_labels <- c("Группа_1", "Группа_2")
	column <- 8
	pic_path <- paste0(tempdir(), "/")
	pic_name <- "Test_CAT_7"
	#Построение графика
	graphic <- cmp.categories(data_test_cmp_categories, group_id,
		group_labels, column, pic_path, pic_name)
	#Инициализация ожидаемых значений различных параметров графика
	expect_percents <- c(61, 0, 39, 44, 0, 56)
	expect_number_peoples <- c(36, 0, 23, 18, 0, 23)
	expect_lower_borders_ci <- c(48, 0, 28, 30, 0, 41)
	expect_upper_borders_ci <- c(72, 9, 52, 59, 6, 70)
	expect_p_values <- c("p < 0.001*", "p = 0.682", "p < 0.001*")
	expect_x_min_pos <- c(0.775, 1.775, 2.775)
	expect_x_max_pos <- c(1.225, 2.225, 3.225)
	expect_y_pos <- c(82, 69, 80)
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
