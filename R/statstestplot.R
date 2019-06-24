p_value_formatted <- function(p_value) {
  nod <- 3
  p_val <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", format(round(p_value, nod), nsmall = nod)))
  p_val <- ifelse(p_value < 0.05, paste0(p_val, "*"), p_val)
  p_val <- ifelse(round(p_value, nod) == 1.0, "p > 0.999", p_val)
  return (p_val)
}

save_pic_png <- function(plot, path_pics, file_name, ...) {
  filename <- paste0(path_pics, file_name, ".png")
  grDevices::png(filename = filename, ...)
  print(plot)
  grDevices::dev.off()
}

save_pic_png_600 <- function(plot, path_pics, file_name) {
  filename <- paste0("VLADISLAV_", file_name)
  save_pic_png(plot = plot,
               path_pics = path_pics,
               file_name = filename,
               res = 600,
               width = 10 * 2, height = 5 * 2,
               units = "cm",
               pointsize = 1,
               type = "cairo")
  print(plot)
}

check_columns <- function(function_name, data, group_id, group_labels, column_list) {
  sep_data_on_groups <- split(data, data[, group_id])
  groups_names <- levels(data[, group_id])
  columns_names <- colnames(data)
  incorrect_vars_and_grps <- list()
  for (i in 1:length(sep_data_on_groups)) {
    for (j in 1:length(column_list)) {
      warning_flag <- FALSE
      if (anyNA(sep_data_on_groups[[i]][, column_list[j]])) {
        warning(paste("Группа", group_labels[i], "в показателе", columns_names[column_list[j]],
                      "содержит пустые значения"))
        warning_flag <- TRUE
      }
      if (function_name == "cmp.bin.indicators") {
        min_value <- 0
        max_value <- 1
        correct_values <- all(stats::na.omit(sep_data_on_groups[[i]][[ column_list[j] ]]) %in% min_value:max_value)
        if (!correct_values) {
          warning(paste("Группа", group_labels[i], "в показателе", columns_names[column_list[j]],
                        "содержит не бинарные значения"))
          warning_flag <- TRUE
        }
      }
      if (warning_flag) incorrect_vars_and_grps[[length(incorrect_vars_and_grps) + 1]] <- list(group = groups_names[i], column = j)
    }
  }
  return (incorrect_vars_and_grps)
}

check_errors_in_param <- function(data, group_id, group_labels,
                                  columns, path_pics_out, pic_name, p_display, p_adjust_method = "BH") {
  #data
  if (class(data) != "data.frame") stop("Аргумент data не является датафреймом")
  else if (plyr::empty(data)) stop("Датафрейм data пуст")
  else {
    #group_id
    range_values <- 1:ncol(data)
    if (class(group_id) != "numeric" || length(group_id) != 1) stop("Аргумент group_id не является числом")
    else if (!(group_id %in% range_values)) {
      stop(paste0("Аргумент group_id должен находиться в диапазоне допустимых значений", " (1 - ", ncol(data), ")"))
    }
    #columns
    if (class(columns) != "numeric") stop("Аргумент column_* не является числовым вектором")
    else if (!all(columns %in% range_values) || any(columns == group_id)) {
      stop(paste0("Вектор column_* содержит номера столбцов, находящихся не в диапазоне допустимых значений",
                  " (1 - ", ncol(data), ")", ", или содержит номер столбца, совпадающий с group_id"))
    }
  }
  #group_labels
  if (class(group_labels) != "character" || length(group_labels) == 0) {
    stop("Аргумент group_labels не является строковым вектором")
  }
  else {
    for (i in 1:length(group_labels)) {
      if (group_labels[i] == "") {
        stop("Вектор group_labels содержит пустые строки")
      }
    }
  }
  #path_pics_out
  if (class(path_pics_out) != "character") stop("Аргумент path_pics_out не является строкой")
  else if (path_pics_out == "") stop("Аргумент path_pics_out является пустой строкой")
  else if (!dir.exists(path_pics_out)) stop(paste("Директории", path_pics_out, "не существует"))
  #pic_name
  if (class(pic_name) != "character") stop("Аргумент pic_name не является строкой")
  else if (pic_name == "") stop("Аргумент pic_name является пустой строкой")
  #p_display
  if (class(p_display) != "character") stop("Аргумент p_display не является строковым вектором")
  else if (!(p_display %in% c("s", "ns", "all"))) {
    stop("Аргумент p_display может принимать следующие значения - s, ns, all")
  }
  #p_adjust_method
  if (class(p_adjust_method) != "character") stop("Аргумент p_adjust_method не является строковым вектором")
  else if (!(p_adjust_method %in% stats::p.adjust.methods)) {
    stop(paste0("Аргумент p_adjust_method может принимать следующие значения - ",
                stats::p.adjust.methods))
  }
}

get_groups_with_errors <- function(function_name, incorrect_vars_and_grps, indicator_number) {
  groups_with_errors <- c()
  if (length(incorrect_vars_and_grps) > 0) {
    if (function_name == "cmp.bin.indicators") {
      indicator_with_error <- any(sapply(incorrect_vars_and_grps, function (x) { x$column == indicator_number }))
      if (indicator_with_error) {
        for (j in 1:length(incorrect_vars_and_grps)) {
          if (incorrect_vars_and_grps[[j]]$column == indicator_number) {
            groups_with_errors <- append(groups_with_errors, incorrect_vars_and_grps[[j]]$group)
          }
        }
      }
    }
    else if (function_name == "cmp.categories") {
      for (j in 1:length(incorrect_vars_and_grps)) {
        groups_with_errors <- append(groups_with_errors, incorrect_vars_and_grps[[j]]$group)
      }
    }
  }
  return (groups_with_errors)
}

get_p_values <- function(function_name, data, indicators, groups, incorrect_vars_and_grps) {
  number_groups <- length(groups)
  number_indicators <- length(indicators)
  groups_combn <- t(utils::combn(groups, 2))
  p_values <- c()
  for (i in 1:number_indicators) {
    for (j in 1:nrow(groups_combn)) {
      groups_with_errors <- get_groups_with_errors(function_name, incorrect_vars_and_grps, i)
      if (any(groups_combn[j, ] %in% groups_with_errors)) next
      filtered_data <- dplyr::filter(data, data[[1]] %in% groups_combn[j, ])
      if (function_name == "cmp.bin.indicators") {
        contingency_table <- table(as.vector(filtered_data[, 1]), filtered_data[, indicators[i]])
      } else if (function_name == "cmp.categories") {
        contingency_table <- table(as.vector(filtered_data[, 1]), filtered_data[, 2] == indicators[i])
        contingency_table <- t(contingency_table)
      }
      p_values <- append(p_values, stats::fisher.test(contingency_table)$p.value)
    }
  }
  return (p_values)
}
  
get_x_arrow_coordinates <- function(function_name, groups, number_groups, number_indicators, incorrect_vars_and_grps) {
  groups_indicator_width <- 0.9
  width_btw_indicators <- 0.55
  bar_width <- groups_indicator_width / number_groups
  start_x <- seq(from = width_btw_indicators, by = bar_width, length.out = number_groups)
  x_positions <- data.frame(x_min = numeric(), x_max = numeric())
  coord_combn <- as.data.frame(cbind(t(utils::combn(groups, 2)), t(utils::combn(start_x, 2))))
  coord_combn$V1 <- as.character(coord_combn$V1)
  coord_combn$V2 <- as.character(coord_combn$V2)
  coord_combn$V3 <- as.double(as.character(coord_combn$V3))
  coord_combn$V4 <- as.double(as.character(coord_combn$V4))
  coord_combn$V4 <- coord_combn$V4 + bar_width
  for (i in 1:number_indicators) {
    groups_with_errors <- get_groups_with_errors(function_name, incorrect_vars_and_grps, i)
    for (j in 1:nrow(coord_combn)) {
      if (any(coord_combn[j, c(1, 2)] %in% groups_with_errors)) next
      else x_positions[nrow(x_positions) + 1, ] <- coord_combn[j, c(3, 4)] + (i - 1)
    }
  }
  return (x_positions)
}

get_y_arrow_coordinates <- function(function_name, data, groups,
                                    indicators, number_groups, number_indicators, incorrect_vars_and_grps) {
  y_positions <- c()
  groups_combn <- t(utils::combn(groups, 2))
  number_combn <- nrow(groups_combn)
  for (i in 1:number_indicators) {
    y_pos_temp <- c()
    filtered_data <- dplyr::filter(data, data$groups %in% groups, data$indicators == indicators[i])
    upper_borders_ci <- stats::na.omit(filtered_data$upper)
    step_length <- 10
    y_start <- max(upper_borders_ci) + 10
    y_end <- y_start + (step_length * number_combn)
    seq_step <- (y_end - y_start) / (number_combn - 1)
    groups_with_errors <- get_groups_with_errors(function_name, incorrect_vars_and_grps, i)
    for (j in 1:number_combn) {
      if (any(groups_combn[j, ] %in% groups_with_errors)) next
      else {
        if (j == 1) y_pos_temp <- append(y_pos_temp, y_start)
        else {
          y_pos_temp_without_na <- stats::na.omit(y_pos_temp)
          if (length(y_pos_temp_without_na) == 0) y_pos_temp <- append(y_pos_temp, y_start)
          else y_pos_temp <- append(y_pos_temp, y_pos_temp_without_na[length(y_pos_temp_without_na)] + seq_step)
        }
      }
    }
    y_positions <- append(y_positions, y_pos_temp)
  }
  return (y_positions)
}
  
get_p_values_and_arrow_coord <- function(function_name, input_data, binom_data, indicators, groups,
                             incorrect_vars_and_grps, p_adjust_method) {
  p_values <- get_p_values(function_name, input_data, indicators, groups, incorrect_vars_and_grps)
  p_values <- stats::p.adjust(p_values, method = p_adjust_method)
  arrow_x_coord <- get_x_arrow_coordinates(function_name, groups, length(groups), length(indicators), incorrect_vars_and_grps)
  arrow_y_coord <- get_y_arrow_coordinates(function_name, binom_data, groups, indicators,
                                           length(groups), length(indicators), incorrect_vars_and_grps)
  arrow_positions <- data.frame()
  if (!plyr::empty(arrow_x_coord)) {
    arrow_positions <- as.data.frame(p_values)
    arrow_positions <- cbind(arrow_positions, arrow_x_coord)
    arrow_positions <- cbind(arrow_positions, arrow_y_coord)
    colnames(arrow_positions) <- c("p", "x_min", "x_max", "y")
  }
  return (arrow_positions)
}
  
get_data_for_building_graphic <- function(function_name, indicators, groups, number_peoples) {
  data_for_graphic <- data.frame()
  for (i in 1:length(indicators)) {
    if (function_name == "cmp.bin.indicators") peoples_total <- rep(sum(stats::na.omit(number_peoples[, i])), length(groups))
    else if (function_name == "cmp.categories") peoples_total <- margin.table(number_peoples, margin = 1)
    for (j in 1:length(groups)) {
      if (!is.na(number_peoples[j, i])) {
        binom <- data.frame(groups[j], indicators[i],
                          binom::binom.confint(number_peoples[j, i], peoples_total[j], method = "wilson"))
        data_for_graphic <- rbind(data_for_graphic, binom)
      }
      else {
        binom <- data.frame(groups[j], indicators[i],
                          binom::binom.confint(0, 0, method = "wilson"))
        binom$mean <- 0
        data_for_graphic <- rbind(data_for_graphic, binom)
      }
    }
  }
  data_for_graphic$mean <- round(data_for_graphic$mean * 100)
  data_for_graphic$lower <- round(data_for_graphic$lower * 100)
  data_for_graphic$upper <- round(data_for_graphic$upper * 100)
  return (data_for_graphic)
}
#' Построение графика сравнения бинарных показателей в исследуемых группах пациентов
#'
#' Имеется выборка из N пациентов, разделенных на 2 или более группы, а также набор показателей.
#' Каждый показатель представляет собой набор бинарных значений (0 и 1). 1 - пациент имеет данный показатель, 0 - показатель у пациента отсутствует.
#'
#' На графике каждый показатель - это набор столбцов, их количество равно количеству групп пациентов.
#' Над столбцами отображается информация о количестве в группе пациентов, имеющих данный показатель, а также процент этих людей от всех пациентов, имеющих данный показатель.
#' Для столбцов в виде стрелки отображается диапазон, в котором с вероятностью 95\% находится "истинный" процент пациентов, называемый доверительным интервалом.
#' Для всевозможных пар групп отображается p-значение, которое используется для их сравнения.
#'
#' @param data Датафрейм с данными (Пример: statstestplot::binary_indicators)
#' @param group_id Число, являющееся номером столбца с группами пациентов
#' @param group_labels Строковый вектор, cодержащий имена групп. Эти имена будут сопоставлены в легенде графика
#' @param column_list Числовой вектор, содержащий номера столбцов с бинарными показателями
#' @param path_pics_out Строка, содержащая путь, по которому будет сохранен файл с графиком
#' @param pic_name Строка, содержащая имя сохраняемого файла
#' @param p_display Решает, какие парные p-значения отображать.
#' Возможные значения: "s" (значимые [p <= 0.05]), "ns" (незначимые [p > 0.05]), "all" (все значения). (По умолчанию: "all").
#' @param p_adjust_method Решает, каким методом будут отрегулированы p-значения.
#' Возможные значения: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". (По умолчанию: "BH").
#' @return Объект класса ggplot2
#' @export
#' @examples
#' #Подключим библиотеку и загрузим датафрейм с бинарными показателями
#' library(statstestplot)
#' data(binary_indicators)
#'
#' #Распечатаем его
#' print(binary_indicators)
#'
#' #Построим график с параметрами по умолчанию
#' group_id <- 1
#' group_labels <- c("A", "B", "C")
#' columns <- c(2, 3, 4, 5, 6)
#' picture_name <- "БП_все_значения_p"
#' picture_path <- paste0(getwd(), "/")
#' graphic <- cmp.bin.indicators(binary_indicators, group_id,
#'     group_labels, columns, picture_path, picture_name)
#'
#' #Построим график с отображением только незначимых p и их регулировкой методом Бенджамини-Хохберга
#' group_id <- 1
#' group_labels <- c("A", "B", "C")
#' columns <- c(2, 3, 4, 5, 6)
#' p_display <- "ns"
#' p_adjust_method = "BH"
#' picture_name <- "БП_незначимые_p"
#' picture_path <- paste0(getwd(), "/")
#' graphic <- cmp.bin.indicators(binary_indicators, group_id,
#'     group_labels, columns, picture_path, picture_name, p_display, p_adjust_method)
cmp.bin.indicators <- function(data, group_id, group_labels, column_list, path_pics_out, pic_name, p_display = "all", p_adjust_method = "BH") {
  #проверка входных параметров на корректность
  check_errors_in_param(data, group_id, group_labels, column_list, path_pics_out, pic_name, p_display)
  #проверка столбцов c переменными на ошибки
  incorrect_vars_and_grps <- check_columns("cmp.bin.indicators", data, group_id, group_labels, column_list)
  #преобразование столбца с группами и столбцов с показателями к факторам
  data[, group_id] <- factor(data[, group_id])
  for (i in 1:length(column_list)) {
    data[, column_list[i]] <- factor(as.logical(data[, column_list[i]]))
  }
  #запоминаем имена групп и показателей
  groups <- levels(data[, group_id])
  indicators <- colnames(data)[column_list]
  #создание датафрейма, содержащего количество пациентов в данной группе, имеющих данный показатель
  number_peoples <- data.frame()
  for (i in 1:length(indicators)) {
    contingency_table <- table(data[, c(group_id, column_list[i])])
    number_peoples <- rbind(number_peoples, contingency_table[, "TRUE"])
  }
  number_peoples <- t(number_peoples)
  rownames(number_peoples) <- groups
  colnames(number_peoples) <- indicators
  #присваивание неопределенных значений ячейкам number_peoples на пересечении группы и показателя с ошибкой
  if (length(incorrect_vars_and_grps) > 0) {
    for (i in 1:length(incorrect_vars_and_grps)) {
      number_peoples[incorrect_vars_and_grps[[i]]$group, incorrect_vars_and_grps[[i]]$column] <- NA
    }
  }
  #создание датафрейма с данными для построения графика 
  data_for_graphic <- get_data_for_building_graphic("cmp.bin.indicators", indicators, groups, number_peoples)
  #создание датафрейма с p-значениями и координатами стрелок
  data_with_necessary_col <- data[, c(group_id, column_list)]
  arrow_positions <- get_p_values_and_arrow_coord("cmp.bin.indicators", data_with_necessary_col,
                                                  data_for_graphic, indicators,
                                                  groups, incorrect_vars_and_grps, p_adjust_method)
  #фильтрация датафрейма с p-значениями в зависимости от режима их отображения
  if (p_display == "s") {
    arrow_positions <- dplyr::filter(arrow_positions, arrow_positions$p <= 0.05)
  } else if (p_display == "ns") {
    arrow_positions <- dplyr::filter(arrow_positions, arrow_positions$p > 0.05)
  }
  #инициализация графика
  graphic <- ggplot2::ggplot(data_for_graphic, ggplot2::aes(x = data_for_graphic$indicators,
                                          y = data_for_graphic$mean, fill = data_for_graphic$groups))
  graphic <- graphic + ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE)
  #редактирование оси абсцисс
  graphic <- graphic + ggplot2::scale_x_discrete(name = NULL)
  #изменение угла наклона надписей на оси абсцисс
  graphic <- graphic + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 0.5, vjust = 0.8))
  #редактирование оси ординат
  lim_y <- 100
  arrow_positions_without_na <- stats::na.omit(arrow_positions)
  if (!plyr::empty(arrow_positions_without_na)) {
    if (max(arrow_positions_without_na$y) > 100) {
      lim_y <- max(arrow_positions_without_na$y) + 100
    }
  }
  graphic <- graphic + ggplot2::scale_y_continuous(name = NULL, breaks = seq(0, lim_y, 5),
                                          limits = c(0, lim_y), labels = function (x) paste0(x, "%"))
  #изменение размера элементов легенды
  graphic <- graphic + ggplot2::theme(legend.key.size = grid::unit(0.7, "cm"))
  #редактирование заголовка легенды
  graphic <- graphic + ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  #редактирование названий групп в легенде
  peoples_total <- table(data[, group_id])
  for (i in 1:length(groups)) {
    group_labels[i] <- paste0(group_labels[i], "\n(n = ", peoples_total[i], ")")
  }
  graphic <- graphic + ggplot2::scale_fill_discrete(labels = group_labels, limits = groups)
  #вывод информации о количестве пациентов и проценте этих пациентов над столбцами
  percents <- as.character(data_for_graphic$mean)
  number_peoples_in_group <- as.character(data_for_graphic$x)
  labels_for_bars <- c()
  for (i in 1:length(percents)) {
    if (is.nan(data_for_graphic[i, 7]) && is.nan(data_for_graphic[i, 8])) {
      labels_for_bars <- append(labels_for_bars, "")
    }
    else {
      labels_for_bars <- append(labels_for_bars, paste0(percents[i], "%  ", "(", number_peoples_in_group[i], ")"))
    }
  }
  graphic <- graphic + ggplot2::geom_text(ggplot2::aes(label = labels_for_bars),
                                          vjust = -0.5,
                                          hjust = 0.54,
                                          position = ggplot2::position_dodge(0.9),
                                          size = 3,
                                          na.rm = TRUE)
  #добавление к графику доверительных интервалов для процента пациентов
  graphic <- graphic + ggplot2::geom_errorbar(ggplot2::aes(ymin = data_for_graphic$lower,
                                                           ymax = data_for_graphic$upper),
                                     width = 0.5,
                                     position = ggplot2::position_dodge(0.9),
                                     na.rm = TRUE)
  #добавление p-значений и стрелок на график
  if (!plyr::empty(arrow_positions)) {
    graphic <- graphic + ggsignif::geom_signif(y_position = arrow_positions$y,
                                              xmin = arrow_positions$x_min,
                                              xmax = arrow_positions$x_max,
                                              annotations = p_value_formatted(arrow_positions$p),
                                              vjust = 0,
                                              textsize = 3,
                                              na.rm = TRUE)
    
  }
  #сохранение графика в файл и вывод графика на экран
  save_pic_png_600(graphic, path_pics_out, pic_name)
  return (graphic)
}
#' Построение графика сравнения категорий категориального признака в исследуемых группах пациентов
#'
#' Имеется выборка из N пациентов, разделенных на 2 или более группы, и каждому пациенту соответствует какая-либо категория признака.
#'
#' На графике каждая категория - это набор столбцов, их количество равно количеству групп пациентов.
#' Над столбцами отображается информация о количестве в группе пациентов, принадлежащих данной категории, а также процент этих людей от всех пациентов в группе.
#' Для столбцов в виде стрелки отображается диапазон, в котором с вероятностью 95\% находится "истинный" процент пациентов, называемый доверительным интервалом.
#' Для всевозможных пар групп отображается p-значение, которое используется для их сравнения.
#'
#' @param data Датафрейм с данными (Пример: statstestplot::categories)
#' @param group_id Число, являющееся номером столбца с группами пациентов
#' @param group_labels Строковый вектор, cодержащий имена групп. Эти имена будут сопоставлены в легенде графика
#' @param column_cat Число, являющееся номером столбца с категориями
#' @param path_pics_out Строка, содержащая путь, по которому будет сохранен файл с графиком
#' @param pic_name Строка, содержащая имя сохраняемого файла
#' @param p_display Решает, какие парные p-значения отображать.
#' Возможные значения: "s" (значимые [p <= 0.05]), "ns" (незначимые [p > 0.05]), "all" (все значения). (По умолчанию: "all").
#' @param p_adjust_method Решает, каким методом будут отрегулированы p-значения.
#' Возможные значения: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". (По умолчанию: "BH").
#' @return Объект класса ggplot2
#' @export
#' @examples
#' #Подключим библиотеку и загрузим датафрейм с категориями
#' library(statstestplot)
#' data(categories)
#'
#' #Распечатаем его
#' print(categories)
#'
#' #Построим график с параметрами по умолчанию
#' group_id <- 1
#' group_labels <- c("A", "B", "C")
#' column_cat <- 2
#' picture_name <- "Категории_по_умолчанию"
#' picture_path <- paste0(getwd(), "/")
#' cmp.categories(categories, group_id, group_labels, column_cat,
#'     picture_path, picture_name)
#'
#' #Построим график, отобразив только незначимые p и отрегулировав их методом поправки Бонферрони
#' group_id <- 1
#' group_labels <- c("A", "B", "C")
#' column_cat <- 2
#' p_display <- "ns"
#' p_adjust_method <- "bonferroni"
#' picture_name <- "Категории_незначимые_p_метод_Бонферрони"
#' picture_path <- paste0(getwd(), "/")
#' cmp.categories(categories, group_id, group_labels,
#'     column_cat, picture_path, picture_name, p_display, p_adjust_method)
cmp.categories <- function(data, group_id, group_labels, column_cat, path_pics_out, pic_name, p_display = "all", p_adjust_method = "BH") {
  #проверка входных параметров на корректность
  check_errors_in_param(data, group_id, group_labels, column_cat, path_pics_out, pic_name, p_display, p_adjust_method)
  #проверка на пустые значения в столбце с категориями
  incorrect_vars_and_grps <- check_columns("cmp.categories", data, group_id, group_labels, column_cat)
  #преобразование столбца с группами и категориями к факторам
  data[, group_id] <- factor(data[, group_id])
  data[, column_cat] <- factor(data[, column_cat])
  #запоминаем имена групп и категории категориального признака
  groups <- levels(data[, group_id])
  categories <- levels(data[, column_cat])
  #создание датафрейма, содержащего количество пациентов в данной группе, принадлежащих данной категории
  number_peoples <- table(data[, c(group_id, column_cat)])
  #присваивание неопределенных значений всем группам number_peoples с пустыми значениями в категориях 
  if (length(incorrect_vars_and_grps) > 0) {
    for (i in 1:length(incorrect_vars_and_grps)) {
      number_peoples[incorrect_vars_and_grps[[i]]$group, ] <- NA
    }
  }
  #создание датафрейма с данными для построения графика
  data_for_graphic <- get_data_for_building_graphic("cmp.categories", categories, groups, number_peoples)
  #создание датафрейма с p-значениями и координатами стрелок
  data_with_only_cat <- data[, c(group_id, column_cat)]
  arrow_positions <- get_p_values_and_arrow_coord("cmp.categories", data_with_only_cat, data_for_graphic, categories,
                         groups, incorrect_vars_and_grps, p_adjust_method)
  #фильтрация датафрейма со p-значениями в зависимости от режима их отображения
  if (p_display == "s") {
    arrow_positions <- dplyr::filter(arrow_positions, arrow_positions$p <= 0.05)
  } else if (p_display == "ns") {
    arrow_positions <- dplyr::filter(arrow_positions, arrow_positions$p > 0.05)
  }
  #инициализация графика
  graphic <- ggplot2::ggplot(data_for_graphic,
                             ggplot2::aes(x = data_for_graphic$indicators,
                                          y = data_for_graphic$mean,
                                          fill = data_for_graphic$groups))
  graphic <- graphic + ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE)
  #редактирование оси абсцисс
  graphic <- graphic + ggplot2::scale_x_discrete(name = colnames(data)[column_cat])
  #редактирование оси ординат
  lim_y <- 100
  arrow_positions_without_na <- stats::na.omit(arrow_positions)
  if (!plyr::empty(arrow_positions_without_na)) {
    if (max(arrow_positions_without_na$y) > 100) {
      lim_y <- max(arrow_positions_without_na$y) + 100
    }
  }
  graphic <- graphic + ggplot2::scale_y_continuous(name = NULL, breaks = seq(0, lim_y, 5),
                                                   limits = c(0, lim_y), labels = function (x) paste0(x, "%"))
  #изменение размера элементов легенды
  graphic <- graphic + ggplot2::theme(legend.key.size = grid::unit(0.7, "cm"))
  #редактирование заголовка легенды
  graphic <- graphic + ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  #редактирование названий групп в легенде
  peoples_total <- table(data[, group_id])
  for (i in 1:length(groups)) {
    group_labels[i] <- paste0(group_labels[i], "\n(n = ", peoples_total[i], ")")
  }
  graphic <- graphic + ggplot2::scale_fill_discrete(labels = group_labels)
  #вывод информации о количестве пациентов и проценте этих пациентов над столбцами
  percents <- as.character(data_for_graphic$mean)
  number_peoples_in_group <- as.character(data_for_graphic$x)
  labels_for_bars <- c()
  for (i in 1:length(percents)) {
    if (is.nan(data_for_graphic[i, 7]) && is.nan(data_for_graphic[i, 8])) {
      labels_for_bars <- append(labels_for_bars, "")
    }
    else {
      labels_for_bars <- append(labels_for_bars, paste0(percents[i], "%  ", "(", number_peoples_in_group[i], ")"))
    }
  }
  graphic <- graphic + ggplot2::geom_text(ggplot2::aes(label = labels_for_bars),
                                 vjust = -0.5,
                                 hjust = 0.54,
                                 position = ggplot2::position_dodge(0.9),
                                 size = 3,
                                 na.rm = TRUE)
  #добавление к графику доверительных интервалов для процента пациентов
  graphic <- graphic + ggplot2::geom_errorbar(ggplot2::aes(ymin = data_for_graphic$lower,
                                                           ymax = data_for_graphic$upper),
                                              width = 0.5,
                                              position = ggplot2::position_dodge(0.9),
                                              na.rm = TRUE)
  #добавление p-значений и стрелок на график
  if (!plyr::empty(arrow_positions)) {
    graphic <- graphic + ggsignif::geom_signif(y_position = arrow_positions$y,
                                               xmin = arrow_positions$x_min,
                                               xmax = arrow_positions$x_max,
                                               annotations = p_value_formatted(arrow_positions$p),
                                               vjust = 0,
                                               textsize = 3,
                                               na.rm = TRUE
                                               )
  }
  #сохранение графика в файл и вывод графика на экран
  save_pic_png_600(graphic, path_pics_out, pic_name)
  return (graphic)
}
