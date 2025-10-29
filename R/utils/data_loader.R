load_case_data <- function(file_path = "data/complete_case_dataset_sim_final.xlsx") {

  if (!file.exists(file_path)) {
    stop(paste("Data file not found:", file_path,
               "\nWorking directory:", getwd(),
               "\nAvailable files:", paste(list.files(".", recursive = TRUE), collapse = ", ")))
  }

  message("Loading data from: ", file_path)
  dat <- read_excel(file_path)
  message("Loaded ", nrow(dat), " rows and ", ncol(dat), " columns")
  message("Column names: ", paste(names(dat), collapse = ", "))

  dat <- process_sex_variable(dat)

  dat <- process_age_variable(dat)

  dat <- process_age_group_variable(dat)

  dat <- process_province_variable(dat)

  message("After processing - unique sex values: ", paste(unique(dat$sex), collapse = ", "))

  return(dat)
}

process_sex_variable <- function(dat) {
  if ("case_sex_num" %in% names(dat)) {
    message("Processing case_sex_num column")
    dat <- dat %>%
      mutate(sex = factor(case_sex_num, levels = c(1, 2),
                         labels = c("Male", "Female")))
  } else if ("case_sex" %in% names(dat)) {
    message("Processing case_sex column")
    dat <- dat %>%
      mutate(sex = str_trim(str_to_title(as.character(case_sex)))) %>%
      mutate(sex = recode(sex,
                          "M" = "Male", "F" = "Female",
                          "Masculin" = "Male", "Féminin" = "Female",
                          "Feminin" = "Female",
                          .default = sex)) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female")))
  } else if ("sex" %in% names(dat)) {
    message("Sex column already exists, cleaning it")
    dat <- dat %>%
      mutate(sex = str_trim(str_to_title(as.character(sex)))) %>%
      mutate(sex = recode(sex,
                          "M" = "Male", "F" = "Female",
                          "Masculin" = "Male", "Féminin" = "Female",
                          "Feminin" = "Female",
                          .default = sex)) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female")))
  } else {
    warning("No sex column found in the data. Available columns: ", paste(names(dat), collapse = ", "))
    dat$sex <- NA_character_
  }
  return(dat)
}

process_age_variable <- function(dat) {
  if ("case_age" %in% names(dat)) {
    dat <- dat %>%
      mutate(case_age = suppressWarnings(as.numeric(case_age)))
  }
  return(dat)
}

process_age_group_variable <- function(dat) {
  age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                  "25-29", "30-34", "35-39", "40-44", "45-49", "50+")

  if ("age_group" %in% names(dat)) {
    dat <- dat %>%
      mutate(age_group = str_replace_all(age_group, "–", "-"),
             age_group = factor(age_group, levels = age_levels))
  } else if ("age_group_num" %in% names(dat)) {
    dat <- dat %>%
      mutate(age_group = factor(age_group_num, levels = 1:11,
                                labels = age_levels))
  }
  return(dat)
}

process_province_variable <- function(dat) {
  if ("province_division" %in% names(dat)) {
    message("Processing province_division column")
    dat <- dat %>%
      mutate(province = str_squish(as.character(province_division)))
  } else if ("province_division_num" %in% names(dat)) {
    message("Processing province_division_num column")
    dat <- dat %>%
      mutate(province = as.character(province_division_num))
  } else if ("province" %in% names(dat)) {
    message("Province column already exists")
    dat <- dat %>%
      mutate(province = str_squish(as.character(province)))
  } else {
    warning("No province column found in the data")
    dat$province <- NA_character_
  }
  return(dat)
}

get_filter_options <- function(dat) {
  list(
    provinces = sort(unique(dat$province[!is.na(dat$province) &
                                           dat$province != ""])),
    sexes = c("All", "Male", "Female"),
    age_range = c(0, 110)
  )
}
