if (!require("dplyr", quietly = TRUE)) library(dplyr)
if (!require("stringr", quietly = TRUE)) library(stringr)

load_case_data <- function(source = "database", db_pool = NULL) {
  if (source == "database") {
    if (is.null(db_pool)) {
      stop("Database pool is required when source='database'")
    }
    return(load_case_data_from_db(db_pool))
  } else {
    stop("Excel data source is no longer supported. Use source='database'")
  }
}

load_case_data_from_db <- function(db_pool) {
  message("Loading data from PostgreSQL database (v_mpox_drc view)...")

  dat <- query_mpox_data(db_pool, view_name = "v_mpox_drc")

  message("Raw data loaded: ", nrow(dat), " rows and ", ncol(dat), " columns")

  # Map PostgreSQL columns to expected R data frame structure
  dat <- dat %>%
    mutate(
      # Sex mapping: PostgreSQL "Sex" -> case_sex
      case_sex = `Sex`,

      # Age mapping: Use "Age In Years", parse to numeric
      case_age = suppressWarnings(as.numeric(`Age In Years`)),

      # If Age In Years is missing but Date Of Birth exists, calculate age
      case_age = if_else(
        is.na(case_age) & !is.na(`Date Of Birth`),
        as.numeric(difftime(Sys.Date(), as.Date(`Date Of Birth`), units = "days") / 365.25),
        case_age
      ),

      # Province mapping: Use "Reporting Location (subnational)"
      province_division = `Reporting Location (subnational)`,

      # Additional useful fields
      case_id = `Case ID`,
      record_id = `Record ID`,
      case_classification = `Case Classification`,
      date_of_diagnosis = as.Date(`Date Of Diagnosis`),
      date_of_notification = as.Date(`DateOfNotification`),
      full_name = `Full Name`,
      province_iso = `Reporting Location (subnational) ISO`,
      province_iso_name = `Reporting Location (subnational) (ISO Name)`,
      date_of_birth = as.Date(`Date Of Birth`),
      status = `Status`
    )

  message("Column mapping completed")

  dat <- process_sex_variable(dat)
  dat <- process_age_variable(dat)
  dat <- process_age_group_variable(dat)
  dat <- process_province_variable(dat)

  message("After processing - unique sex values: ", paste(unique(dat$sex), collapse = ", "))
  message("Final dataset: ", nrow(dat), " rows with ", sum(!is.na(dat$sex)), " having sex data")

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
  } else if ("case_age" %in% names(dat)) {
    message("Creating age groups from case_age")
    dat <- dat %>%
      mutate(
        age_group = case_when(
          is.na(case_age) ~ NA_character_,
          case_age < 5 ~ "0-4",
          case_age < 10 ~ "5-9",
          case_age < 15 ~ "10-14",
          case_age < 20 ~ "15-19",
          case_age < 25 ~ "20-24",
          case_age < 30 ~ "25-29",
          case_age < 35 ~ "30-34",
          case_age < 40 ~ "35-39",
          case_age < 45 ~ "40-44",
          case_age < 50 ~ "45-49",
          TRUE ~ "50+"
        ),
        age_group = factor(age_group, levels = age_levels)
      )
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
