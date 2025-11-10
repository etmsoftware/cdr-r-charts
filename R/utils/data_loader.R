if (!require("dplyr", quietly = TRUE)) library(dplyr)
if (!require("stringr", quietly = TRUE)) library(stringr)
if (!require("lubridate", quietly = TRUE)) library(lubridate)

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
      # ============ DEMOGRAPHICS ============
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

      # ============ CASE IDENTIFIERS ============
      case_id = `Case ID`,
      record_id = `Record ID`,
      full_name = `Full Name`,
      province_iso = `Reporting Location (subnational) ISO`,
      province_iso_name = `Reporting Location (subnational) (ISO Name)`,

      # ============ DATES ============
      date_of_birth = as.Date(`Date Of Birth`),
      date_of_diagnosis = as.Date(`Date Of Diagnosis`),
      date_of_notification = as.Date(`DateOfNotification`),
      notification_date = as.Date(`DateOfNotification`),  # For R script compatibility
      symptom_onset_date = as.Date(`If Symptomatic, Date Of Symptoms Onset`),
      specimen_collection_date = as.Date(`Date Of Specimen Collection`),
      date_of_death = as.Date(`If Outcome Is Died, Report The Date Of Death`),
      discharge_or_death_date = as.Date(`Date Of Discharge/of Death`),

      # ============ CASE CLASSIFICATION ============
      case_classification = `Case Classification`,
      final_classification = `Final Case Classification`,
      final_classification_num = case_when(
        str_detect(str_to_lower(`Final Case Classification`), "suspect") ~ 1,
        str_detect(str_to_lower(`Final Case Classification`), "probabl") ~ 2,
        str_detect(str_to_lower(`Final Case Classification`), "confirm") ~ 3,
        str_detect(str_to_lower(`Final Case Classification`), "under.*invest") ~ 4,
        str_detect(str_to_lower(`Final Case Classification`), "not.*case") ~ 5,
        TRUE ~ NA_real_
      ),

      # ============ CASE STATUS / OUTCOME ============
      status = `Status`,
      case_status = `Status`,
      case_status_num = case_when(
        str_detect(str_to_lower(`Status`), "alive|vivant") ~ 1,
        str_detect(str_to_lower(`Status`), "deceased|dead|mort|décédé") ~ 2,
        TRUE ~ NA_real_
      ),

      # ============ LAB RESULTS ============
      lab_results = `Test Result`,
      lab_results_num = case_when(
        str_detect(str_to_lower(`Test Result`), "positif|positive|pos") ~ 1,
        str_detect(str_to_lower(`Test Result`), "negatif|négatif|negative|neg") ~ 2,
        str_detect(str_to_lower(`Test Result`), "indetermina|indéterminé") ~ 3,
        str_detect(str_to_lower(`Test Result`), "invalide|invalid") ~ 4,
        TRUE ~ NA_real_
      ),
      specimen_type = `Specimen For The Diagnosis`,
      clade_characterization = `Clade Characterization`,
      clade_type = `If Clade Characterization Is Yes, Which Clade?`,

      # ============ EXPOSURE / RISK FACTORS ============
      animal_contact = `Contact Animals`,
      animal_contact_num = case_when(
        str_detect(str_to_lower(`Contact Animals`), "yes|oui|y|1") ~ 1,
        str_detect(str_to_lower(`Contact Animals`), "no|non|n|0") ~ 0,
        TRUE ~ 2  # Don't know / missing
      ),
      animal_group = `If Contact Animals Is Yes, Which Group Of Animals`,
      animal_contact_type = `If Contact Animals Is Yes, Type Of Contact`,

      contact_with_person_with_lesions = `Contact With A Case`,
      contact_person_with_lesions_num = case_when(
        str_detect(str_to_lower(`Contact With A Case`), "yes|oui|y|1") ~ 1,
        str_detect(str_to_lower(`Contact With A Case`), "no|non|n|0") ~ 0,
        TRUE ~ 2  # Don't know / missing
      ),
      contact_frequency = `---How Often Did The Contact Occur?`,
      contact_location = `If Contact With A Case Is Yes, Where Did The Contact Occur?`,

      is_health_worker = `Is The Case A Health Worker?`,

      # ============ SYMPTOMS ============
      symptoms_present = `Symptoms`,
      symptoms_present_num = case_when(
        str_detect(str_to_lower(`Symptoms`), "yes|oui|y|1") ~ 1,
        str_detect(str_to_lower(`Symptoms`), "no|non|n|0") ~ 0,
        TRUE ~ 9  # Don't know
      ),
      symptoms_observed = `If Symptomatic, List Of Symptoms`,

      # ============ VACCINATION HISTORY ============
      previous_mpox_infection = `Previous Mpox Infection`,
      mpox_vaccination_dose = `Mpox Vaccination Dose`,
      vaccine1_date = as.Date(`If MonkeypoxVaccine1 Yes, Vaccination Date`),
      vaccine2_date = as.Date(`If MonkeypoxVaccine2 Yes, Vaccination Date`),

      # ============ TREATMENT ============
      intensive_care = `Intensive Care`,
      antiviral_treatment = `What Antiviral Treatment Is The Case Receiving For Mpox?`,

      # ============ ADMINISTRATIVE ============
      dictionary_id = `dictionaryId`,
      created_at = `Created At`,
      dictionary_version = `Dictionary Version`,
      comments = `Comments`,
      name_key = `name_key`
    )

  message("Column mapping completed")

  # Process standard variables
  dat <- process_sex_variable(dat)
  dat <- process_age_variable(dat)
  dat <- process_age_group_variable(dat)
  dat <- process_province_variable(dat)

  # Add derived temporal variables for epi curves
  dat <- dat %>%
    mutate(
      notification_month = floor_date(notification_date, "month"),
      month = month(notification_date),
      year = year(notification_date),
      iso_year = isoyear(notification_date),
      iso_week = isoweek(notification_date),
      week_start = floor_date(notification_date, unit = "week", week_start = 1)
    )

  message("After processing - unique sex values: ", paste(unique(dat$sex), collapse = ", "))
  message("Final dataset: ", nrow(dat), " rows with ", sum(!is.na(dat$sex)), " having sex data")

  return(dat)
}

process_sex_variable <- function(dat) {
  if ("case_sex_num" %in% names(dat)) {
    message("Processing case_sex_num column")
    dat <- dat %>%
      mutate(sex = factor(case_sex_num, levels = c(1, 2, NA),
                         labels = c("Male", "Female", "Unknown")))
  } else if ("case_sex" %in% names(dat)) {
    message("Processing case_sex column")
    dat <- dat %>%
      mutate(sex = str_trim(str_to_title(as.character(case_sex)))) %>%
      mutate(sex = recode(sex,
                          "M" = "Male", "F" = "Female",
                          "Masculin" = "Male", "Féminin" = "Female",
                          "Feminin" = "Female",
                          .default = sex)) %>%
      mutate(sex = case_when(
        is.na(sex) | sex == "" | sex == "Na" ~ "Unknown",
        TRUE ~ sex
      )) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female", "Unknown")))
  } else if ("sex" %in% names(dat)) {
    message("Sex column already exists, cleaning it")
    dat <- dat %>%
      mutate(sex = str_trim(str_to_title(as.character(sex)))) %>%
      mutate(sex = recode(sex,
                          "M" = "Male", "F" = "Female",
                          "Masculin" = "Male", "Féminin" = "Female",
                          "Feminin" = "Female",
                          .default = sex)) %>%
      mutate(sex = case_when(
        is.na(sex) | sex == "" | sex == "Na" ~ "Unknown",
        TRUE ~ sex
      )) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female", "Unknown")))
  } else {
    warning("No sex column found in the data. Available columns: ", paste(names(dat), collapse = ", "))
    dat$sex <- factor("Unknown", levels = c("Male", "Female", "Unknown"))
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
  # Calculate actual age range from data (default to 0-110 if no age data)
  age_min <- if(all(is.na(dat$case_age))) 0 else floor(min(dat$case_age, na.rm = TRUE))
  age_max <- if(all(is.na(dat$case_age))) 110 else ceiling(max(dat$case_age, na.rm = TRUE))

  list(
    provinces = sort(unique(dat$province[!is.na(dat$province) &
                                           dat$province != ""])),
    sexes = c("All", "Male", "Female", "Unknown"),
    age_range = c(max(0, age_min), age_max)  # Ensure min is at least 0
  )
}
