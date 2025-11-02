# ===============================================================
# Database Connection Utilities
# ===============================================================

#' Load environment variables from .env file (for local development only)
#' Server environment variables (e.g., from Posit Connect) take precedence
#' @return NULL (sets environment variables)
load_env <- function() {
  env_file <- ".env"

  if (file.exists(env_file)) {
    message("Loading environment variables from .env file (local development)")
    lines <- readLines(env_file)

    lines <- lines[!grepl("^\\s*#", lines) & nchar(trimws(lines)) > 0]

    for (line in lines) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        key <- trimws(parts[1])
        value <- trimws(parts[2])

        # Only set if the environment variable doesn't already exist
        # This allows server environment variables to take precedence
        if (Sys.getenv(key, unset = "") == "") {
          do.call(Sys.setenv, setNames(list(value), key))
        } else {
          message("  - ", key, " already set by server (using server value)")
        }
      }
    }
  } else {
    message("No .env file found. Using server environment variables or config.yml defaults.")
  }
}

#' Create a database connection pool
#' @param config_env Environment name (development, production). Defaults to R_CONFIG_ACTIVE env var
#' @return A pool object
#' @export
create_db_pool <- function(config_env = NULL) {

  load_env()

  if (is.null(config_env)) {
    config_env <- Sys.getenv("R_CONFIG_ACTIVE", "development")
  }

  message("Loading database configuration for environment: ", config_env)

  tryCatch({
    cfg <- config::get(config = config_env, file = "config.yml")
    db_config <- cfg$database

    message("Attempting to connect to PostgreSQL database...")
    message("Host: ", db_config$host)
    message("Port: ", db_config$port)
    message("Database: ", db_config$dbname)
    message("User: ", db_config$user)

    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      host = db_config$host,
      port = db_config$port,
      dbname = db_config$dbname,
      user = db_config$user,
      password = db_config$password,
      minSize = 1,
      maxSize = db_config$pool_size
    )

    message("Database connection pool created successfully!")

    conn <- pool::poolCheckout(pool)
    tables <- DBI::dbListTables(conn)
    message("Connected! Available tables/views: ", length(tables))
    pool::poolReturn(conn)

    return(pool)

  }, error = function(e) {
    stop(paste("Failed to create database connection pool:", e$message,
               "\n\nPlease check:",
               "\n1. Database credentials in config.yml or .env file",
               "\n2. PostgreSQL server is running and accessible",
               "\n3. Network connectivity to database host",
               "\n4. User has proper permissions"))
  })
}

#' Test database connection
#' @param pool Database connection pool
#' @return TRUE if connection is valid, FALSE otherwise
test_db_connection <- function(pool) {
  tryCatch({
    conn <- pool::poolCheckout(pool)
    result <- DBI::dbGetQuery(conn, "SELECT 1 as test")
    pool::poolReturn(conn)

    if (result$test == 1) {
      message("Database connection test: SUCCESS")
      return(TRUE)
    } else {
      warning("Database connection test: FAILED - Unexpected result")
      return(FALSE)
    }
  }, error = function(e) {
    warning("Database connection test: FAILED - ", e$message)
    return(FALSE)
  })
}

#' Query data from v_mpox_drc view
#' @param pool Database connection pool
#' @param view_name Name of the view (default: v_mpox_drc)
#' @param limit Optional row limit for testing
#' @return Data frame with case data
#' @export
query_mpox_data <- function(pool, view_name = "v_mpox_drc", limit = NULL) {

  message("Querying data from view: ", view_name)

  tryCatch({
    # Build query
    query <- sprintf("SELECT * FROM %s", view_name)

    if (!is.null(limit)) {
      query <- paste(query, "LIMIT", limit)
      message("Limiting query to ", limit, " rows for testing")
    }

    # Execute query
    start_time <- Sys.time()
    conn <- pool::poolCheckout(pool)
    data <- DBI::dbGetQuery(conn, query)
    pool::poolReturn(conn)
    end_time <- Sys.time()
    query_time <- round(difftime(end_time, start_time, units = "secs"), 2)

    message("Query completed successfully!")
    message("Retrieved ", nrow(data), " rows and ", ncol(data), " columns")
    message("Query execution time: ", query_time, " seconds")
    message("Column names: ", paste(head(names(data), 10), collapse = ", "), "...")

    return(data)

  }, error = function(e) {
    stop(paste("Failed to query data from", view_name, ":", e$message))
  })
}

#' Close database connection pool
#' @param pool Database connection pool
#' @return NULL
#' @export
close_db_pool <- function(pool) {
  if (!is.null(pool)) {
    message("Closing database connection pool...")
    pool::poolClose(pool)
    message("Database connection pool closed.")
  }
}

#' Get database connection info
#' @param pool Database connection pool
#' @return List with connection information
get_db_info <- function(pool) {
  tryCatch({
    conn <- pool::poolCheckout(pool)

    info <- list(
      database = DBI::dbGetInfo(conn)$dbname,
      user = DBI::dbGetInfo(conn)$user,
      host = DBI::dbGetInfo(conn)$host,
      port = DBI::dbGetInfo(conn)$port,
      protocol_version = DBI::dbGetInfo(conn)$protocol.version,
      server_version = DBI::dbGetInfo(conn)$server.version
    )

    pool::poolReturn(conn)
    return(info)

  }, error = function(e) {
    warning("Failed to get database info: ", e$message)
    return(NULL)
  })
}
