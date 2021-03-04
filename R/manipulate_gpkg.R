#' Set explicit timestamp in a GeoPackage 'gpkg_contents' table
#'
#' Overwrites all timestamps (column \code{last_change}) of the
#' \code{gpkg_contents} table in an existing GeoPackage file.
#' As such the function assists in making a binary-reproducible GeoPackage file.
#'
#' Internally the timestamp is converted to a specific ISO 8601 format
#' that is required by the GeoPackage standard.
#'
#' @param dsn the path to the GeoPackage file (*.gpkg)
#' @param timestamp a \code{Date} or \code{POSIXct} object, used to generate
#' the timestamp.
#' For a \code{Date} object, time will be considered as 00:00:00 local time.
#' Defaults to system time, however must be set explicitly for reproducible
#' workflows.
#' @param verbose Logical.
#' Prints a message with the number of affected rows in \code{gpkg_contents}.
#'
#' @return
#' Value \code{0} is returned invisibly.
#'
#' @author Floris Vanderhaeghe, \url{https://github.com/florisvdh}
#'
#' @export
set_timestamp_gpkg <- function(dsn,
							   timestamp = Sys.time(),
							   verbose = TRUE) {
	stopifnot(file.exists(dsn))
	stopifnot(is.logical(verbose), !is.na(verbose))
	# soft checking file format:
	if (!grepl("\\.gpkg$", dsn)) {
		stop("Expecting a file with extension '.gpkg'")
	}
	if (!inherits(timestamp, c("Date", "POSIXct"))) {
		stop("timestamp must be a Date or POSIXct object")
	}

	if (!requireNamespace("RSQLite", quietly = TRUE)) {
		stop("Package \"RSQLite\" is needed when using this function. ",
			 "Please install it.",
			 call. = FALSE)
	}

	timestamp <- format(timestamp,
						format = "%Y-%m-%dT%H:%M:%S.000Z",
						tz = "UTC")

	con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
	updatequery <- sprintf("UPDATE gpkg_contents SET last_change = '%s'",
						   timestamp)
	rows <- RSQLite::dbExecute(con, updatequery)
	if (verbose) {
		message(
			rows,
			" rows of the gpkg_contents table have been set with timestamp ",
			timestamp)
	}
	RSQLite::dbDisconnect(con)
	return(invisible(0))
}
