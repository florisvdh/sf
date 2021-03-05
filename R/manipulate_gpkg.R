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
#' For a \code{Date} object, time will be considered as \code{00:00:00} local
#' time.
#' Defaults to system time, however must be set explicitly for reproducible
#' workflows.
#' @param verbose Logical.
#' Prints a message with the number of affected rows in \code{gpkg_contents}.
#'
#' @return
#' \code{NULL} is returned invisibly.
#'
#' @examples
#' library(openssl)
#' md5sum <- function(x) paste(md5(file(x)))
#'
#' # Using existing geopackage with vector layer:
#' filepath <- system.file("gpkg/b_pump.gpkg", package = "sf")
#' (md5_original <- md5sum(filepath))
#'
#' sf_layer <- read_sf(system.file("gpkg/b_pump.gpkg", package = "sf"))
#'
#' # A rewrite changes the checksum:
#' filepath_notimeset <- file.path(tempdir(), "b_pump_notimeset.gpkg")
#'   # write 1:
#' st_write(sf_layer, dsn = filepath_notimeset)
#' (md5_notimeset1 <- md5sum(filepath_notimeset))
#'   # write 2:
#' st_write(sf_layer, dsn = filepath_notimeset, delete_dsn = TRUE)
#' (md5_notimeset2 <- md5sum(filepath_notimeset))
#'   # compare:
#' md5_notimeset1 == md5_notimeset2
#'
#' # Setting a fixed date/time
#' (fixed_date <- as.POSIXct("2020-12-25"))
#' (fixed_time <- as.POSIXct("2020-12-25 12:00:00", tz = "CET"))
#'
#' filepath_timeset <- file.path(tempdir(), "b_pump_timeset.gpkg")
#'   # write 1 (date):
#' st_write(sf_layer, dsn = filepath_timeset)
#' set_timestamp_gpkg(filepath_timeset, fixed_date)
#' md5_timeset1 <- md5sum(filepath_timeset)
#'   # write 2 (date):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' set_timestamp_gpkg(filepath_timeset, fixed_date)
#' md5_timeset2 <- md5sum(filepath_timeset)
#'   # compare:
#' all.equal(md5_timeset1, md5_timeset2)
#'   # write 3 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' set_timestamp_gpkg(filepath_timeset, fixed_time)
#' md5_timeset3 <- md5sum(filepath_timeset)
#'   # write 4 (time):
#' st_write(sf_layer, dsn = filepath_timeset, delete_dsn = TRUE)
#' set_timestamp_gpkg(filepath_timeset, fixed_time)
#' md5_timeset4 <- md5sum(filepath_timeset)
#'   # compare:
#' all.equal(md5_timeset3, md5_timeset4)
#'
#' # Also works for GPKG 2D gridded coverage (with stars):
#' library(stars)
#' library(dplyr)
#'
#' filepath_stars <- file.path(tempdir(), "stars_2d.gpkg")
#'
#' stars_2d <-
#' 	system.file("tif/L7_ETMs.tif", package = "stars") %>%
#' 	read_stars() %>%
#' 	slice(band, 1)
#'   # write 1:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' set_timestamp_gpkg(filepath_stars, fixed_time)
#' md5_stars1 <- md5sum(filepath_stars)
#'   # write 2:
#' stars_2d %>%
#' 	write_stars(filepath_stars, driver = "GPKG")
#' set_timestamp_gpkg(filepath_stars, fixed_time)
#' md5_stars2 <- md5sum(filepath_stars)
#'   # compare:
#' all.equal(md5_stars1, md5_stars2)
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
			" row(s) of the gpkg_contents table have been set with timestamp ",
			timestamp)
	}

	has_metadata <-
		nrow(RSQLite::dbGetQuery(con, "SELECT name FROM sqlite_master
						WHERE name == 'gpkg_metadata_reference'")) > 0
	if (has_metadata) {
		updatequery <-
			sprintf("UPDATE gpkg_metadata_reference SET timestamp = '%s'",
					timestamp)
		rows <- RSQLite::dbExecute(con, updatequery)
		if (verbose) {
			message(
				rows,
				" row(s) of the gpkg_metadata_reference table have ",
				"been set with timestamp ",
				timestamp)
		}
	}

	RSQLite::dbDisconnect(con)
	return(invisible(NULL))
}
