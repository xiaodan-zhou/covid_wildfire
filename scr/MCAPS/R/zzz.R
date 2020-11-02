.dbEnv <- new.env()

.onLoad <- function(lib, pkg) {
    if(!require(stashR, quietly = TRUE))
        stop("'stashR' package required")
    stashROption("quietDownload", TRUE)
}

.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (%s %s)", dcf[, "Title"],
                    as.character(dcf[, "Version"]), dcf[, "Date"])
    message(paste(strwrap(msg), collapse = "\n"))
    message(paste(strwrap(gettext("Initialize database using 'initMCAPS'")),
                  collapse = "\n"))
}

