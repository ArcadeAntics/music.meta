

exiftool.version <- "12.41"


download_exiftool <- function (verbose = getOption("verbose"))
{
    hostname <- "https://exiftool.org"


    # give the user a message declaring what is going on ----
    if (verbose) {
        cat("\n")
        cat("   ", strrep("*", 46), "\n", sep = "")

        cat("   Downloading ExifTool by Phil Harvey\n   from ")
        cat(hostname, "\n")
        cat(strwrap("ExifTool is a platform-independent application for reading, writing, and editing meta information",
            indent = 3, exdent = 3), sep = "\n")

        cat("   ", strrep("*", 46), "\n\n", sep = "")
    }


    # download the zip or tarball, extract the tool, and finish! ----


    exdir <- "exec"


    if (.Platform$OS.type == "windows") {
        url <- sprintf("%s/exiftool-%s.zip", hostname, exiftool.version)
        destfile <- tempfile(fileext = ".zip")
        on.exit(unlink(destfile))
        utils::download.file(url, destfile, quiet = !verbose)
        utils::unzip(destfile, files = "exiftool(-k).exe", exdir = exdir)
        from <- file.path(exdir, "exiftool(-k).exe")
        to   <- file.path(exdir, "exiftool.exe")


    } else {
        filename <- sprintf("Image-ExifTool-%s", exiftool.version)
        url <- sprintf("%s/%s.tar.gz", hostname, filename)
        destfile <- tempfile(fileext = ".tar.gz")
        on.exit(unlink(destfile))
        utils::download.file(url, destfile, quiet = !verbose)
        on.exit(unlink(file.path(exdir, filename), recursive = TRUE))
        utils::untar(destfile, files = file.path(filename, "exiftool"), exdir = exdir)
        from <- file.path(exdir, filename, "exiftool")
        to   <- file.path(exdir, "exiftool")


    }


    if (!file.rename(from, to))
        stop(gettextf("unable to rename %s to %s",
            dQuote(from), dQuote(to)))
    if (!file.exists(to))
        stop(gettextf("unable to find %s", dQuote(to)))
}


download_exiftool(verbose = TRUE)
