

# give the user a message declaring what is going on ----


cat("\n")
cat("   ", strrep("*", 46), "\n", sep = "")

cat("   Downloading ExifTool by Phil Harvey\n   from ")
cat(hostname <- "https://exiftool.org", "\n")
cat(strwrap("ExifTool is a platform-independent application for reading, writing, and editing meta information",
    indent = 3, exdent = 3), sep = "\n")

cat("   ", strrep("*", 46), "\n\n", sep = "")


# download the zip or tarball, extract the tool, and finish! ----


exdir <- "exec"


if (.Platform$OS.type == "windows") {


    url <- paste(hostname, "exiftool-12.30.zip", sep = "/")
    utils::download.file(url, destfile <- tempfile(fileext = ".zip"))
    utils::unzip(destfile, files = "exiftool(-k).exe", exdir = exdir)
    if (!file.rename(
        file.path(exdir, "exiftool(-k).exe"),
        file.path(exdir, "exiftool.exe")
    )) {
        stop(gettextf("unable to rename %s to %s",
            dQuote(file.path(exdir, "exiftool(-k).exe")),
            dQuote(file.path(exdir, "exiftool.exe"))))
    }
    if (!file.exists(file.path(exdir, "exiftool.exe")))
        stop(gettextf("unable to find %s",
            dQuote(file.path(exdir, "exiftool.exe"))))


} else {


    filename <- "Image-ExifTool-12.30"
    url <- paste(hostname, paste0(filename, ".tar.gz"), sep = "/")
    utils::download.file(url, destfile <- tempfile(fileext = ".tar.gz"))
    utils::untar(destfile, files = file.path(filename, "exiftool"), exdir = exdir)
    if (!file.rename(
        file.path(exdir, filename, "exiftool"),
        file.path(exdir, "exiftool")
    )) {
        stop(gettextf("unable to move %s from %s to %s",
            dQuote("exiftool"),
            dQuote(file.path(exdir, filename)),
            dQuote(exdir)))
    }
    unlink(file.path(exdir, filename), recursive = TRUE, force = TRUE)
    if (!file.exists(file.path(exdir, "exiftool")))
        stop(gettextf("unable to find %s",
            dQuote(file.path(exdir, "exiftool"))))


}
