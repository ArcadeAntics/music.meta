cat("\n")
cat("   ", strrep("*", 46), "\n", sep = "")

cat("   Downloading ExifTool by Phil Harvey\n   from: ")
cat(URL <- "https://exiftool.org", "\n")
cat(strwrap("ExifTool is a platform-independent application for reading, writing, and editing meta information",
    indent = 3, exdent = 3), sep = "\n")

cat("   ", strrep("*", 46), "\n\n", sep = "")


if (.Platform$OS.type == "windows") {
    BASE <- "exiftool-12.30.zip"
    URL <- paste0(URL, "/", BASE)
    DIR <- tempdir()
    FILE <- file.path(DIR, BASE)
    utils::download.file(URL, FILE)
    utils::unzip(FILE, files = "exiftool(-k).exe", exdir = "tools")
    if (!file.rename("tools/exiftool(-k).exe", "exiftool.exe"))
        stop("unable to rename 'exiftool(-k).exe' to 'exiftool.exe'")
} else {
    BASE <- "Image-ExifTool-12.30"
    URL <- paste0(URL, "/", BASE, ".tar.gz")
    DIR <- tempdir()
    FILE <- file.path(DIR, paste0(BASE, ".tar.gz"))
    utils::download.file(URL, FILE)
    utils::untar(FILE,
        files = file.path(BASE, "exiftool"),
        exdir = "tools")
    file.copy(
        from = file.path("tools", BASE, "exiftool"),
        to   = file.path("exiftool")
    )
    unlink(file.path("tools", BASE), recursive = TRUE)
}
# file.open("tools")
