.asUTF8 <- function (x)
{
    if (all(i <- validUTF8(x)))
        Encoding(x) <- "UTF-8"
    else {
        Encoding(x) <- ifelse(i, "UTF-8", "latin1")
        x[!i] <- enc2utf8(x[!i])
    }
    x
}


asUTF8 <- function (x)
{
    if (any(i <- Encoding(x) == "unknown"))
        x[i] <- .asUTF8(x[i])
    x
}


as.exiftool.args <- function (x)
{
    if (any(i <- grep("(^#)|\n|\r", x))) {
        y <- x[i]
        y <- gsub("\\", "\\\\", y, fixed = TRUE)
        y <- gsub("\n", "\\n", y, fixed = TRUE)
        y <- gsub("\r", "\\r", y, fixed = TRUE)
        x[i] <- paste0("#[CSTR]", y)
    }
    x
}


delayedAssign("exiftool.path", {
    system.file(
        package = .packageName,
        "exec",
        if (.Platform$OS.type == "windows") "exiftool.exe" else "exiftool",
        mustWork = TRUE
    )
})


exiftool <- function (...)
{
    text <- c(character(), list(...), recursive = TRUE, use.names = FALSE)
    text <- as.exiftool.args(asUTF8(text))


    tmp.file <- tempfile(fileext = ".args")
    on.exit(unlink(tmp.file), add = TRUE)
    writeLines(text, tmp.file, useBytes = TRUE)
    # cat(readLines(tmp.file, encoding = "UTF-8"), sep = "\n")


    args <- c(exiftool.path, "-@", tmp.file)
    args <- essentials::shEncode(args)
    command <- paste(args, collapse = " ")
    asUTF8(suppressWarnings(system(command, intern = TRUE, ignore.stderr = TRUE)))
}


read.MusicMetadata <- function (file)
{
    file <- path.expand(file)
    if (length(file) == 1 &&
        file.exists(file) &&
        !dir.exists(file) &&
        !grepl("\\.(m4a|mp3)$", file, ignore.case = TRUE))
        stop(errorCondition("'read.MusicMetadata' does not yet support saving and loading music meta information",
            class = "NotYetImplementedError", call = sys.call(sys.nframe())))


    x <- exiftool(
        "-charset"  , "FileName=UTF-8",
        "-short"    ,         # short output format
        "-tab"      ,         # output in tab-delimited list format,
        "-extension", "m4a",  # process files with m4a extension
        "-extension", "mp3",  # process files with mp3 extension


        # include -- before the files so that files which
        # start with - or -- are not interpreted as options
        "--", file
    )


    # if 0 files are found
    if (identical(x, structure(character(), status = 1L)))
        return(as.MusicMetadata())


    # if the last element is:
    # "n directories scanned"
    # "n image files read",
    # "n files could not be read"
    # remove them from 'value'
    while (grepl("^[[:blank:]]*[[:digit:]]+ (director(y|ies) scanned|image files? read|files? could not be read)[[:blank:]]*$", x[length(x)])) {
        x <- x[-length(x)]
    }


    # when multiple files are found, they are listed like:
    #
    # ======== path1
    # metadata for path 1
    # ...
    # ======== path2
    # metadata for path 2
    # ...
    #
    # and so on. if we cannot find "========" at the start, then only one file
    # was found, and we do something special
    pattern <- "^======== "
    i <- grep(pattern, x)


    .format <- function(xx) {
        m <- regexpr("\t", xx, fixed = TRUE, useBytes = TRUE)
        if (any(invalid <- m == -1L))
            stop(errorCondition(sprintf(ngettext(sum(invalid),
                "unexpected item found: %s",
                "unexpected items found:\n    %s"),
                paste(dQuote(xx[invalid]), collapse = "\n    ")),
                call = sys.call(-1L)))
        value <- substr(xx, m + 1L, 1000000L)
        names(value) <- substr(xx, 1L, m - 1L)
        value
    }


    # if 1 file is found
    if (length(i) <= 0) {
        value <- .format(x)
        value <- structure(list(value), names = file.path(value[["Directory"]], value[["FileName"]]))


    # if multiple files are found
    } else {
        from <- i + 1L
        to   <- c(i[-1L] - 1L, length(x))
        value <- essentials::plapply(list(from, to), function(from, to) {
            .format(x[essentials:::seq2(from, to, by = 1L)])
        })
        names(value) <- sub(pattern, "", x[i])


    }


    names(value) <- normalizePath(names(value), winslash = "/", mustWork = FALSE)
    return(as.MusicMetadata(value))
}
