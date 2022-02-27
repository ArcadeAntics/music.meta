


exiftool <- function (
    path,
    options = c(
        "-short"    ,         # short output format
        "-tab"      ,         # output in tab-delimited list format,
        "-extension", "m4a",  # process files with m4a extension
        "-extension", "mp3"   # process files with mp3 extension
    ))
{
    exiftool <- system.file(
        package = .packageName,
        "exec",
        if (.Platform$OS.type == "windows") "exiftool.exe" else "exiftool",
        mustWork = TRUE
    )
    path <- path.expand(path)
    if (anyNA(path))
        stop("invalid 'path'")
    command <- paste(c(
        essentials::shEncode(exiftool),
        essentials::shEncode(options),
        essentials::shEncode(path)
    ), collapse = " ")
    value <- suppressWarnings(system(command, intern = TRUE))
    Encoding(value) <- "UTF-8"
    Encoding(value[!validUTF8(value)]) <- "latin1"


    # if 0 files are found
    if (identical(value, structure("No matching files", status = 1L)))
        return(structure(list(), names = character()))


    # if the last element is " n image files read " or " n directories scanned",
    # remove them from 'value'
    if (grepl("^ *[[:digit:]]+ image files read *$", value[length(value)]))
        value <- value[-length(value)]
    if (grepl("^ *[[:digit:]]+ directories scanned *$", value[length(value)]))
        value <- value[-length(value)]


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
    i <- grep(pattern, value)


    .format <- function(x) {
        m <- regexpr("\t", x, fixed = TRUE, useBytes = TRUE)
        if (any(invalid <- m == -1L))
            stop(errorCondition(sprintf(ngettext(sum(invalid),
                "unexpected item found: %s",
                "unexpected items found:\n    %s"),
                paste(dQuote(x[invalid]), collapse = "\n    ")),
                call = sys.call(-1L)))
        value <- substr(x, m + 1L, 1000000L)
        names(value) <- substr(x, 1L, m - 1L)
        value
    }


    # if 1 file is found
    if (length(i) <= 0) {


        value2 <- .format(value)
        value2 <- structure(list(value2), names = file.path(value2[["Directory"]], value2[["FileName"]]))


    # if multiple files are found
    } else {

        from <- i + 1L
        to   <- c(i[-1L] - 1L, length(value))
        value2 <- essentials::plapply(
            X = list(
                from = from,
                to   = to
            ),
            FUN = function(x, from, to) .format(x[essentials:::seq2(from, to, by = 1L)]),
            x = value
        )
        names(value2) <- sub(pattern, "", value[i])


    }


    names(value2) <- normalizePath(names(value2), winslash = "/", mustWork = FALSE)
    value2
}


readMusicMetadata <- function (path)
exiftool(path = path)
