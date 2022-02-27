

# class definition ----


MusicMetadata <- methods::setRefClass(
    Class  = "MusicMetadata",
    fields = c(value = "data.frame"),
    methods = list(


has.title = function ()
.self$value$title != "",


has.artist = function ()
.self$value$artist != "",


has.genre = function ()
.self$value$genre != "",


has.album = function ()
.self$value$album != "",


has.album.artist = function ()
.self$value$album.artist != "",


has.composer = function ()
.self$value$composer != "",


is.single = function ()
.self$has.title() & .self$value$title == .self$value$album &
    .self$has.album.artist() &
    sub("/.*", "", .self$value$album.artist) == sub("/.*", "", .self$value$artist) &
    .self$value$track.number == 1L & .self$value$track.count == 1L &
    .self$value$disk.number == 1L & .self$value$disk.count == 1L,


get.title = function ()
fix.title(.self$value$title),


as.filename = function (short = FALSE)
{
    # .self <- value2
    if (short) {
        val <- .self$get.title()
        i <- nzchar(val)
        val[i] <- paste0(val[i], essentials::ext(names(.self)[i]))
        i <- !.self$has.album.artist() | .self$has.composer() | .self$is.single()
        val[i] <- .self[i]$as.filename(short = FALSE)
        val
    }
    else {
        val <- ifelse(
            .self$has.title(),
            ifelse(
                .self$has.composer(),
                .self$value$composer,
                ifelse(
                    .self$has.artist(),
                    .self$value$artist,
                    ""
                )
            ),
            ""
        )
        i <- nzchar(val)
        val[i] <- paste0(fix.artists(val[i]), "_", recycle0 = TRUE)
        val <- paste0(val, .self$get.title())
        i <- nzchar(val)
        val[i] <- paste0(val[i], essentials::ext(basename(names(.self)[i])))
        val
    }
},


validate.filename = function (original.artist.check = TRUE)
{
    # .self <- value2 ; stop("comment this out later", immediate. = TRUE)


    askYesNo2 <- function(msg, add = TRUE) {
        `%until%` <- essentials::`%until%`
        if (add)
            msg <- paste(msg, "(Yes/No/Cancel): ")
        do ({
            val <- tolower(substr(sub("^\\s+", "", readline(msg)), 1L, 1L))
        }) %until% (val %in% c("y", "n", "c"))
        switch(val, y = TRUE, n = FALSE, c = NA, stop("invalid 'value'; should never happen, please report!"))
    }


    keep <- rep(TRUE, length(.self))
    x <- data.frame(
        path = names(.self),
        folder = dirname(names(.self)),
        extension = essentials::ext(names(.self)),
        old = basename(names(.self)),
        new = .self$as.filename(),
        self = .self,
        indx = seq_along(.self)
    )


    i <- x$new == ""
    if (any(i)) {
        keep[i] <- FALSE
        warning(
            sprintf(
                ngettext(
                    sum(i),
                    "unable to make filename for: %s",
                    "unable to make filenames for:\n    %s"
                ),
                paste(dQuote(x$path[i]), collapse = "\n    ")
            )
        )
        x <- x[!i, , drop = FALSE]
    }
    # cat(as.character(x$.self), sep = "\n\n")


    i <- x$old == x$new
    if (any(i)) {
        keep[i] <- FALSE
        x <- x[!i, , drop = FALSE]
    }


    i <- tolower(x$old) == tolower(x$new)
    if (any(i)) {
        keep[i] <- FALSE
        file.rename(
            x$path[i],
            file.path(x$folder[i], x$new[i])
        )
        x <- x[!i, , drop = FALSE]
    }


    for (i in seq_len(nrow(x))) {
        cat("For file:\n", x$char[[i]], sep = "")
        ans <- askYesNo2(paste0("Rename as ", dQuote(x$new[[i]]), "?"))
        if (is.na(ans))
            return(invisible())
        if (ans) {
            to <- file.path(x$folder[[i]], x$new[[i]])
            if (cond <- !file.rename(x$path[[i]], to))
                stop(gettextf("unable to rename\nfrom %s\nto   %s",
                    dQuote(x$path[[i]]), dQuote(to)))
            names(.self)[[x$indx[[i]]]] <- to
        }
    }
    invisible()
},


# as.filename = function (short = FALSE)
# {
#     if (short) {
#         val <- paste0(.self$get.title(), essentials::ext(names(.self)))
#         def <- .self$as.filename()
#         i <- def == "" | def == val
#         def[i] <- basename(names(.self)[i])
#         i <- .self$has.composer() | .self$is.single() | !.self$has.album.artist()
#         val[i] <- def[i]
#         val
#     }
#     else {
#         val <- character(length(.self))
#         orig <- !.self$is.remix()
#         blank <- !.self$has.title() | (orig & !.self$has.artist())
#         if (any(i <- orig & !blank))
#             val[i] <- paste0(gsub("/", ", ", .self$value$artist[i], fixed = TRUE), " - ")
#         if (any(i <- !blank))
#             val[i] <- paste0(val[i], .self$value$title[i], essentials::ext(names(.self)[i]))
#         essentials::asWindowsbasename(val)
#     }
# },


CheckInconsistencies = function (what, warn = TRUE)
{
    what <- match.arg(what, c("artist", "date", "track.count", "disk.count"))
    f <- switch (what,
        artist = return(.self$CheckArtistInconsistencies(warn = warn)),
        track.count = paste0(.self$value$album.artist, " - ", .self$value$album, " - Disk ", .self$value$disk.number, recycle0 = TRUE),
                      paste0(.self$value$album.artist, " - ", .self$value$album                                     , recycle0 = TRUE)
    )
    i <- vapply(split(.self$value[[what]], f), function(xx) {
        length(unique(xx))
    }, FUN.VALUE = 0L) == 1L
    i <- unsplit(i, f)
    f[i] <- NA
    x <- split(.self, f)
    if (warn) {
        if (length(x)) {
            k <- vapply(x, function(xx) {
                paste(dQuote(unique(xx$value[[what]])), collapse = ", ")
            }, FUN.VALUE = "")
            warning(
                sprintf(
                    ngettext(
                        length(x),
                        "found %d %s inconsistency",
                        "found %d %s inconsistencies"
                    ),
                    length(x), what
                ),
                paste0("\n  in ", unstrip(dQuote(names(k))), " found: ", k))
        }
        invisible(x)
    }
    else x
},


CheckDateInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies(what = "date", warn = warn)
},


CheckTrackCountInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies(what = "track.count", warn = warn)
},


CheckDiskCountInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies(what = "disk.count", warn = warn)
},


CheckArtistInconsistencies = function (warn = TRUE)
{
    artists1 <- strsplit(.self$value$artist      , "/", fixed = TRUE)
    artists2 <- strsplit(.self$value$album.artist, "/", fixed = TRUE)
    x <- data.frame(
        indexes = c(
            rep(seq_along(.self), lengths(artists1)),
            rep(seq_along(.self), lengths(artists2))),
        artists = c(unlist(artists1), unlist(artists2))
    )
    # x <- x[!duplicated(x$artists), , drop = FALSE]
    x <- split(x,
        f = factor(
            tolower(x$artists),
            levels = unique(tolower(x$artists)),
            labels = x$artists[!duplicated(tolower(x$artists))]
        ))
    x <- x[vapply(x, function(xx) length(unique(xx$artists)), FUN.VALUE = 0L) != 1L]
    y <- lapply(x, function(xx) {
        .self[unique(xx$indexes)]
    })
    if (warn) {
        if (length(x)) {
            k <- vapply(x, function(xx) {
                paste(dQuote(unique(xx$artists)), collapse = ", ")
            }, FUN.VALUE = "")
            warning(
                sprintf(
                    ngettext(
                        length(x),
                        "found %d artist inconsistency",
                        "found %d artist inconsistencies"
                    ),
                    length(x)
                ),
                paste0("\n  for ", unstrip(dQuote(names(k))), ", found: ", k)
            )
        }
        invisible(y)
    }
    else y
},


CheckGenreInconsistencies = function (warn = TRUE)
{
    x <- data.frame(
        indexes = seq_along(.self),
        genres = .self$value$genre
    )
    x <- x[x$genres != "", , drop = FALSE]
    x <- split(x, factor(
        tolower(x$genres),
        levels = unique(tolower(x$genres)),
        labels = x$genres[!duplicated(tolower(x$genres))]
    ))
    x <- x[vapply(x, function(xx) length(unique(xx$genres)), FUN.VALUE = 0L) != 1L]
    y <- lapply(x, function(xx) {
        .self[unique(xx$indexes)]
    })
    if (warn) {
        if (length(x)) {
            k <- vapply(x, function(xx) {
                paste(dQuote(unique(xx$genres)), collapse = ", ")
            }, FUN.VALUE = "")
            warning(
                sprintf(
                    ngettext(
                        length(x),
                        "found %d artist inconsistency",
                        "found %d artist inconsistencies"
                    ),
                    length(x)
                ),
                paste0("\n  for ", unstrip(dQuote(names(k))), ", found: ", k)
            )
        }
        invisible(y)
    }
    else y
},


CheckAlbumInconsistencies = function (warn = TRUE)
{
    x <- data.frame(
        indexes = seq_along(.self),
        albums = paste(.self$value$album.artist, .self$value$album, sep = " - ")
    )
    x <- split(x, factor(
        tolower(x$albums),
        levels = unique(tolower(x$albums)),
        labels = x$albums[!duplicated(tolower(x$albums))]
    ))
    x <- x[vapply(x, function(xx) length(unique(xx$albums)), FUN.VALUE = 0L) != 1L]
    y <- lapply(x, function(xx) {
        .self[unique(xx$indexes)]
    })
    if (warn) {
        if (length(x)) {
            k <- vapply(x, function(xx) {
                paste(dQuote(unique(xx$albums)), collapse = ", ")
            }, FUN.VALUE = "")
            warning(
                sprintf(
                    ngettext(
                        length(x),
                        "found %d artist inconsistency",
                        "found %d artist inconsistencies"
                    ),
                    length(x)
                ),
                paste0("\n  for ", unstrip(dQuote(names(k))), ", found: ", k)
            )
        }
        invisible(y)
    }
    else y
},


refresh = function ()
{
    need2refresh <- .self$value$mtime + 1 <= file.mtime(names(.self))
    files2refresh <- names(.self)[need2refresh]
    if (length(files2refresh))
        .self$value[need2refresh, ] <- as.MusicMetadata(exiftool(path = files2refresh))$value
    invisible()
},


validate.playlist = function (file = this.path::here(.. = 1, name), name)
{
    # .self <- value2 ; file <- this.path::here(.. = 1, "last-added.txt") ; text <- readLines(file, encoding = "UTF-8") ; warning("comment this out later", immediate. = TRUE)
    text <- readLines(file, encoding = "UTF-8")
    table <- paste(
        .self$value$album.artist,
        .self$value$album,
        ifelse(
            .self$has.composer(),
            .self$value$composer,
            .self$value$artist
        ),
        .self$value$title,
        sep = " - "
    )
    i <- match(text, table)
    if (any(isna <- is.na(i)))
        warning(sprintf(ngettext(sum(isna),
            "%scould not find song: %s",
            "%scould not find songs:\n    %s"),
            if (!missing(name) && nzchar(name))
                paste0("in playlist ", dQuote(name), ", ")
            else "",
            paste(dQuote(text[isna]), collapse = "\n    ")))
    invisible(!isna)
},


validate.playlists = function (files = this.path::here(.. = 1, names), names)
{
    X <- list(file = files)
    if (!missing(names))
        X["name"] <- list(names)
    invisible(essentials::plapply(X, .self$validate.playlist))
},


organize.music = function (dir, folders4singles = FALSE)
{
    # .self <- value2 ; dir <- this.path::here(.. = 1, "output") ; folders4singles <- FALSE ; stop("comment this out later")


    dir <- essentials::as.scalar.string(dir)
    folders4singles <- essentials::as.scalar.logical(folders4singles)


    to <- rep(dir, length(.self))


    i <- .self$has.album.artist()
    to[i] <- file.path(to[i], music.meta::fix.artists(.self$value$album.artist[i]))


    i <- i & .self$has.album() & (folders4singles | !.self$is.single())
    to[i] <- file.path(to[i], music.meta::fix.title(.self$value$album[i]))


    to <- file.path(to, .self$as.filename(short = TRUE))


    for (path in unique(dirname(to)))
        dir.create(path, showWarnings = FALSE, recursive = TRUE)


    if (any(i <- !file.copy(
        from = names(.self),
        to   = to
    ))) {
        .x <- cbind(from = names(.self), to = to)[i, , drop = FALSE]
        rownames(.x) <- rep(" ", nrow(.x))
        .x <- utils::capture.output(print(.x, quote = FALSE))
        warning(gettextf("unable to copy files:\n%s",
            paste(x, collapse = "\n")))
    }
    invisible(i)
}


    )
)


# S3 methods       ----


as.data.frame.MusicMetadata <- function (x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- deparse1(substitute(x))
    if (is.null(row.names)) {
        autoRN <- FALSE
        if (nrows == 0L)
            row.names <- character()
        else if (length(row.names <- names(x)) == nrows &&
            !anyDuplicated(row.names)) {
        }
        else {
            autoRN <- TRUE
            row.names <- .set_row_names(nrows)
        }
    }
    else autoRN <- is.integer(row.names) && length(row.names) ==
        2L && is.na(rn1 <- row.names[[1L]]) && rn1 < 0
    value <- list(x)
    if (!optional)
        names(value) <- nm
    class(value) <- "data.frame"
    attr(value, "row.names") <- row.names
    value
}


format.MusicMetadata <- function (x, ...)
format(x = names(x), ...)


# S4 methods       ----


methods::setMethod("[", "MusicMetadata", function (x, i, j, ..., drop = TRUE)
{
    Narg <- nargs() - (!missing(x)) - (!missing(i)) - (!missing(drop))
    if (Narg >= 1L)
        stop("incorrect number of dimensions")
    val <- x$copy()
    val$value <- val$value[i, , drop = FALSE]
    val
})


methods::setMethod("[[", "MusicMetadata", function (x, i, j, ...)
{
    Narg <- nargs() - (!missing(x)) - (!missing(i))
    if (Narg >= 1L)
        stop("incorrect number of dimensions")
    if (is.character(i))
        i <- pmatch(i, names(x), duplicates.ok = TRUE)
    x[seq_along(x)[[i]]]
})


methods::setMethod("as.character", "MusicMetadata", function (x, source = FALSE, indent = 0, ...)
{
    # x <- value2 ; source <- FALSE ; indent <- 0 ; stop("comment this out later")


    if (length(x) <= 0L)
        return(character(0))


    source <- essentials::as.scalar.logical(source)
    indent <- essentials::as.scalar.integer(indent)
    indentString <- strrep(" ", indent)
    val <- paste0(indentString, "Title           : ", x$value$title)


    artists       <- strsplit(x$value$artist      , "/", fixed = TRUE)
    album.artists <- strsplit(x$value$album.artist, "/", fixed = TRUE)
    composers     <- strsplit(x$value$composer    , "/", fixed = TRUE)


    artists1       <- vapply(artists      , "[", 1L, FUN.VALUE = "")
    album.artists1 <- vapply(album.artists, "[", 1L, FUN.VALUE = "")


    artists1[is.na(artists1)] <- ""
    album.artists1[is.na(album.artists1)] <- ""


    val <- paste(val,
        ifelse(
            lengths(artists) == 1,
            paste0(indentString, "Artist          : ", artists1),
            paste0(indentString, "Artists         : ", vapply(artists, function(xx) {
                                                           paste(dQuote(xx), collapse = ", ")
                                                       }, FUN.VALUE = ""))
        ),
        sep = "\n")
    composers <- strsplit(x$value$composer, "/", fixed = TRUE)
    i <- x$has.composer()
    val[i] <- paste(val[i],
        ifelse(
            lengths(composers[i]) == 1,
            paste0(indentString, "Composer        : ", composers[i]),
            paste0(indentString, "Composers       : ", vapply(composers[i], function(xx) {
                                                           paste(dQuote(xx), collapse = ", ")
                                                       }, FUN.VALUE = ""))
        ),
        sep = "\n")

    i <- x$has.genre()
    val[i] <- paste(val[i],
        paste0(indentString, "Genre           : ", x$value$genre[i]),
        sep = "\n")
    i <- !x$is.single()
    val[i] <- paste(val[i],
        paste0(indentString, "Album           : ", x$value$album[i]),
        sep = "\n")
    j <- i & !(album.artists1 == artists1 | as.character(artists) == as.character(album.artists))
    val[j] <- paste(val[j],
        ifelse(
            lengths(album.artists[j]) == 1,
            paste0(indentString, "Album Artist    : ", album.artists1[j]),
            paste0(indentString, "Album Artists   : ", vapply(album.artists[j], function(xx) {
                                                           paste(dQuote(xx), collapse = ", ")
                                                       }, FUN.VALUE = ""))
        ),
        sep = "\n")
    # j <- i & x$value$track.number != 0L
    j <- i & !(x$value$track.number == 0 | x$value$track.count == 1)
    val[j] <- paste(val[j],
        ifelse(
            x$value$track.count[j] != 0L,
            paste0(indentString, "Track           : ", x$value$track.number[j], " of ", x$value$track.count[j]),
            paste0(indentString, "Track           : ", x$value$track.number[j])
        ),
        sep = "\n")
    # j <- i & x$value$disk.count != 1L
    j <- i & !(x$value$disk.number == 0 | x$value$disk.count == 1)
    val[j] <- paste(val[j],
        ifelse(
            x$value$disk.count[j] != 0L,
            paste0(indentString, "Disk            : ", x$value$disk.number[j], " of ", x$value$disk.count[j]),
            paste0(indentString, "Disk            : ", x$value$disk.number[j])
        ),
        sep = "\n")
    if (source)
        val <- paste(val,
            paste0(indentString, "Source          : ", names(x)),
            sep = "\n")
    val
})


methods::setMethod("length", "MusicMetadata", function(x)
nrow(x$value))


methods::setMethod("length<-", c(x = "MusicMetadata"), function (x, value)
{
    x[seq_len(value), , drop = FALSE]
})


methods::setMethod("lengths", c(x = "MusicMetadata"), function (x, use.names = TRUE)
{
    value <- rep(1L, length.out = nrow(x$value))
    if (use.names)
        names(value) <- row.names(x$value)
    value
})


methods::setMethod("names", "MusicMetadata", function(x)
row.names(x$value))


methods::setMethod("names<-", c("MusicMetadata", "ANY"), function (x, value)
{
    row.names(x$value) <- value
    return(x)
})


# other misc       ----


as.MusicMetadata <- function (x, ...)
{
    # x <- value2 ; warning("comment this out later")
    # x <- x[order(names(x))]
    m4a <- grep("\\.m4a$", names(x), ignore.case = TRUE)
    mp3 <- setdiff(seq_along(x), m4a)
    mp3 <- mp3[grep("\\.mp3$", names(x)[mp3], ignore.case = TRUE)]
    others <- setdiff(seq_along(x), c(m4a, mp3))
    if (length(others) >= 1)
        stop(ngettext(length(others),
            "cannot extract music metadata from file:",
            "cannot extract music metadata from files:"),
            paste("\n  ", dQuote(names(x)[others]), collapse = "", recycle0 = TRUE))



    x.m4a <- x[m4a]
    x.m4a <- vapply(x.m4a, "[", c("Title", "Artist", "Genre", "TrackNumber", "DiskNumber", "Album", "AlbumArtist", "Composer", "ContentCreateDate"), FUN.VALUE = character(9L))
    x.m4a <- t(x.m4a)
    x.m4a[is.na(x.m4a)] <- ""
    x.m4a <- as.data.frame(x.m4a)
    colnames(x.m4a) <- c("title", "artist", "genre", "track.number", "disk.number", "album", "album.artist", "composer", "date")
    x.m4a[c("track.number", "track.count")] <- as.data.frame(t(vapply(strsplit(x.m4a[, "track.number"], "of"), function(xx) {
        pmax.int(as.integer(xx[1:2]), 0L, na.rm = TRUE)
    }, FUN.VALUE = integer(2L))))
    x.m4a[c("disk.number", "disk.count")] <- as.data.frame(t(vapply(strsplit(x.m4a[, "disk.number"], "of"), function(xx) {
        pmax.int(as.integer(xx[1:2]), 0L, na.rm = TRUE)
    }, FUN.VALUE = integer(2L))))


    x.mp3 <- x[mp3]
    x.mp3 <- vapply(x.mp3, "[", c("Title", "Artist", "Genre", "Track", "PartOfSet", "Album", "Band", "Composer", "DateTimeOriginal"), FUN.VALUE = character(9L))
    x.mp3 <- t(x.mp3)
    x.mp3[is.na(x.mp3)] <- ""
    x.mp3 <- as.data.frame(x.mp3)
    colnames(x.mp3) <- c(
        "title",
        "artist",
        "genre",
        "track.number",
        "disk.number",
        "album",
        "album.artist",
        "composer",
        "date"
    )
    # x.mp3 <- as.data.frame(t(vapply(x[mp3], function(xx) {
    #     value <- substring(xx[pmatch(c(
    #         "Title                           : ",
    #         "Artist                          : ",
    #         "Genre                           : ",
    #         "Track                           : ",
    #         "Part Of Set                     : ",
    #         "Album                           : ",
    #         "Band                            : ",
    #         "Date/Time Original              : "
    #     ), xx)], 35L)
    #     value[is.na(value)] <- ""
    #     value
    # }, FUN.VALUE = character(8L))))
    # names(x.mp3) <- c(
    #     "title",
    #     "artist",
    #     "genre",
    #     "track.number",
    #     "disk.number",
    #     "album",
    #     "album.artist",
    #     "date"
    # )
    x.mp3[c("track.number", "track.count")] <- as.data.frame(t(vapply(strsplit(x.mp3[, "track.number"], "/"), function(xx) {
        pmax.int(as.integer(xx[1:2]), 0L, na.rm = TRUE)
    }, FUN.VALUE = integer(2L))))
    x.mp3[c("disk.number", "disk.count")] <- as.data.frame(t(vapply(strsplit(x.mp3[, "disk.number"], "/"), function(xx) {
        pmax.int(as.integer(xx[1:2]), 0L, na.rm = TRUE)
    }, FUN.VALUE = integer(2L))))


    music <- as.data.frame(matrix(nrow = length(x), ncol = ncol(x.m4a), dimnames = list(
        names(x), colnames(x.m4a)
    )))
    music[m4a, ] <- x.m4a
    music[mp3, ] <- x.mp3
    music$mtime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileModifyDate", FUN.VALUE = "", USE.NAMES = FALSE))
    music$ctime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileCreateDate", FUN.VALUE = "", USE.NAMES = FALSE))
    music$atime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileAccessDate", FUN.VALUE = "", USE.NAMES = FALSE))


    empty <- cbind(
        Title          = music$title        == "",
        Artist         = music$artist       == "",
        `Track Number` = music$track.number == 0L,
        `Track Count`  = music$track.count  == 0L,
        `Disk Number`  = music$disk.number  == 0L,
        `Disk Count`   = music$disk.count   == 0L,
        Album          = music$album        == "",
        `Album Artist` = music$album.artist == "",
        Date           = music$date         == ""
    )
    rownames(empty) <- rownames(music)
    empty <- empty[apply(empty, 1L, "any"), apply(empty, 2L, "any"), drop = FALSE]
    if (nrow(empty)) {
        dimnames(empty) <- list(
            dQuote(rownames(empty)),
            paste(dQuote(colnames(empty)), "field is empty")
        )
        for (i in seq_len(nrow(empty))) {
            warning(gettextf("for file %s:\n    %s",
                rownames(empty)[[i]],
                paste(colnames(empty[i, , drop = FALSE])[empty[i, ]], collapse = "    ")))
        }
    }


    return(methods::new(MusicMetadata$def, value = music))
}





exiftoolDate2POSIXct <- function (x)
{
    shift <- sub("^.+([+-][[:digit:]]{2}:[[:digit:]]{2})$", "\\1", x)
    shift <- strsplit(shift, ":", fixed = TRUE)
    shift <- as.difftime(as.numeric(vapply(shift, "[[", 1L, FUN.VALUE = "")), units = "hours") +
        as.difftime(as.numeric(vapply(shift, "[[", 2L, FUN.VALUE = "")), units = "mins")
    x <- as.POSIXct(x, tz = "UTC", format = "%Y:%m:%d %H:%M:%S")
    x - shift
}


unstrip <- function (x)
{
    n <- nchar(x, type = "width", keepNA = FALSE)
    paste0(x, strrep(" ", max(n) - n), recycle0 = TRUE)
}


asUnixbasename.custom <- function (x)
{
    x <- gsub("([^! ])!([^! ])", "\\1I\\2", x)
    x <- gsub("$", "S", x, fixed = TRUE)
    x <- asUnixbasename(x)
    x <- gsub(".", "-", x, fixed = TRUE)
    x <- gsub("_", "-", x, fixed = TRUE)
    x <- gsub("(^-+)|(-+$)", "", x)
    x <- gsub("-{2,}", "-", x)
    x
}


fix.artists <- function (x)
{
    skeleton <- strsplit(x, "/", fixed = TRUE)
    flesh <- unlist(skeleton, use.names = FALSE)
    flesh <- asUnixbasename.custom(flesh)
    x <- relist(flesh, skeleton = skeleton)
    vapply(x, paste, collapse = "_", FUN.VALUE = "")
}


fix.title <- function (x)
{
    parenthesis.pattern <- "\\(([^)]+)\\)"
    x <- gsub(parenthesis.pattern, "\n\\1\n", x)
    skeleton <- strsplit(x, "\n+")
    x <- unlist(skeleton)
    remix.pattern <- "^(.+) (cover|mashup|remix)$"
    remix <- grepl(remix.pattern, x, ignore.case = TRUE)
    x[remix] <- paste0(
        fix.artists(sub(remix.pattern, "\\1", x[remix], ignore.case = TRUE)),
        "_",
        tolower(sub(remix.pattern, "\\2", x[remix], ignore.case = TRUE)),
        recycle0 = TRUE
    )
    x[!remix] <- asUnixbasename.custom(x[!remix])
    x <- relist(x, skeleton = skeleton)
    x <- vapply(x, paste, collapse = "_", FUN.VALUE = "")
    asUnixbasename(x)
}
