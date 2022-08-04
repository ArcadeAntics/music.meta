

# class definition ----


MusicMetadata <- methods::setRefClass(
    Class  = "MusicMetadata",
    fields = c(
        metadata = "data.frame",
        directories = "data.frame"
    ),
    methods = list(


has.title = function ()
.self$metadata$title != "",


has.artist = function ()
.self$metadata$artist != "",


has.genre = function ()
.self$metadata$genre != "",


has.album = function ()
.self$metadata$album != "",


has.album.artist = function ()
.self$metadata$album.artist != "",


has.composer = function ()
.self$metadata$composer != "",


is.single = function ()
.self$has.title() & .self$metadata$title == .self$metadata$album &
    .self$has.album.artist() &
    sub("/.*", "", .self$metadata$album.artist) == sub("/.*", "", .self$metadata$artist) &
    .self$metadata$track.number == 1L & .self$metadata$track.count == 1L &
    .self$metadata$disk.number == 1L & .self$metadata$disk.count == 1L,


get.title = function ()
fix.title(.self$metadata$title),


as.filename = function (short = FALSE)
{
    # metadata <- music.meta:::as.MusicMetadata(list(
    #     test1.m4a = c(
    #         Title  = "I'm Not Okay (I Promise)",
    #         Artist = "My Chemical Romance",
    #         TrackNumber = "5 of 13",
    #         DiskNumber = "1 of 1",
    #         Album = "Three Cheers for Sweet Revenge",
    #         AlbumArtist = "My Chemical Romance",
    #         ContentCreateDate = "2004",
    #         FileModifyDate = "1970:01:01 00:00:00+00:00",
    #         FileAccessDate = "1970:01:01 00:00:00+00:00",
    #         FileCreateDate = "1970:01:01 00:00:00+00:00"
    #     ),
    #     test2.m4a = c(
    #         Title  = "I'm Not Okay (I Promise)",
    #         Artist = "My Chemical Romance",
    #         TrackNumber = "26 of 44",
    #         DiskNumber = "1 of 1",
    #         Album = "Burnout 3: Takedown",
    #         AlbumArtist = "Stephen Root",
    #         ContentCreateDate = "2004",
    #         FileModifyDate = "1970:01:01 00:00:00+00:00",
    #         FileAccessDate = "1970:01:01 00:00:00+00:00",
    #         FileCreateDate = "1970:01:01 00:00:00+00:00"
    #     ),
    #     test3.m4a = c(
    #         Title  = "Lazy Generation",
    #         Artist = "The F-Ups",
    #         TrackNumber = "14 of 44",
    #         DiskNumber = "1 of 1",
    #         Album = "Burnout 3: Takedown",
    #         AlbumArtist = "Stephen Root",
    #         ContentCreateDate = "2004",
    #         FileModifyDate = "1970:01:01 00:00:00+00:00",
    #         FileAccessDate = "1970:01:01 00:00:00+00:00",
    #         FileCreateDate = "1970:01:01 00:00:00+00:00"
    #     )
    # ))
    # writeLines(as.character(metadata), sep = "\n\n")
    # metadata$as.filename()
    # metadata$as.filename(short = TRUE)
    #
    #
    # .self <- metadata
    # fix.artists <- music.meta:::fix.artists
    # fix.title <- music.meta:::fix.title
    # short <- FALSE


    short <- rep_len(as.logical(short), length(.self))
    oshort <- short  # original short argument
    if (any(i <- is.na(short))) {
        tmp <- .self[i]
        short[i] <- tmp$has.album.artist() & !tmp$has.composer() & !tmp$is.single()
    }
    do_short <- function(x) x$get.title()
    do_long <- function(x) {
        val <- ifelse(
            x$has.title(),
            ifelse(
                x$has.composer(),
                x$metadata$composer,
                x$metadata$artist
            ),
            ""
        )
        i <- nzchar(val)
        val[i] <- paste0(fix.artists(val[i]), "_", recycle0 = TRUE)
        paste0(val, x$get.title())
    }
    val <- character(length(.self))
    if (any(short))
        val[short] <- do_short(.self[short])
    if (any(long <- !short))
        val[long] <- do_long(.self[long])


    # suppose you had the same song twice:
    #
    # Title           : I'm Not Okay (I Promise)
    # Artist          : My Chemical Romance
    # Album           : Three Cheers for Sweet Revenge
    # Track           : 5 of 13
    #
    # Title           : I'm Not Okay (I Promise)
    # Artist          : My Chemical Romance
    # Album           : Burnout 3: Takedown
    # Album Artist    : Stephen Root
    # Track           : 26 of 44
    #
    # the above would generate the same filename for each,
    # so what we have to do is add the album information as well
    #
    # HOWEVER, if the album artist is the same as the artist,
    # don't bother to add the artist


    # print(val, quote = FALSE, width = 10)


    i <- which(nzchar(val) & val %in% val[(is.na(oshort) | oshort) & duplicated(val)])
    short_with_album_artist <- essentials::pvapply(list(
        strsplit(.self$metadata$artist      [i], "/", fixed = TRUE),
        strsplit(.self$metadata$album.artist[i], "/", fixed = TRUE)
    ), function(artist, album.artist) {
        all(album.artist %in% artist)
    }, NA)
    do_short_with_album_artist <- function(x) {
        paste(
            fix.artists(x$metadata$album.artist),
            fix.title(x$metadata$album),
            x$get.title(),
            sep = "_"
        )
    }
    do_long_with_album_artist <- function(x) {
        paste(
            fix.artists(x$metadata$album.artist),
            fix.title(x$metadata$album),
            fix.artists(x$metadata$artist),
            x$get.title(),
            sep = "_"
        )
    }
    j <- i[short_with_album_artist]
    val[j] <- do_short_with_album_artist(.self[j])
    j <- i[!short_with_album_artist]
    val[j] <- do_long_with_album_artist(.self[j])


    # print(val, quote = FALSE, width = 10)


    if (any(i <- !nzchar(val))) {
        warning("unable to make meaningful filenames for the following, generating random names instead:\n",
            paste(utils::capture.output(print(names(.self)[i], quote = FALSE, max = 9, width = 10)), collapse = "\n"))
        tmp <- sprintf("%08x", sample(.Machine$integer.max, sum(i) * 4L, replace = TRUE))
        dim(tmp) <- c(length(tmp)/4, 4)
        val[i] <- apply(tmp, 1L, paste, collapse = "")
    }
    paste0(val, essentials::ext(basename(names(.self))))
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


    x$char <- as.character(x$self, source = TRUE)


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
#             val[i] <- paste0(gsub("/", ", ", .self$metadata$artist[i], fixed = TRUE), " - ")
#         if (any(i <- !blank))
#             val[i] <- paste0(val[i], .self$metadata$title[i], essentials::ext(names(.self)[i]))
#         essentials::asWindowsbasename(val)
#     }
# },


CheckInconsistencies = function (what = choices, warn = TRUE)
{
    # .self <- metadata ; what <- mkPROMISE(choices) ; warn <- TRUE ; stop("delete this later")
    choices <- c("album", "artist", "date", "disk.count", "genre", "track.count")
    what <- unique(match.arg(what, choices = choices, several.ok = TRUE))
    warn <- if (warn) TRUE else FALSE
    if (length(.self) <= 0L) {
        x <- structure(list(), names = character())
        x <- structure(rep(list(x), length(what)), names = what)
        return(if (warn)
            invisible(x)
        else x)
    }
    x <- lapply(what, function(wwhat) {
        x <- switch (wwhat, album = {
            data.frame(
                indexes = seq_along(.self),
                values  = paste(.self$metadata$album.artist, .self$metadata$album, sep = " - ")
            )
        }, artist = {
            artists1 <- strsplit(.self$metadata$artist      , "/", fixed = TRUE)
            artists2 <- strsplit(.self$metadata$album.artist, "/", fixed = TRUE)
            artists3 <- strsplit(.self$metadata$composer    , "/", fixed = TRUE)
            data.frame(
                indexes = c(
                    rep(seq_along(.self), lengths(artists1)),
                    rep(seq_along(.self), lengths(artists2)),
                    rep(seq_along(.self), lengths(artists3))
                ),
                values  = c(
                    unlist(artists1),
                    unlist(artists2),
                    unlist(artists3)
                )
            )
        }, {
            data.frame(
                indexes = seq_along(.self),
                values  = .self$metadata[[wwhat]]
            )
        })
        f <- switch (wwhat, album = , artist = , genre = {
            tmp <- tolower(x$values)
            tmp2 <- !duplicated(tmp)
            f <- factor(tmp, levels = tmp[tmp2], labels = x$values[tmp2])
        }, track.count = {
            paste0(.self$metadata$album.artist, " - ", .self$metadata$album, " - Disk ", .self$metadata$disk.number, recycle0 = TRUE)
        }, {
            paste0(.self$metadata$album.artist, " - ", .self$metadata$album                                        , recycle0 = TRUE)
        })
        x <- split(x, f)
        x <- x[vapply(x, function(xx) {
            length(unique(xx$values))
        }, FUN.VALUE = 0L) != 1L]
        # y <- lapply(x, function(xx) {
        #     .self[unique(xx$indexes)]
        # })
        x
    })
    names(x) <- what
    mfor(y, wwhat, list(x, what), {
        if (warn && length(y)) {
            k <- vapply(y, function(yy) {
                paste(dQuote(unique(yy$values)), collapse = ", ")
            }, FUN.VALUE = "")
            warning(
                sprintf(
                    ngettext(
                        length(y),
                        "found %d %s inconsistency",
                        "found %d %s inconsistencies"
                    ),
                    length(y), wwhat
                ),
                paste0("\n  in ", unstrip(dQuote(names(k))), " found: ", k))
        }
    })
    y <- lapply(x, function(xx) {
        lapply(xx, function(xxx) {
            .self[unique(xxx$indexes)]
        })
    })
    if (warn)
        invisible(y)
    else y
},


CheckAlbumInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("album", warn = warn)[[1L]]
},


CheckArtistInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("artist", warn = warn)[[1L]]
},


CheckDateInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("date", warn = warn)[[1L]]
},


CheckDiskCountInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("disk.count", warn = warn)[[1L]]
},


CheckGenreInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("genre", warn = warn)[[1L]]
},


CheckTrackCountInconsistencies = function (warn = TRUE)
{
    .self$CheckInconsistencies("track.count", warn = warn)[[1L]]
},


.scan.files = function (path)
{
    path <- unique(path)
    keep <- !(path %in% names(.self))
    if (any(i <- !keep)) {
        keep[i] <- .self$metadata[path[i], "mtime"] + 1 <= file.mtime(path[i])
        path <- path[keep & !is.na(keep)]
    }
    if (length(path) > 0L) {
        x <- read.MusicMetadata(path, check = "stop")
        .self[names(x)] <- x
    }
    invisible()
},


scan.files = function (path)
{
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    path <- path[file.exists(path) & !dir.exists(path)]
    .self$.scan.files(path)
},


scan.directories = function (path = .self$directories$path)
{
    if (!missing(path))
        path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .self$.scan.files(essentials::list.files2(path, "\\.(m4a|mp3)$",
        all.files = TRUE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE))
},


refresh = function ()
{
    need2refresh <- .self$metadata$mtime + 1 <= file.mtime(names(.self))
    need2remove <- is.na(need2refresh)
    if (any(need2remove)) {
        i <- which(!need2remove)
        .self$metadata <- .self$metadata[i, , drop = FALSE]
        need2refresh <- need2refresh[i]
    }
    if (any(need2refresh))
        .self$metadata[need2refresh, ] <- read.MusicMetadata(names(.self)[need2refresh])$metadata


    info <- file.info(.self$directories$path, extra_cols = FALSE)
    info$need2refresh <- .self$directories$mtime + 1 <= info$mtime
    need2remove <- is.na(info$need2refresh) | !info$isdir
    if (any(need2remove)) {
        i <- which(!need2remove)
        .self$directories <- .self$directories[i, , drop = FALSE]
        info <- info[i, , drop = FALSE]
    }
    mtime <- info$mtime
    .self$scan.directories(row.names(info)[info$need2refresh])
    .self$directories$mtime <- mtime
    invisible()
},


validate.playlist = function (file = this.path::here(.. = 1, name), name)
{
    # .self <- value2 ; file <- this.path::here(.. = 1, "last-added.txt") ; text <- readLines(file, encoding = "UTF-8") ; warning("comment this out later", immediate. = TRUE)
    text <- readLines(file, encoding = "UTF-8")
    table <- paste(
        .self$metadata$album.artist,
        .self$metadata$album,
        ifelse(
            .self$has.composer(),
            .self$metadata$composer,
            .self$metadata$artist
        ),
        .self$metadata$title,
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
    to[i] <- file.path(to[i], fix.artists(.self$metadata$album.artist[i]))


    i <- i & .self$has.album() & (folders4singles | !.self$is.single())
    to[i] <- file.path(to[i], fix.title(.self$metadata$album[i]))


    to <- file.path(to, .self$as.filename(short = i))


    for (path in unique(dirname(to)))
        dir.create(path, showWarnings = FALSE, recursive = TRUE)


    if (any(i <- !file.copy(
        from = names(.self),
        to   = to
    ))) {
        .x <- cbind(from = names(.self), to = to)[i, , drop = FALSE]
        .x <- utils::capture.output(print(.x, quote = FALSE, width = 10))
        warning(gettextf("unable to copy files:\n%s",
            paste(.x, collapse = "\n")))
    }
    invisible(i)
},


add.directories = function (paths)
{
    .self$directories <- rbind(
        .self$directories,
        as.directories(paths)
    )
    if (any(dup <- duplicated(.self$directories$path, fromLast = TRUE)))
        .self$directories <- .self$directories[!dup, , drop = FALSE]
    invisible(.self$directories)
}


    )
)


as.directories <- function (paths)
{
    info <- file.info(normalizePath(paths, winslash = "/", mustWork = TRUE), extra_cols = FALSE)
    info <- info[info$isdir, , drop = FALSE]
    mtime <- info$mtime - 2
    attr(mtime, "tzone") <- "UTC"
    data.frame(path = row.names(info), mtime = mtime)
}


MusicMetadataClass <- MusicMetadata$def@className


MusicMetadata <- function (metadata = metadata.prototype, paths = character())
{
    methods::new(MusicMetadataClass, metadata = metadata,
        directories = as.directories(paths))
}



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
    val$metadata <- val$metadata[i, , drop = FALSE]
    val
})


methods::setMethod("[[", "MusicMetadata", function (x, i, j, ...)
{
    Narg <- nargs() - (!missing(x)) - (!missing(i))
    if (Narg >= 1L)
        stop("incorrect number of dimensions")
    if (is.character(i))
        i <- pmatch(i, names(x), duplicates.ok = TRUE)
    i <- seq_along(x)[[i]]
    val <- x$copy()
    val$metadata <- val$metadata[i, , drop = FALSE]
    val
})


methods::setMethod("[<-", "MusicMetadata", function (x, i, j, ..., value)
{
    Narg <- nargs() - (!missing(x)) - (!missing(i)) - (!missing(value))
    if (Narg >= 1L)
        stop("incorrect number of dimensions")
    if (value |> methods::is(MusicMetadataClass))
        x$metadata[i, ] <- value$metadata
    else x$metadata[i, ] <- value
    x
})


methods::setMethod("[[<-", "MusicMetadata", function (x, i, j, ..., value)
{
    Narg <- nargs() - (!missing(x)) - (!missing(i)) - (!missing(value))
    if (Narg >= 1L)
        stop("incorrect number of dimensions")
    if (is.character(i))
        i <- pmatch(i, names(x), duplicates.ok = TRUE)
    i <- seq_along(x)[[i]]
    if (value |> methods::is(MusicMetadataClass))
        x$metadata[i, ] <- value$metadata
    else x$metadata[i, ] <- value
    x
})


methods::setMethod("as.character", "MusicMetadata", function (x, source = FALSE, indent = 0, ...)
{
    # x <- value2 ; source <- FALSE ; indent <- 0 ; stop("comment this out later")


    if (length(x) <= 0L)
        return(character(0))


    source <- essentials::as.scalar.logical(source)
    indent <- essentials::as.scalar.integer(indent)
    indentString <- strrep(" ", indent)
    val <- paste0(indentString, "Title           : ", x$metadata$title)


    artists       <- strsplit(x$metadata$artist      , "/", fixed = TRUE)
    album.artists <- strsplit(x$metadata$album.artist, "/", fixed = TRUE)
    composers     <- strsplit(x$metadata$composer    , "/", fixed = TRUE)


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
    composers <- strsplit(x$metadata$composer, "/", fixed = TRUE)
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
        paste0(indentString, "Genre           : ", x$metadata$genre[i]),
        sep = "\n")
    i <- !x$is.single()
    val[i] <- paste(val[i],
        paste0(indentString, "Album           : ", x$metadata$album[i]),
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
    # j <- i & x$metadata$track.number != 0L
    j <- i & !(x$metadata$track.number == 0 | x$metadata$track.count == 1)
    val[j] <- paste(val[j],
        ifelse(
            x$metadata$track.count[j] != 0L,
            paste0(indentString, "Track           : ", x$metadata$track.number[j], " of ", x$metadata$track.count[j]),
            paste0(indentString, "Track           : ", x$metadata$track.number[j])
        ),
        sep = "\n")
    # j <- i & x$metadata$disk.count != 1L
    j <- i & !(x$metadata$disk.number == 0 | x$metadata$disk.count == 1)
    val[j] <- paste(val[j],
        ifelse(
            x$metadata$disk.count[j] != 0L,
            paste0(indentString, "Disk            : ", x$metadata$disk.number[j], " of ", x$metadata$disk.count[j]),
            paste0(indentString, "Disk            : ", x$metadata$disk.number[j])
        ),
        sep = "\n")
    if (source)
        val <- paste(val,
            paste0(indentString, "Source          : ", names(x)),
            sep = "\n")
    val
})


methods::setMethod("length", "MusicMetadata", function(x)
nrow(x$metadata))


methods::setMethod("length<-", c(x = "MusicMetadata"), function (x, value)
{
    x[seq_len(value), , drop = FALSE]
})


methods::setMethod("lengths", c(x = "MusicMetadata"), function (x, use.names = TRUE)
{
    value <- rep(1L, length.out = nrow(x$metadata))
    if (use.names)
        names(value) <- row.names(x$metadata)
    value
})


methods::setMethod("names", "MusicMetadata", function(x)
row.names(x$metadata))


methods::setMethod("names<-", c("MusicMetadata", "ANY"), function (x, value)
{
    row.names(x$metadata) <- value
    return(x)
})


# other misc       ----


as.MusicMetadata <- function (x = NULL, ...)
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


    metadata <- x.m4a[rep(NA_integer_, length(x)), , drop = FALSE]
    row.names(metadata) <- names(x)
    metadata[m4a, ] <- x.m4a
    metadata[mp3, ] <- x.mp3
    metadata$mtime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileModifyDate", FUN.VALUE = "", USE.NAMES = FALSE))
    metadata$ctime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileCreateDate", FUN.VALUE = "", USE.NAMES = FALSE))
    metadata$atime <- exiftoolDate2POSIXct(vapply(x, "[[", "FileAccessDate", FUN.VALUE = "", USE.NAMES = FALSE))


    empty <- cbind(
        Title          = metadata$title        == "",
        Artist         = metadata$artist       == "",
        `Track Number` = metadata$track.number == 0L,
        `Track Count`  = metadata$track.count  == 0L,
        `Disk Number`  = metadata$disk.number  == 0L,
        `Disk Count`   = metadata$disk.count   == 0L,
        Album          = metadata$album        == "",
        `Album Artist` = metadata$album.artist == "",
        Date           = metadata$date         == ""
    )
    rownames(empty) <- rownames(metadata)
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


    MusicMetadata(metadata = metadata)
}





exiftoolDate2POSIXct <- function (x)
{
    # an exiftool datetime looks like
    #
    # %Y:%m:%d %H:%M:%S
    #
    # followed by + or - HH:MM
    # where H:M is the time zone shift


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
    x[!remix] <- asUnixbasename.custom(gsub("(?<=[[:digit:]])[,\\.]([[:digit:]]{3})(?![[:digit:]])", "\\1", x[!remix], perl = TRUE))
    x <- relist(x, skeleton = skeleton)
    x <- vapply(x, paste, collapse = "_", FUN.VALUE = "")
    asUnixbasename(x)
}


metadata.prototype <- as.MusicMetadata()$metadata
