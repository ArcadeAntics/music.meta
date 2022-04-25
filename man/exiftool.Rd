\name{exiftool}
\alias{exiftool}
\title{
Read, Write, and Edit Meta Information
}
\description{
Run ExifTool with a given set of options and files. This is used by
\code{\link{read.MusicMetadata}}.
}
\usage{
exiftool(options = NULL, file = NULL)
}
\arguments{
  \item{options}{
character vector. The options to be provided to ExifTool.
  }
  \item{file}{
character vector. The filenames to be provided to ExifTool.
  }
}
\details{
\code{\link[base:path.expand]{Tilde-expansion}} is first done on \code{file}.
\code{file} may contain any number of files, directories, or patterns to read,
see section \strong{Examples}.
}
\value{
A character vector; the result of calling ExifTool.
}
\examples{
\dontrun{
exiftool(file = c(
    "Music/input",  # read all files in Music/input directory
    "dirtyphonics_ivory_bastille.m4a",  # read an individual file
    "camellia_*"  # read all files that match this glob pattern
))
exiftool(
    options = c(
        "-extension", "m4a",  # process files with m4a extension
        "-extension", "mp3"   # process files with mp3 extension
    ),
    file = "Music/input"  # read all m4a/mp3 files in Music/input directory
)
}
}
