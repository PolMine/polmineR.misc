% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/as.tibble_method.R
\docType{methods}
\name{as.tibble}
\alias{as.tibble}
\alias{as.tibble,partition-method}
\alias{as.tibble,partitionBundle-method}
\title{Turn partition into tibble.}
\usage{
as.tibble(.Object, ...)

\S4method{as.tibble}{partition}(.Object, pAttribute)

\S4method{as.tibble}{partitionBundle}(.Object, pAttribute,
  mc = getOption("polmineR.mc"))
}
\description{
Turn partition into tibble.
}
\examples{
\dontrun{
library(polmineR)
library(tibble)
library(plyr)
library(dplyr)
library(pbapply)

plprbt <- partition("PLPRBT", speaker_type = "speech", speaker_year = "2010")
speeches <- as.speeches(
  plprbt,
  sAttributeDates = "speaker_date", sAttributeNames = "speaker_name", gap = 500,
  mc = TRUE, progress = TRUE
)
for (i in rev(which(summary(speeches)[["token"]] == 0))) speeches@objects[[i]] <- NULL
tib <- as.tibble(speeches, pAttribute = "word")
Y <- ddply(
  .data = tib,
  .variable = .(speaker_type, speaker_date, speaker_name, partition_name),
  .fun = function(x) paste(x[["word"]], collapse = " "),
  .progress = "text"
  )
colnames(Y)[5] <- "fulltext"
}
}

