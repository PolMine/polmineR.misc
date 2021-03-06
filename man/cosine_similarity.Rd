% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/cosine_similarity_method.R
\docType{methods}
\name{cosine_similarity}
\alias{cosine_similarity}
\alias{cosine_similarity,TermDocumentMatrix-method}
\alias{cosine_similarity,numeric-method}
\title{Calculate cosine similarity.}
\usage{
cosine_similarity(x, ...)

\S4method{cosine_similarity}{numeric}(x, y)

\S4method{cosine_similarity}{TermDocumentMatrix}(x, y = NULL,
  progress = TRUE, verbose = TRUE, mc = FALSE)
}
\arguments{
\item{...}{further parameters (for use with blapply)}

\item{y}{matrix from which the values are taken}

\item{progress}{logical}

\item{verbose}{logical}

\item{mc}{logical, or if numeric, providing number of cores}

\item{y}{simple triplet matrix}

\item{tool}{}

\item{method}{defaults to "cosine", no other method is implemented}
}
\description{
Calculate cosine similarity of two vectors.
Note: Much faster for pairwise comparisons than proxy::simil.
}

