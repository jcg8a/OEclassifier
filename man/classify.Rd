% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{classify}
\alias{classify}
\title{Classify open ended responses from surveys using Jaro-Winkler metric}
\usage{
classify(
  raw_data,
  dictionary,
  destiny = NULL,
  raw_header = FALSE,
  dic_header = FALSE
)
}
\arguments{
\item{raw_data}{Filepath to a csv or xlsx file}

\item{dictionary}{Filepath to a csv or xlsx file}

\item{destiny}{Destiny filepath string. if NULL setwd path will be used}

\item{raw_header}{TRUE if the raw_data has header}

\item{dic_header}{TRUE if the dictionary has header}
}
\value{
A tibble
}
\description{
Classify open ended responses from surveys using Jaro-Winkler metric
}
