% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recanting_twins.R
\name{recanting_twins}
\alias{recanting_twins}
\title{Mediation Analysis With Intermediate Confounding Using Recanting Twins}
\usage{
recanting_twins(
  data,
  W,
  A,
  Z,
  M,
  Y,
  outcome_type = c("binomial", "continuous"),
  .control = .recanting_twins_control()
)
}
\arguments{
\item{data}{\[\code{data.frame}\]\cr}

\item{W}{trt \[\code{character}\]\cr}

\item{A}{trt \[\code{character}(1)\]\cr}

\item{Z}{trt \[\code{character}\]\cr}

\item{M}{trt \[\code{character}\]\cr}

\item{Y}{trt \[\code{character(1)}\]\cr}

\item{outcome_type}{trt \[\code{character}\]\cr}

\item{.control}{trt \[\code{list}\]\cr}
}
\description{
Mediation Analysis With Intermediate Confounding Using Recanting Twins
}
