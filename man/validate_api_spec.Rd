% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_api_spec.R
\name{validate_api_spec}
\alias{validate_api_spec}
\title{Validate OpenAPI Spec}
\usage{
validate_api_spec(
  pr,
  ...,
  ruleset = c("minimal", "recommended", "recommended-strict"),
  verbose = TRUE
)
}
\arguments{
\item{pr}{A Plumber API}

\item{...}{Ignored}

\item{ruleset}{Character that determines the ruleset to use for validation. Can be one of "minimal", "recommended",
or "recommended-strict". Defaults to "minimal". See \href{https://redocly.com/docs/cli/commands/lint#options}{\verb{@redocly/cli} options} for more details.}

\item{verbose}{Logical that determines if a "is valid" statement is displayed. Defaults to \code{TRUE}}
}
\description{
Validate an OpenAPI Spec using \href{https://redocly.com/docs/cli/commands/lint}{\verb{@redocly/cli}}.
}
\details{
If any warning or error is presented, an error will be thrown.

This function is \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} and may be altered, changed, or removed in the future.
}
\examples{
\dontrun{
pr <- plumb_api("plumber", "01-append")
validate_api_spec(pr)
}
}
