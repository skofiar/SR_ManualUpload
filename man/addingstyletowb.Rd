% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddStyleToWorkingbook.R
\name{addingstyletowb}
\alias{addingstyletowb}
\title{Function that adds a row-alternating Excel-style to the Excel.
This should help the user to better understand/be able to read the rows.
We do this, by defining the Excel styles in the function itself and then write
to the corresponding places the style!}
\usage{
addingstyletowb(wb, sheetnm, data)
}
\arguments{
\item{wb}{Current WorkingBook.}

\item{sheetnm}{Excel-Sheet name, in which the data should be written in and
the style should be applied to}

\item{data}{Data table to which the style should be applied to. We are not using
the data table content itself but more the characteristics of it like
the nrow and ncol.}
}
\value{
Nothing is going to be returned.
Only the style is added in this function
}
\description{
Function that adds a row-alternating Excel-style to the Excel.
This should help the user to better understand/be able to read the rows.
We do this, by defining the Excel styles in the function itself and then write
to the corresponding places the style!
}
