% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labeler.R
\name{labeler}
\alias{labeler}
\title{Label Data set}
\usage{
library(atRfunctions)
labeler(data, tool,
        survey_label = "label:English",
        choice_lable = "label:English",
        multi_response_sep = ";"
        )
}
\arguments{
\item{data}{data set}

\item{tool}{the path to the data collection tool}

\item{survey_label}{column name for the question labels in 'survey' sheet of the data collection tool. The default value is 'label:English'}

\item{choice_lable}{column name for value labels in 'choices' sheet of the data collection tool. The default value is 'label:English'}

\item{multi_response_sep}{separator for the multiple select questions. The default value is ';'}
}
\description{
Attach value labels to single-select and multiple select questions
}
