% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSurvSongAUC.R
\name{MeasureSurvSongAUC}
\alias{MeasureSurvSongAUC}
\alias{mlr_measures_surv.songAUC}
\title{Song and Zhou's AUC Survival Measure}
\description{
Calls \code{\link[survAUC:AUC.sh]{survAUC::AUC.sh()}}.

Assumes Cox PH model specification.
}
\details{
All measures implemented from \CRANpkg{survAUC} should be used with care, we are aware
of problems in implementation that sometimes cause fatal errors in R. In future updates these
measures will all be re-written and implemented directly in \code{mlr3proba}.
}
\section{Dictionary}{

This \link[mlr3:Measure]{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3:mlr_measures]{mlr_measures} or with the associated sugar function \link[mlr3:msr]{msr()}:\preformatted{MeasureSurvSongAUC$new()
mlr_measures$get("surv.songAUC")
msr("surv.songAUC")
}
}

\section{Meta Information}{

\itemize{
\item Type: \code{"surv"}
\item Range: \eqn{[0, 1]}{[0, 1]}
\item Minimize: \code{FALSE}
\item Required prediction: \code{lp}
}
}

\references{
\cite{mlr3proba}{song_2008}
}
\seealso{
Other survival measures: 
\code{\link{MeasureSurvBeggC}},
\code{\link{MeasureSurvChamblessAUC}},
\code{\link{MeasureSurvGonenC}},
\code{\link{MeasureSurvGrafSE}},
\code{\link{MeasureSurvGraf}},
\code{\link{MeasureSurvHarrellC}},
\code{\link{MeasureSurvHungAUC}},
\code{\link{MeasureSurvIntLoglossSE}},
\code{\link{MeasureSurvIntLogloss}},
\code{\link{MeasureSurvLoglossSE}},
\code{\link{MeasureSurvLogloss}},
\code{\link{MeasureSurvMAESE}},
\code{\link{MeasureSurvMAE}},
\code{\link{MeasureSurvMSESE}},
\code{\link{MeasureSurvMSE}},
\code{\link{MeasureSurvNagelkR2}},
\code{\link{MeasureSurvOQuigleyR2}},
\code{\link{MeasureSurvRMSESE}},
\code{\link{MeasureSurvRMSE}},
\code{\link{MeasureSurvSongTNR}},
\code{\link{MeasureSurvSongTPR}},
\code{\link{MeasureSurvUnoAUC}},
\code{\link{MeasureSurvUnoC}},
\code{\link{MeasureSurvUnoTNR}},
\code{\link{MeasureSurvUnoTPR}},
\code{\link{MeasureSurvXuR2}}

Other AUC survival measures: 
\code{\link{MeasureSurvChamblessAUC}},
\code{\link{MeasureSurvHungAUC}},
\code{\link{MeasureSurvSongTNR}},
\code{\link{MeasureSurvSongTPR}},
\code{\link{MeasureSurvUnoAUC}},
\code{\link{MeasureSurvUnoTNR}},
\code{\link{MeasureSurvUnoTPR}}

Other lp survival measures: 
\code{\link{MeasureSurvBeggC}},
\code{\link{MeasureSurvChamblessAUC}},
\code{\link{MeasureSurvGonenC}},
\code{\link{MeasureSurvHungAUC}},
\code{\link{MeasureSurvNagelkR2}},
\code{\link{MeasureSurvOQuigleyR2}},
\code{\link{MeasureSurvSongTNR}},
\code{\link{MeasureSurvSongTPR}},
\code{\link{MeasureSurvUnoAUC}},
\code{\link{MeasureSurvUnoTNR}},
\code{\link{MeasureSurvUnoTPR}},
\code{\link{MeasureSurvXuR2}}
}
\concept{AUC survival measures}
\concept{lp survival measures}
\concept{survival measures}
\section{Super classes}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{\link[mlr3proba:MeasureSurv]{mlr3proba::MeasureSurv}} -> \code{\link[mlr3proba:MeasureSurvIntegrated]{mlr3proba::MeasureSurvIntegrated}} -> \code{\link[mlr3proba:MeasureSurvAUC]{mlr3proba::MeasureSurvAUC}} -> \code{MeasureSurvSongAUC}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{type}}{(\code{character(1)})\cr
Type of measure, one of: \code{'cumulative'}, \code{'incident'}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MeasureSurvSongAUC$new()}}
\item \href{#method-clone}{\code{MeasureSurvSongAUC$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="aggregate">}\href{../../mlr3/html/Measure.html#method-aggregate}{\code{mlr3::Measure$aggregate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="format">}\href{../../mlr3/html/Measure.html#method-format}{\code{mlr3::Measure$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="help">}\href{../../mlr3/html/Measure.html#method-help}{\code{mlr3::Measure$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="print">}\href{../../mlr3/html/Measure.html#method-print}{\code{mlr3::Measure$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="score">}\href{../../mlr3/html/Measure.html#method-score}{\code{mlr3::Measure$score()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvSongAUC$new(
  integrated = TRUE,
  times,
  type = c("incident", "cumulative")
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{integrated}}{(\code{logical(1)})\cr
If \code{TRUE} (default), returns the integrated score; otherwise, not integrated.}

\item{\code{times}}{(\code{numeric()})\cr
If \code{integrate == TRUE} then a vector of time-points over which to integrate the score.
If \code{integrate == FALSE} then a single time point at which to return the score.}

\item{\code{type}}{(\code{character(1)})\cr
Determines the type of score, one of: \code{'cumulative'}, \code{'incident'}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvSongAUC$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
