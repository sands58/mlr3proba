% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerSurvGBM.R
\name{LearnerSurvGBM}
\alias{LearnerSurvGBM}
\alias{mlr_learners_surv.gbm}
\title{Generalized Boosting Regression Modeling Survival Learner}
\description{
Calls \code{\link[gbm:gbm]{gbm::gbm()}}.
\itemize{
\item lp is predicted by \code{\link[gbm:predict.gbm]{gbm::predict.gbm()}}
\item \code{crank} is identical to \code{lp}
}

Parameter \code{distribution} is set to \code{coxph} as this is the only distribution implemented
in \code{\link[gbm:gbm]{gbm::gbm()}} for survival analysis; parameter \code{keep.data} is set to \code{FALSE} for efficiency.
}
\section{Dictionary}{

This \link[mlr3:Learner]{Learner} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3:mlr_learners]{mlr_learners} or with the associated sugar function \link[mlr3:lrn]{lrn()}:\preformatted{LearnerSurvGBM$new()
mlr_learners$get("surv.gbm")
lrn("surv.gbm")
}
}

\section{Meta Information}{

\itemize{
\item Type: "surv"
\item Predict Types: \verb{crank, lp}
\item Feature Types: \verb{integer, numeric, factor, ordered}
\item Properties: \verb{importance, missings, weights}
\item Packages: \CRANpkg{gbm}
}
}

\references{
\cite{mlr3proba}{freund_1997}

\cite{mlr3proba}{ridgeway_1999}

\cite{mlr3proba}{friedman_2000}

\cite{mlr3proba}{friedman_2001}

\cite{mlr3proba}{friedman_2001}

\cite{mlr3proba}{friedman_2002}

\cite{mlr3proba}{kriegler_2007}

\cite{mlr3proba}{burges_2010}
}
\seealso{
Other survival learners: 
\code{\link{LearnerSurvBlackboost}},
\code{\link{LearnerSurvCVGlmnet}},
\code{\link{LearnerSurvCoxPH}},
\code{\link{LearnerSurvFlexible}},
\code{\link{LearnerSurvGamboost}},
\code{\link{LearnerSurvGlmboost}},
\code{\link{LearnerSurvGlmnet}},
\code{\link{LearnerSurvKaplan}},
\code{\link{LearnerSurvMboost}},
\code{\link{LearnerSurvNelson}},
\code{\link{LearnerSurvObliqueRSF}},
\code{\link{LearnerSurvParametric}},
\code{\link{LearnerSurvPenalized}},
\code{\link{LearnerSurvRandomForestSRC}},
\code{\link{LearnerSurvRanger}},
\code{\link{LearnerSurvRpart}},
\code{\link{LearnerSurvSVM}}
}
\concept{survival learners}
\section{Super classes}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{\link[mlr3proba:LearnerSurv]{mlr3proba::LearnerSurv}} -> \code{LearnerSurvGBM}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LearnerSurvGBM$new()}}
\item \href{#method-importance}{\code{LearnerSurvGBM$importance()}}
\item \href{#method-clone}{\code{LearnerSurvGBM$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="format">}\href{../../mlr3/html/Learner.html#method-format}{\code{mlr3::Learner$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="help">}\href{../../mlr3/html/Learner.html#method-help}{\code{mlr3::Learner$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict">}\href{../../mlr3/html/Learner.html#method-predict}{\code{mlr3::Learner$predict()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict_newdata">}\href{../../mlr3/html/Learner.html#method-predict_newdata}{\code{mlr3::Learner$predict_newdata()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="print">}\href{../../mlr3/html/Learner.html#method-print}{\code{mlr3::Learner$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="reset">}\href{../../mlr3/html/Learner.html#method-reset}{\code{mlr3::Learner$reset()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="train">}\href{../../mlr3/html/Learner.html#method-train}{\code{mlr3::Learner$train()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvGBM$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importance"></a>}}
\subsection{Method \code{importance()}}{
The importance scores are extracted from the model slot \code{variable.importance}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvGBM$importance()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Named \code{numeric()}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvGBM$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
