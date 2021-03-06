% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerSurvSVM.R
\name{LearnerSurvSVM}
\alias{LearnerSurvSVM}
\alias{mlr_learners_surv.svm}
\title{Regression, Ranking and Hybrid Support Vector Machines Survival Learner}
\description{
Calls \code{\link[survivalsvm:survivalsvm]{survivalsvm::survivalsvm()}}.
\itemize{
\item crank is predicted by \code{\link[survivalsvm:predict.survivalsvm]{survivalsvm::predict.survivalsvm()}}
}

Four possible SVMs can be implemented, dependent on the \code{type} parameter. These correspond
to predicting the survival time via regression (\code{regression}), predicting a continuous rank
(\code{vanbelle1}, \code{vanbelle2}), or a hybrid of the two (\code{hybrid}).
Whichever \code{type} is chosen determines how the \code{crank} predict type is calculated,
but in any case all can be considered a valid continuous ranking.

To be in line with the Van Belle papers and to prevent the learner crashing without
user-set parameters, the default \code{diff.meth} is set to \code{diffmeth3} with \code{gamma.mu} equal to 0.1.
}
\section{Dictionary}{

This \link[mlr3:Learner]{Learner} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3:mlr_learners]{mlr_learners} or with the associated sugar function \link[mlr3:lrn]{lrn()}:\preformatted{LearnerSurvSVM$new()
mlr_learners$get("surv.svm")
lrn("surv.svm")
}
}

\section{Meta Information}{

\itemize{
\item Type: "surv"
\item Predict Types: \code{crank}
\item Feature Types: \verb{integer, numeric}
\item Properties: \code{-}
\item Packages: \CRANpkg{survivalsvm}
}
}

\references{
\cite{mlr3proba}{vanbelle_2010}

\cite{mlr3proba}{vanbelle_2011}
}
\seealso{
Other survival learners: 
\code{\link{LearnerSurvBlackboost}},
\code{\link{LearnerSurvCVGlmnet}},
\code{\link{LearnerSurvCoxPH}},
\code{\link{LearnerSurvFlexible}},
\code{\link{LearnerSurvGBM}},
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
\code{\link{LearnerSurvRpart}}
}
\concept{survival learners}
\section{Super classes}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{\link[mlr3proba:LearnerSurv]{mlr3proba::LearnerSurv}} -> \code{LearnerSurvSVM}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LearnerSurvSVM$new()}}
\item \href{#method-clone}{\code{LearnerSurvSVM$clone()}}
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
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvSVM$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerSurvSVM$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
