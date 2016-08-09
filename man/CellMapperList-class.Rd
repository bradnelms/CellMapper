\name{CellMapperList-class}
\docType{class}

\alias{CellMapperList}
\alias{CellMapperList-class}
\alias{show,CellMapperList-method}

\title{
Class to contain expression data pre-processed for the 'CellMapper' package.
}
\description{
Container for expression data that has been pre-processed using the 'CM.prep' function. 'CellMapperList' class can be provided directly to the 'CM.search' function to predict genes expressed selectively in specific cell types using the 'CellMapper' package.
}
\section{creating objects}{
'CellMapperInputData' instances are usually created through the 'CM.prep' function.
}
\references{
B.D. Nelms, L. Waldron, L.A. Barrera, A.W. Weflen, J.A. Goettel, G. Guo, R.K. Montgomery, M.R. Neutra, D.T. Breault, S.B. Snapper, S.H. Orkin, M.L. Bulyk, C. Huttenhower and W.I. Lencer. CellMapper: rapid and accurate inference of gene expression in difficult-to-isolate cell types. Genome Biol 2016, Accepted in Principle.
}
\author{
Brad Nelms
}
\seealso{
\code{\link{CM.search}}, \code{\link{CM.prep}}
}
\keyword{classes}
