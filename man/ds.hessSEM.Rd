% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.hessSEM.R
\name{ds.hessSEM}
\alias{ds.hessSEM}
\title{Combines values into a vector or list in the server-side}
\usage{
ds.hessSEM(
  resource.name,
  inFile,
  blockList.name,
  varType.name,
  SEMGraph.name,
  autoAddIntercept,
  gammaInit,
  nIter,
  burnin,
  nChains,
  seed,
  method,
  datasources = NULL
)
}
\arguments{
\item{resource.name}{is the name of the shell resource on the server which will be used to
obtains the variables on the columns and observations on the rows.}

\item{inFile}{is the name of the file, obtained via the shell resource on the server, which
contains the variables on the columns and observations on the rows.}

\item{blockList.name}{is the name of the variable on the server which contains the list of
blocks in the model; each element of the list contains the (column) indices of the variables
in each block, with respect to the data file}

\item{varType.name}{is the name of the variable on the server which contains variable type
for each column in the data file; coded as: 0 - continuous, 1- binary, 2 - categorical. Note
that categorical variables cannot be imputed}

\item{SEMGraph.name}{is the name of the variable on the server which contains graph adjacency
matrix representing the SEM structure between the blocks. Edges represented as 2 indicate
variables that will be always included in the regression model}

\item{autoAddIntercept}{is the should the c++ code automatically add an intercept to every
equation? Value should be TRUE or FALSE}

\item{gammaInit}{is the gamma initialisation to either all-zeros ("0"), all ones ("1"),
randomly ("R") or (default) MLE-informed ("MLE")}

\item{nIter}{is the number of iterations for the MCMC procedure}

\item{burnin}{is the number of iterations (or fraction of iterations) to discard at the
start of the chain}

\item{nChains}{is the number of parallel chains to run}

\item{seed}{is the pRNG seed}

\item{method}{is the gamma sampling method, where 0=MC^3 and 1=Thompson-sampling inspired
novel method}

\item{datasources}{a list of \code{\link{DSConnection-class}} 
objects obtained after login. If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link{datashield.connections_default}}.}
}
\value{
a list from each server/study containing the results of that server/study. Each results in the
list is named with the name allocated to the server/study during the login process. The results from
each server/study is a list containing the following named items:
HESS_beta_1: 
HESS_beta_2_MCMC: 
HESS_beta_2: 
HESS_gamma_1_MCMC: 
HESS_gamma_1: 
HESS_gamma_2_MCMC: 
HESS_gamma_2: 
HESS_logP: 
HESS_R2_complete_cases_1_MCMC: (if input contains NAs) 
HESS_R2_complete_cases_2_MCMC: (if input contains NAs) 
HESS_R2_full_data_1_MCMC: 
HESS_R2_full_data_2_MCMC:
}
\description{
Concatenates objects into one vector.
}
\details{
To avoid combining the character names and not the
}
\author{
DataSHIELD Development Team
}
