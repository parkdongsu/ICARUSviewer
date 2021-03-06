% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LCMMclustering.r
\name{latent_class_classification}
\alias{latent_class_classification}
\title{using latent class linear mixed model (LCMM), create groups according to longitudinally measured data trajectories}
\usage{
latent_class_classification(all_longitudinal_data_for_cluster,
  measurementConceptId_Trajectory, degreeOfPolynomial, cluster_number)
}
\arguments{
\item{all_longitudinal_data_for_cluster}{result of getAllLongitudinal code}

\item{measurementConceptId_Trajectory}{which measurement_concept_id do you want to use to cluster cohort}

\item{degreeOfPolynomial}{}

\item{cluster_number}{the number of clusters}
}
\description{
using latent class linear mixed model (LCMM), create groups according to longitudinally measured data trajectories
}
