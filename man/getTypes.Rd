\name{getTypes}
\alias{getTypes}
\title{Query meta-data of a SPARQL database}
\description{
  These functions implement generic SPARQL queries to
  obtain information about the structure of a SPARQL
  database in terms of its classes, types, predicates.
  This allows a user to get an understanding of the structure of the RDF triples
  and relationships in the database.
}
\usage{
getTypes(..., fun = SPARQL, asIs = FALSE)
getPredicates(..., fun = SPARQL, asIs = FALSE)
getPredicatesByClass(..., classes = getClasses(..., fun = fun, asIs = TRUE),
                      fun = SPARQL, asIs = FALSE)
getClasses(owl = TRUE, ..., fun = SPARQL, asIs = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{owl}{a logical value indicating whether to get the OWL and RDFS
    classes, or to query the RDF types.  If \code{FALSE}, this is really
    the same as the \code{getTypes} function.}
  \item{\dots}{any additional arguments to be passed to \code{fun} which
    makes the \code{\link{SPARQL}} request and query. These are
    arguments such as the URL of the SPARQL endpoint, curl options or a curl handle to customize the
    HTTP request, and so on.
   }
   \item{fun}{a function that will send the SPARQL query.
    The default of \code{\link{SPARQL}} works well, but the caller has
    to provide the URL of the endpoint. For an endpoint that requires
    authentication (e.g. a username and password), it is convenient to
    write  a simple function that passes these and the URL to
    \code{\link{SPARQL}} and then use this function  as the value of \code{fun}.
   }
  \item{asIs}{a logical value that controls whether the class/type
    values returned are left as URIs or broken into their name and namespaces.}
}
\details{
  These functions implement generic SPARQL queries to discover aspects
  of  the structure of the SPARQL database.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Duncan Temple Lang
}

\seealso{
\code{\link{SPARQL}}.
}
\examples{
U = "http://dbpedia.org/sparql"
getTypes(url = U)
}
\keyword{programming}
\concept{metadata}
