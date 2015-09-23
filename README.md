This is a package that allows R to act as SPARQL client for sending
queries to SPAQRL endpoints and receiving results and converting them
to R data frames.
This explicitly extends the original SPARQL package on CRAN and
we hope to integrate the changes back to that package rather than forking two
developments.
The changes here are to 
  a) add new features to the SPARQL() function, e.g., add a curl handle,
    insert PREFIX commands by identifying namespaces used in the query,
  b) improve the performance of converting the XML in a query result to a data frame,
  c) add support for a JSON response,
  d) simplify and "correct" some of the code (e.g., to ensure a data frame when there is a
    single variable/column in the result)

This is probably a temporary package that I hope to push back to the original package author
rather than have a separate fork.