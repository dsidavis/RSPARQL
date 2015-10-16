#library(XML)
#library(RCurl)

sparqlns <- c('s'='http://www.w3.org/2005/sparql-results#')
commonNS <- c('xsd' = '<http://www.w3.org/2001/XMLSchema#>',
              'rdf' = '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
              'rdfs' = '<http://www.w3.org/2000/01/rdf-schema#>',
              'owl' ='<http://www.w3.org/2002/07/owl#>',
              'skos' = '<http://www.w3.org/2004/02/skos/core#>',
              'dc' = '<http://purl.org/dc/elements/1.1/>',
              dcterms = '<http://purl.org/dc/terms/>',
              'foaf' = '<http://xmlns.com/foaf/0.1/>',
              'wgs84' = '<http://www.w3.org/2003/01/geo/wgs84_pos#>',
              'qb' = '<http://purl.org/linked-data/cube#>')


#
# Read SPARQL results from end-point
#
SPARQL <- function(url = "http://localhost/", query="", 
                   ns = commonNS, param = "",
                   update = FALSE, 
                   addPrefix = length(ns) & !grepl("PREFIX", query),
                   extraArgs = NULL, format = "xml", parser_args = list(),
                   curl = getCurlHandle(),
                   asText = FALSE,
                   contentType = sprintf("application/sparql-results+%s", format),
                   quiet = TRUE,
                   ...)
{

  ns = fixNamespaces(ns)
    
  if(!update) {
    if (param == "") 
      param <- "query"

    if(addPrefix && length(ns)) 
       query = addNamespaces(query, c(ns, commonNS))

    if(format %in% c('xml', 'json')) {
        args = list(query = query)
        args = append(args, extraArgs)
        curl_args = list(...)        
        curl_args[["httpheader"]] = c(Accept = contentType)
        if(!quiet)
            message("submitting SPARQL query\n")
        
        tryCatch( tf <- getForm(url, .params = args, .opts = curl_args, curl = curl, binary = FALSE),
                  Bad_Request = function(e) {
                      e$message = e$body
                      class(e) = c("BadSPARQLRequest" , class(e))
                      stop(e)
                  })

       
        if(asText)
            return(tf)
        
        if(!quiet)
            message("processing SPARQL result\n")
        
        df <- if(format == "xml")
                processXMLResults(tf, parser_args, sparqlns, ns)
              else
                processJSONResults(tf, parser_args, sparqlns, ns)                  

    } else if (format %in% c('csv', 'tsv')) {
      tf <- do.call(getURL, append(list(url = paste(url, '?', param, '=', gsub('\\+','%2B', URLencode(query,reserved=TRUE)), extrastr, sep="")),
                                   curl_args))
      pfun = switch(format, csv = readCSVstring, tsv = readTSVstring)
      df <- do.call(readCSVstring, append(list(tf, blank.lines.skip = TRUE, strip.white = TRUE), parser_args))
      if (!is.null(ns)) 
        df <- dropNS(df, ns)
    } else {
      warning('unknown format "', format, '"\n\n')
      return(list(results = NULL, namespaces = ns))
    }
    
    list(results = df, namespaces = ns)
  } else {
      
    if (param == "") 
      param <- "update"

    extra[[param]] <- update
    do.call(postForm, append(list(url, .params = extra), curl_args))
  } 
}


addNamespaces =
    #
    #  Add PREFIX statements to the query for the ns-prefixes we recognize.
    #  Warn about the ones we don't.
    #
function(q, nsDefs)
{
  m = gregexpr("([a-zA-Z]+):", q)[[1]]
  ns = substring(q, m, m + attr(m, "match.length") - 2L)

  ns = setdiff(ns, c("http", "https"))
  
  i = match(ns, names(nsDefs))

  if(any(is.na(i))) 
      warning("namespace prefix/abbreviations with no definition: ", paste(ns[is.na(i)], collapse = ", "))
  
  if(any(!is.na(i))) {
      i = unique(i[ !is.na(i) ])
      paste(c(sprintf( "PREFIX %s: %s", names(nsDefs)[i], nsDefs[i]),
               "", q), collapse = "\n")
  } else
      q
}


fixNamespaces =
function(x)
{
  w = !grepl("^\\<.*\\>$", x)
  if(any(w)) 
      x[!w] = sprintf("<%s>", x[!w])

  x
}








