#library(XML)
#library(RCurl)

sparqlns <- c('s'='http://www.w3.org/2005/sparql-results#')
commonNS <- c('xsd' = '<http://www.w3.org/2001/XMLSchema#>',
              'rdf' = '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
              'rdfs' = '<http://www.w3.org/2000/01/rdf-schema#>',
              'owl' ='<http://www.w3.org/2002/07/owl#>',
              'skos' = '<http://www.w3.org/2004/02/skos/core#>',
              'dc' = '<http://purl.org/dc/elements/1.1/>',
              'foaf' = '<http://xmlns.com/foaf/0.1/>',
              'wgs84' = '<http://www.w3.org/2003/01/geo/wgs84_pos#>',
              'qb' = '<http://purl.org/linked-data/cube#>')


#
# Read SPARQL results from end-point
#
SPARQL <- function(url = "http://localhost/", query="", 
                   ns = NULL, param = "",
                   update = FALSE, 
                   addPrefix = length(ns) & !grepl("PREFIX", query),
                   extra = NULL, format="xml", parser_args = list(),
                   curl = getCurlHandle(),
                   asText = FALSE, ...)
{
  if (!is.null(extra)) 
    extrastr <- paste('&', sapply(seq(1,length(extra)),
                                  function (i) { paste(names(extra)[i],'=',URLencode(extra[[i]]), sep="") }),
                      collapse="&", sep='')
  else 
    extrastr <- ""


  if(!update) {
    if (param == "") 
      param <- "query"

    if(addPrefix && length(ns)) 
       query = paste( c(sprintf("PREFIX %s: %s", names(ns), ns), "", q), collapse = "\n")

    if(format == 'xml') {
        args = list(query = query)
        args = append(args, extra)
        curl_args = list(...)        
        curl_args[["httpheader"]] = c(Accept="application/sparql-results+xml")
        tf <- getForm(url, .params = args, .opts = curl_args, curl = curl, ...)
#      tf <- do.call(getURL, append(list(url = paste(url, '?', param, '=', gsub('\\+','%2B', URLencode(query, reserved = TRUE)), extrastr, sep=""),
#                                               httpheader = c(Accept="application/sparql-results+xml"), curl = curl),
#                                          curl_args))
        if(asText)
            return(tf)
        
        df <- processXMLResults(tf, parser_args, sparqlns, ns)      

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


processXMLResults =
function(txt, parser_args, sparqlns, ns)
{
    DOM <- if(is(txt, "XMLInternalDocument"))
              txt
           else
              DOM <- do.call(xmlParse, append(list(txt), parser_args))
    
    if(length(getNodeSet(DOM, '//s:result[1]', namespaces = sparqlns)) == 0) {
        df <- data.frame()
    } else {
        nodes = getNodeSet(DOM, '//s:head/s:variable', namespaces = sparqlns)
        
        attrs <- sapply(nodes, xmlGetAttr, "name")

        ns2 <- noBrackets(ns)

        res <- xget_attr(attrs, DOM, ns2)

        rm(DOM)

        if(length(attrs) == 1) 
           df <- structure(data.frame(unlist(res), stringsAsFactors = FALSE), names = attrs)
        else {
            df <- data.frame(res, stringsAsFactors = FALSE)
            names(df) = attrs
            rm(res)
        
                # FIXME: find neater way to unlist columns
#            for(i in 1:length(df)) 
#                df[[i]] <- as.vector(unlist(df[[i]]))
        }
    }

    df
}



readTSVstring <- function(text, ...) {
  dfr <- read.delim(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

readCSVstring <- function(text, ...) {
  dfr <- read.csv(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

get_attr <- function(attrs, DOM, ns) {
  rs <- getNodeSet(DOM, '//s:result', namespaces = sparqlns)
  t(sapply(rs,
           function(r) { 
             sapply(attrs,
                    function(attr) {
                      get_value(getNodeSet(r, 
                                           paste('.//s:binding[@name="',attr,'"]/*[1]',
                                                 sep=''),
                                           namespaces=sparqlns)[[1]],
                                ns)
                    },simplify=FALSE)
           }))
}


xget_attr <- function(attrs, DOM, ns) {

  if(TRUE) return(  lapply(attrs, getVarResults, DOM, ns) )

   # we should really check that there is a node for each cell in the resulting data frame
   # i.e. that there are as many nodes for each variable. This is a simple test.
  nodes = lapply(sprintf("//s:result/s:binding[@name = '%s']", attrs), function(q) getNodeSet(DOM,  q, sparqlns))
  if(!all(  sapply(nodes, length) == length(nodes[[1]])))
      return(get_attr(attrs, DOM, ns))

   lapply(nodes, function(ll) sapply(ll, get_value, ns))
#   lapply(attrs, getVarResults, DOM, ns)
}

getVarResults =
function(id, DOM, ns)
{    
  rs <- xpathSApply(DOM, sprintf("//s:result/s:binding[@name = '%s']", id), get_value, ns, namespaces = sparqlns)

#lapply(rs,
#         function(r) { 
#           sapply(attrs,
#                  function(attr) {
#                    get_value(getNodeSet(xmlDoc(r),
#                                         paste('//s:binding[@name="',attr,'"]/*[1]',
#                                               sep=''),
#                                         namespaces=sparqlns)[[1]],
#                              ns)
#                  },simplify=FALSE)
#         })
}



get_value <- function(node, ns) {
  # FIXME: very slow...
  if (is.null(node))
      return(NA)
  
  doc <- node # xmlDoc(node)
  uri = xpathSApply(doc, './s:uri', xmlValue, namespaces = sparqlns)
  if(length(uri) == 0) {
    literal = xpathSApply(doc, './s:literal', xmlValue, namespaces = sparqlns)
    if(length(literal) == 0) {
      bnode = xpathSApply(doc, './s:bnode', xmlValue, namespaces = sparqlns)
      if (length(bnode) == 0) { # error
        '***oops***'
      } else { # found bnode
        paste('_:genid', bnode, sep='')
      }
    } else { # found literal
      lang = xpathApply(doc, './s:literal', xmlGetAttr, "xml:lang", namespaces=sparqlns)
      if(is.null(lang[[1]])) {
        type = xpathApply(doc, './s:literal', xmlGetAttr, "datatype", namespaces=sparqlns)
        if(is.null(type[[1]])) {
          literal
        } else {
          interpret_type(type,literal,ns)
        }
      } else {
        paste('"', literal, '"@', lang, sep='')
      }
    }
  } else { # found URI
    qname = qnames(uri, ns)
    if(qname == uri)
      paste('<', uri, '>', sep="")  # just uri saves about 6% for 9000 x 2
    else
      qname
  }
}

noBrackets <- function(ns) {
  sapply(ns, function(br_ns) {
                if(substr(br_ns,1,1)=='<')
                    substr(br_ns,2,nchar(br_ns)-1)
                else
                    br_ns
  })
}

substNS <- function(str0, ns) {
  regex <- paste('^', ns[2], sep="")
  gsub(regex, paste(ns[1], ":", sep=""), str0)
}

qnames <- function(str0, ns_list) {
  if(!length(ns_list))
    str0
  else
    substNS(qnames(str0, ns_list[-1:-2]), ns_list[1:2])
}

interpret_type <- function(type, literal,ns) {

      # FIXME: work out all simple types
  if(type == "http://www.w3.org/2001/XMLSchema#double" ||
     type == "http://www.w3.org/2001/XMLSchema#float" ||
     type == "http://www.w3.org/2001/XMLSchema#decimal")
    as.double(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#integer" ||
          type == "http://www.w3.org/2001/XMLSchema#int" ||
          type == "http://www.w3.org/2001/XMLSchema#long" ||
          type == "http://www.w3.org/2001/XMLSchema#short" ||
          type == "http://www.w3.org/2001/XMLSchema#byte" ||
          type == "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" ||
          type == "http://www.w3.org/2001/XMLSchema#unsignedLong" ||
          type == "http://www.w3.org/2001/XMLSchema#unsignedShort" ||
          type == "http://www.w3.org/2001/XMLSchema#unsignedInt" ||
          type == "http://www.w3.org/2001/XMLSchema#unsignedByte" ||
          type == "http://www.w3.org/2001/XMLSchema#positiveInteger" ||
          type == "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" ||
          type == "http://www.w3.org/2001/XMLSchema#negativeInteger")
        as.integer(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#boolean")
         as.logical(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#string" ||
            type == "http://www.w3.org/2001/XMLSchema#normalizedString")
         literal
  else if(type == "http://www.w3.org/2001/XMLSchema#dateTime")
        as.POSIXct(literal,format="%FT%T")
  else if(type == "http://www.w3.org/2001/XMLSchema#time")
        as.POSIXct(literal,format="%T")
  else if(type == "http://www.w3.org/2001/XMLSchema#date")
        as.POSIXct(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#gYearMonth")
        as.POSIXct(literal,format="%Y-%m")
  else if(type == "http://www.w3.org/2001/XMLSchema#gYear")
        as.POSIXct(literal,format="%Y")
  else if(type == "http://www.w3.org/2001/XMLSchema#gMonthDay")
        as.POSIXct(literal,format="--%m-%d")
  else if(type == "http://www.w3.org/2001/XMLSchema#gDay")
        as.POSIXct(literal,format="---%d")
  else if(type == "http://www.w3.org/2001/XMLSchema#gMonth")
        as.POSIXct(literal,format="--%m")
  else {
      qname <- qnames(type, ns)
      if(unlist(qname) == unlist(type))
          type_uri <- paste('<', type, '>', sep="")
      else
          type_uri <- qname
      paste('"', literal, '"^^', type_uri, sep="")
  }
}

dropNS <- function(df, ns) {
  data.frame(lapply(df, 
                    function(c) {
                      if(is.factor(c)) {
                        c <- as.character(c)
                        c <- qnames(c, ns)
                        return(as.factor(c))
                      }
                      if (is.character(c))
                        return(qnames(c, ns))
                      return(c)
                      } ))
}



sparqltest <- function(...) {
  SPARQL(url='http://semanticweb.cs.vu.nl/lop/sparql/',
         query='SELECT ?et ?r ?at ?t 
                WHERE { 
                  ?e sem:eventType ?et .
                  ?e sem:hasActor ?a . 
                  ?a sem:actorType ?at . 
                  ?e sem:hasPlace ?p . 
                  ?p eez:inPiracyRegion ?r .
                  ?e sem:hasTimeStamp ?t . }',
         ns=c('lop','<http://semanticweb.cs.vu.nl/poseidon/ns/instances/>',
              'eez','<http://semanticweb.cs.vu.nl/poseidon/ns/eez/>'),
         ...)
}
