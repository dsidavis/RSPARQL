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

  if(TRUE) return(  lapply(attrs, getVarResults2, DOM, ns) )

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
   xpathSApply(DOM, sprintf("//s:result/s:binding[@name = '%s']", id), get_value, ns, namespaces = sparqlns)

# nodes <- getNodeSet(DOM, sprintf("//s:result/s:binding[@name = '%s']", id), namespaces = sparqlns)
# uriNodes <- xpathSApply(DOM, sprintf("//s:result/s:binding[@name = '%s']/s:uri/text()", id), xmlValue, namespaces = sparqlns)
# if(length(nodes) == length(uriNodes))
#     return(sapply(uriNodes, xmlValue))
# else
#     sapply(nodes, get_value, ns)
  
    
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

getVarResults2 =
function(id, DOM, ns)
{
   nodes = getNodeSet(DOM, sprintf("//s:result/s:binding[@name = '%s']", id), namespaces = sparqlns)
   ans = vector("list", length(nodes))
   
   nodeNames = xpathSApply(DOM, sprintf("//s:result/s:binding[@name = '%s']/s:*", id), xmlName, namespaces = sparqlns)
   i =  nodeNames == "uri" 
   ans[i] = paste0("<", sapply(nodes[i], xmlValue), ">")

   j =  nodeNames == "bnode" 
   ans[j] = paste0('_:genid', sapply(nodes[j], xmlValue), sep='')

   k =  nodeNames == "literal" 
   ans[k] = sapply(nodes[k], xmlValue)

   if(!all(i | j | k))
      browser()
   
   ans

# nodes <- getNodeSet(DOM, sprintf("//s:result/s:binding[@name = '%s']", id), namespaces = sparqlns)
# uriNodes <- xpathSApply(DOM, sprintf("//s:result/s:binding[@name = '%s']/s:uri/text()", id), xmlValue, namespaces = sparqlns)
# if(length(nodes) == length(uriNodes))
#     return(sapply(uriNodes, xmlValue))
# else
#     sapply(nodes, get_value, ns)


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
