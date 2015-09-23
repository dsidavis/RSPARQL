getClasses =
    # This gets the classes that are defined.
    # not what classes are used, i.e. have instances. That is getTypes
    #XXX add rdfs:subClassOf
function(owl = TRUE, ..., fun = SPARQL, asIs = FALSE)
{

   q = if(!owl) 
       "SELECT DISTINCT * {  {  ?s rdfs:class ?class . } UNION {?s owl:class ?class . }  UNION { ?s rdfs:subClassOf ?class . } ?class a ?classType }"
       else
           "select distinct ?resource {
                  ?resource rdf:type owl:Class .
            }"
    
   ans = fun(q, ns = commonNS, ...)
   if(nrow(ans$results) > 0) {
      classes = ans$results[[1]]
      if(asIs)
          classes
      else
          getNamesNS(classes)
   } else
       character()
}

getTypes =
    #  getTypes(fun = qmeamc)    
function(..., fun = SPARQL, asIs = FALSE)
{
  ans = fun("SELECT DISTINCT ?ty {  ?s rdf:type ?ty . }", ...)
  if(asIs)
      ans$results[[1]]
  else    
     getNamesNS( ans$results[[1]])
}

getPredicates =
    # This is used predicates, not defined properties in the RDFS or OWL way.
    # Use a different query for those.
    #
    #  getPredicates(fun = qmeamc)
    #
function(..., fun = SPARQL, asIs = FALSE)
{
  ans = fun("SELECT DISTINCT ?pred {  ?s ?pred ?ty . }", ...)
  if(asIs)
      ans$results[[1]]
  else
      getNamesNS( ans$results[[1]])
}


getPredicatesByClass =
function(..., classes = getClasses(..., fun = fun, asIs = TRUE), fun = SPARQL, asIs = FALSE)
{
   queries = sprintf("select ?property {  ?property owl:ObjectProperty %s  . }", classes)

   structure(lapply(queries, function(q) fun(q, ...)) , names = classes)
}



getNamesNS =
    # Take a vector of, e.g.,
    # <http://meamcentral.ucdavis.edu/ontology/mco#personDescription>
    # and return the personDescription values (after the # or at the end of the URL
    # and also use the namespace as the name for that element.
function(values)
{
   tmp = gsub("(^<|>$)", "", values)
   i = grepl("#", tmp)

   ans = character(length(tmp))
   ans[i] = gsub(".*#", "", tmp[i])
   names(ans)[i] = gsub("#.*", "", tmp[i])
   ans[!i] = basename(tmp[!i])
   names(ans)[!i] = dirname(tmp[!i])
   ans
}
