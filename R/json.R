processJSONResults =
function(tf, parser_args, sparqlns, ns)                  
{
    if(is.raw(tf))
       tf = rawToChar(tf)
       
    ans = fromJSON(tf)
    res = ans$results$bindings

    cols = lapply(seq(along = ans$head$vars), convertJSONColumn, res)

    df = data.frame(cols, stringsAsFactors = FALSE)
    names(df) = ans$head$vars
    df
}

convertJSONColumn =
function(i, res, valueOnly = TRUE)
{
#Simple with no type info:
  vals = sapply(res, function(x) x[[i]]["value"])
  if(valueOnly)
     return(vals)
  
# Otherwise, we have to examine the data type
  type = sapply(res, function(x) x[[i]]["type"])
  dtype = rep(as.character(NA), length(vals))

  i = type == "literal"
  dtype[i] = sapply(res[i], function(x) x[[i]]["datatype"])

# Now convert the types with a data type
  
#    rows = sapply(res, function(x) x[[i]][1:3])
#    rows[2, ]
}

