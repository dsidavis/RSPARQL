subNSAbbrevs =
    #
    # This takes a collection of values, and identifies URIs, i.e. <http://....>
    # and also a mapping of namespace prefixes to URIs
    # It then converts URI values  to the prefix:value equivalent.
    # If it cannot match a URI to a namespace prefix, it leaves the value
    # unchanged.
function(vals, ns = commonNS)
{
  orig = vals
  rx = "^<(.*)>$"
  w = grep(rx, vals)
  vals = gsub(rx, "<\\1", vals)

  ns = gsub(rx, "<\\1", ns)
   # XXX put <> back on any we didn't change
  moded = rep(FALSE, length(vals))
  for(abbrev in names(ns)) {
      rx = sprintf("^%s", ns[abbrev])
      m = grepl(rx, vals)
      vals[m] = gsub(rx, sprintf("%s:", abbrev), vals[m])
      moded = moded | m
  }
  if(!all(moded))
      vals[!moded] = orig[!moded]
      
  vals
}

