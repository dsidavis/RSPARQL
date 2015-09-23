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
