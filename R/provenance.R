#' @title xsd_time
#' @return returns the time stamp of the query.
#' @export
xsd_time <- function() {
  xsd_time <- gsub(" ", "T", as.character(as.POSIXct(Sys.time(), tz = "UTC", format="%Y-%m-%d %H:%M:%s")))
  paste0('"', strsplit(xsd_time, "[.]")[[1]][1], 'Z"^^xsd:dateTime')
}


#' @title provenance_statement
#' @param startedAtTime the start time of the query
#' @param endedAtTime the end time of the query
#' @param wasAssociatedWith ORCID of the person performing the query
#' @param package the package used to perform the query
#' @return returns a provenance statement to be included as an atribute of the object.
#' @export
provenance_statement <- function(startedAtTime, endedAtTime,
                                    wasAssociatedWith = '<https://orcid.org/0000-0001-7513-6760>',
                                    package = "spotifyr" ) {
  sI <- sessionInfo()
  RSoftwareAgent <- gsub(" ", "_", trimws(strsplit(sI$R.version$version.string, "\\(")[[1]][1]))
  RSoftwareAgentDef <- paste0(':',
                            RSoftwareAgent, ' a prov:SoftwareAgent; rdfs:label "',
                            sI$R.version$version.string, '" . ' )

  if ( package %in% names(sI$otherPkgs)) {

    spotifyrSoftwareAgent <- sI$otherPkgs$spotifyr$Package

    spotifyrSoftwareAgentDef <- paste0(':', spotifyrSoftwareAgent,
                              ' a prov:SoftwareAgent; rdfs:label "',
                              sI$otherPkgs$spotifyr$Title,". Version: ",
                              sI$otherPkgs$spotifyr$Version, '" . ' )
  }

  definition_statements <- paste0(
    ':', wasAssociatedWith, ' a prov:Agent . ',
    RSoftwareAgentDef,
    spotifyrSoftwareAgentDef
  )

  definition_statements

  collection_statement <- paste0(':collectionActivity a prov:Activity; prov:startedAtTime: ',
         startedAtTime, '; prov:endedAtTime: ', endedAtTime,
         '; wasAssociatedWith: ', wasAssociatedWith,
         '; wasAssociatedWith: ', spotifyrSoftwareAgent,
         '; wasAssociatedWith: ', RSoftwareAgent, '.')

 paste0(definition_statements, collection_statement)
  }

