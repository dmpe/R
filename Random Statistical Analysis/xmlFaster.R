require(XML)
## http://hopstat.wordpress.com/2014/01/14/faster-xml-conversion-to-data-frames/
xmlToDF = function(doc, xpath, usewhich = TRUE, verbose = FALSE) {
  
  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)
  ## get the field names
  var.names <- lapply(nodeset, names)
  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  ## extract the values from all fields
  dl = lapply(fields, function(x) {
    if (verbose) print(paste0(" ", x))
  xpathSApply(doc, paste0(xpath, "/", x), xmlValue)
  })
  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))
  df = data.frame(matrix(NA, nrow=nrow(name.mat), ncol=ncol(name.mat)))
  names(df) = fields
  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)){
    rep.rows = name.mat[, icol]
    if (usewhich) rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }
  return(df)
}