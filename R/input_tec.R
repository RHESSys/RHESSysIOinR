#' input_tec
#'
#' Tiny wrapper to simplify input of tec events
#'
#' @param ... Vector(s) containing tec events in format c(<year>,<month>,<day>,<hour>,<name/event type>).
#' Additional tec events can be added just as additional arguments.
#' @author Will Burke
#'
#' @export

input_tec = function(...){
  b = list(...)
  if(class(b[[1]])=="data.frame"){
    return(b[[1]])
  } else{
    if(length(b[[1]])!=5){
      c = unlist(b)
      b = split(c,rep(c(1:(length(c)/5)),each=5))
    }
    a = data.frame(t(data.frame(b)),stringsAsFactors = FALSE) # list to df, transpose df autoconverts to mat, back to df
    a[,1] = as.numeric(a[,1]) # this could be a sapply or something but whatever
    a[,2] = as.numeric(a[,2])
    a[,3] = as.numeric(a[,3])
    a[,4] = as.numeric(a[,4])
    rownames(a) = c() # gross rownames gone
    colnames(a) = c("year","month","day","hour","name") # not sure if needed, but nice anyways
    return(a)
  }
}
