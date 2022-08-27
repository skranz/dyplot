#' Dynamic time series plots (wrapper to dygraphs)
#'
#' The function is a wrapper to the dygraph function in the package dygraph. It shows
#' time series plots with interactive java script. While dygraph needs an xts time series
#' object, dyplot works with a data frame. Missing observations can be filled if period is
#' provided.
#'
#' @param data a data.frame
#' @param xcol name of the column with the x-Axis variable. Ideally a datetime object
#' @param ycol names of the columns that are shown on the yaxis (used for a wide data fram)
#' @param var_col alternative to ycol for a data frame in long format, the column with the variable name
#' @param var_col the column with the values for a data frame in long format
#' @param interval if you have missing rows you can specify the interval of you data, e.g. "day" or "year" or "hour" to fill the gaps
#' @export
dyplot = function(data, xcol=colnames(data)[1], ycol = setdiff(colnames(data),xcol), var_col=NULL, val_col=NULL, interval=NULL) {
  restore.point("dyplot")
  library(dygraphs)

  # data in long format
  if (!is.null(var_col)) {
    if (is.null(val_col)) stop("If your data is in long format, you must specify both  var_col and val_col")
    data = data[,c(xcol,var_col, val_col)]
    data = pivot_wider(data,names_from=all_of(var_col),values_from = all_of(val_col))
    ycol = colnames(data)[-1]
  }

  ts = to.xts(data[,ycol,drop=FALSE], time=data[[xcol]], interval=interval )
  dygraph(ts)
}


#' Transform a data from to an xts object
to.xts = function(dat, time.col, interval=NULL, time = dat[[time.col]], fill=!is.null(interval)) {
  restore.point("to.xts")
  drop.rows = duplicated(time) | is.na(time)
  dat = dat[!drop.rows,,drop=FALSE]
  time= time[!drop.rows]

  if (fill) {
    full.time = seq(min(time, na.rm=TRUE), max(time, na.rm=TRUE), interval)
    dat = left_join(data.frame(.TIME=full.time),cbind(data.frame(.TIME=time),dat),by=".TIME")
    time = dat[,1]
    dat = dat[,-1,drop=FALSE]
  }
  d = as.xts(dat, time)
  #d <-na.approx(d)
  d
}
