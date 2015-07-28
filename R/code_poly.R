#' Build polynomial predictor variables
#' 
#' Takes a data frame, name of predictor variable, and polynomial order. Creates polynomial-transformed predictor variables, adds them to the data frame and returns the result. Original data frame is unchanged, remember to assign the result.
#' 
#' @param df data frame, should not contain any variables called "predictor"
#' @param predictor string name of predictor variable
#' @param poly.order integer order of polynomial to be created
#' @param orthogonal logical value indicating whether polynomial should be orthogonal (default) or natural (aka raw)
#' @param draw.poly logical value indicating whether to create a graph showing tranformed polynomial predictor values, defaults to TRUE
#' @return Returns a data frame containing the original data and at least two new columns: "predictor".Index, and a column for each order of the polynomial-transformed predictor
#' @examples
#' WordLearnEx.gca <- code_poly(df=WordLearnEx, predictor="Block", poly.order=2)
#' Az.gca <- code_poly(df=Az, predictor="Time", poly.order=2, orthogonal=FALSE)
#' @section Contributors:
#' Originally written by Matt Winn \url{http://www.mattwinn.com/tools/R_add_polynomials_to_df.html} and revised by Dan Mirman to handle various corner cases.
code_poly <- function(df=NULL, predictor=NULL, poly.order=NULL, orthogonal=TRUE, draw.poly=TRUE){
  # Codes raw or orthogonal polynomial transformations of a predictor variable
  # be sure to not have an actual variable named "predictor" in your data.frame
  # ultimately adds 2 or more columns, including:
  # (predictor).Index, and a column for each order of the polynomials 
  #
  # Written by Matt Winn: http://www.mattwinn.com/tools/R_add_polynomials_to_df.html
  # Revised by Dan Mirman (11/3/2014): 
  #   - call to poly now uses predictor.index and 1:max instead of unique() to deal with out-of-order time bins
  #   - combined indexing and alignment with original data, mainly to avoid problems when poly.order==1

  # figure out which column is the predictor
  predictor.column.num <- which(colnames(df)==predictor)
  # copy that column into a column that is named "predictor"
  df$predictor <- df[,predictor.column.num]
  
  # convert choice for orthogonal into choice for raw
  raw <- (orthogonal-1)^2
  
  #check whether predictor steps are all equal
  #get unique values of predictor, sort them, and get step sizes
  step.sizes <- diff(sort(unique(df$predictor)))
  #check whether there is more than 1 unique step size
  #  and produce a warning if there is
  if(length(unique(step.sizes)) > 1){
    warning("Warning: predictor appears to have unequal step sizes, experimental solution will be applied. Current solution will not work for non-integer predictor values.")
    #use brute force approach of integer steps from min to max
    px <- data.frame(predictor = seq(min(df$predictor), max(df$predictor), by=1)) 
    px$predictor.index <- px$predictor - min(df$predictor) + 1
    #compute polynomial
    predictor.polynomial <- poly(1:max(px$predictor.index), poly.order, raw=raw)
    #add predictor values to df
    df <- merge(df, px) 
    # use predictor index as index to align 
    # polynomial-transformed predictor values with original dataset
    # (as many as called for by the polynomial order)
    df[, paste("poly", 1:poly.order, sep="")] <- 
      predictor.polynomial[df$predictor.index, 1:poly.order]
  } else {
    #create index of predictor (e.g. numbered time bins)
    df$predictor.index <- as.numeric(as.factor(df$predictor))
    #create x-order order polys (orthogonal if not raw)
    predictor.polynomial <- poly(1:max(df$predictor.index), poly.order, raw=raw)
    # use predictor index as index to align 
    # polynomial-transformed predictor values with original dataset
    # (as many as called for by the polynomial order)
    df[, paste("poly", 1:poly.order, sep="")] <- 
      predictor.polynomial[df$predictor.index, 1:poly.order]
  }
  
  
    
  # draw a plot of the polynomial transformations, if desired
  if (draw.poly == TRUE){
    # extract the polynomials from the df
    df.poly <- df[c("predictor", paste("poly", 1:poly.order, sep=""))]
    
    # melt from wide to long format
    df.poly.melt <- melt(df.poly, id.vars="predictor")
    
    # Make level names intuitive
    # don't bother with anything above 6th order. 
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly1"] <- "Linear"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly2"] <- "Quadratic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly3"] <- "Cubic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly4"] <- "Quartic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly5"] <- "Quintic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly6"] <- "Sextic"
    
    # change some column names for the output
    colnames(df.poly.melt)[colnames(df.poly.melt) == "variable"] <- "Order"
    
    poly.plot <- ggplot(df.poly.melt, aes(x=predictor, y=value, color=Order))+
      geom_line()+
      xlab(paste0(predictor, " (transformed polynomials)"))+
      ylab("Transformed value")+
      scale_color_brewer(palette="Set1")+
      theme_bw()
    
    print(poly.plot)
  }
  
  # restore correct column names
  colnames(df)[colnames(df) == "predictor.index"] <- paste0(predictor,".Index")
  df$predictor <- NULL
  return(df)
}