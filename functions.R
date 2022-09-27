# functions ---------------------------------------------------------------

{
  
  # add an empty line
  add.empty.line=function(doc){
    body_add_par(doc, " ")
    return("empty line added")
  }
  #add page break
  add.page.break=function(doc){
    body_add_break(doc, pos="after")
    return("page break added")
  }
  
  # start landscape
  start.landscape=function(doc){
    doc=body_end_section_continuous(doc)
    return("landscape orientation started")
  }
  # end landscape
  end.landscape=function(doc){
    doc=body_end_section_landscape(doc)
    return("landscape orientation ended")
  }
  
  # add a title
  add.title=function(doc, my.title){
    my.prop=fp_text(font.size = 14, bold = TRUE, font.family = "Times")
    the.title=fpar(ftext(my.title, prop=my.prop))
    body_add_fpar(doc, the.title)
    body_add_par(doc, " ")
    return("title added")
  }
  
  # add an image, such as a jpg or png file
  add.image=function(doc, image, h=5, w=5){
    body_add_img(doc, src=image,
                 height=h, width=w,
                 style="centered")
    return("image added")
  }
  
  # add a data frame as a table
  add.table=function(doc, tbl, col.keys=NULL, col.digits=NULL){
    # create basic flextable
    f.table=qflextable(tbl)
    
    # set numbers of decimals for numeric variables, if specified
    if(!is.null(col.keys)){
      for(j in 1:length(col.keys)){
        f.table=colformat_num(x=f.table,
                              col_keys=col.keys[j],
                              digits=col.digits[j])
      }
    }
    
    # set table borders
    f.table=border_outer(f.table, part="all",
                         border=fp_border(color="black", width = 1))
    f.table=border_inner_h(f.table, part="all",
                           border=fp_border(color="black", width = 1))
    f.table=border_inner_v(f.table, part="all",
                           border=fp_border(color="black", width = 1))
    
    # set fonts
    f.table=font(f.table,  fontname = "Times", part = "all")
    # also set the table's header font as bold
    f.table=bold(f.table, part = "header")
    
    # add the table to the document
    flextable::body_add_flextable(doc, 
                                  value = f.table, 
                                  align = "left" )
    return("table added")
  }
  
}


# Compute pointwise one-sided 'p-values',  i.e. for each time point compute the proportion of replicate (bootstrapped) trends that are positive.
pointwisepval = function(x) {
  isg = x$trendFrame$isGridP
  grad = poptrend:::getGradient(x$bootTrend[isg, ])
  grad = rbind(grad[1,], grad, grad[nrow(grad),])
  pval = rowMeans(grad > 0)
  out = data.frame(x$trendFrame[[x$timeVar]][isg], pval)
  colnames(out) = c(x$timeVar, 'pval')
  out
}

getZones = function(x, alpha = 0.05) {
  df = pointwisepval(x)
  seqs = function(gp) {
    inds = poptrend:::getRuns(gp)
    lims = NULL
    if (length(inds) > 0) {
      lims = data.frame(df[[x$timeVar]][inds[,1]], df[[x$timeVar]][inds[,2]]) 
      colnames(lims) = c('start', 'end')
    }
    lims
  }
  list(increases = seqs(which(df$pval > 1-alpha/2)), decreases = seqs(which(df$pval  < alpha/2)))
}

