
# Sreport
# Rreport
# Rmarkup
# RscriptMarkup
# scriptMarkup
# scriptReport

Rmarkup <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL,
                    cleanup=TRUE, parms=list(height=400,width=600), details=0, ...){
  
  cat("Preprocessing... \n")
  
  if(!file.exists(srcfile)){
    cat(sprintf("Error: file %s does not exist\n", srcfile))
    cat(sprintf("Your working directory is currently: %s\n", getwd()))
    stop(sprintf("Exiting...\n"),call.=FALSE)
  }
  
  if (.Platform$OS.type == "windows"){
    srcfile <- gsub("\\\\", "/", srcfile)
    destdir <- gsub("\\\\", "/", destdir)
  }
  
  if (!file.exists(destdir)){
    cat(sprintf("Error: destdir (destination directory) %s does not exist\n", destdir))
    cat(sprintf("Your working directory is currently: %s\n", getwd()))
    stop("Exiting...", call.=FALSE)
  }
  
  ## File has format: path/filename.extension
  ##
  path.filename  <- gsub("(.*)\\..*" ,"\\1", srcfile)            ## path/filename
  filename       <- gsub(".*/(.*)$","\\1",   path.filename)      ## filename
  destdir.filename.postfix         <- paste(destdir, "/", filename, "-",postfix, sep="")
  destdir.filename.postfix.html    <- paste(destdir.filename.postfix,".html", sep="")
  filename.postfix         <- paste(filename,"-",postfix, sep="")
  
  ## We need two temporary files
  ##
  tmpfile.name <- paste("_",paste(filename,tempfile(pattern="",tmpdir=""),sep="-"),sep="")
  if (.Platform$OS.type == "windows")
    tmpfile.name <- gsub("\\\\", "/", tmpfile.name)
  tmpfile.name <- gsub("/","", tmpfile.name)
  tmpfile.name.html  <- paste(tmpfile.name,".html",sep="")
  
  cat(sprintf(" srcfile            : %s\n",  srcfile))
  cat(sprintf(" filename           : %s\n",  filename))
  cat(sprintf(" filename.postfix   : %s\n",  filename.postfix))
  cat(sprintf(" destdir.filename.postfix         : %s\n",  destdir.filename.postfix))
  cat(sprintf(" destdir.filename.postfix.html    : %s\n",  destdir.filename.postfix.html))
  cat(sprintf(" tmpfile.name       : %s\n",  tmpfile.name))
  cat(sprintf(" tmpfile.name.html  : %s\n",  tmpfile.name.html))
  
  ff <- file.copy(srcfile, tmpfile.name, overwrite=TRUE)   ## Create copy of the source file
  if (!ff){
    stop("Can not create temporary file in working directory. Is the directory write protected?")
  }
  
  inlines   <- readLines(tmpfile.name)
  outlines  <- driver$setup(inlines, details)                    ## Preprocess source file
  write(outlines, file=tmpfile.name)
  cat("Preprocessing done...\n")

  
  Sweave(tmpfile.name, driver=RweaveHTML())

  
  cat("Postprocessing... \n")
  inlines    <- readLines(tmpfile.name.html)
  outlines   <- driver$writedoc(inlines, filename.postfix, tmpfile.name, parms)
  ## Postprocess html file
  
  if (!is.null(cssfile)){
    ##cssline <- "<link rel=stylesheet href=\"R2HTML.css\" type=text/css>"
    cssline <- paste("<link rel=stylesheet href=\"",cssfile,"\" type=text/css>",sep="")
    outlines <- c(cssline, outlines)
  }
  
  write(outlines, file=destdir.filename.postfix.html)
  driver$finish(tmpfile.name, filename.postfix, destdir.filename.postfix)
  cat("Postprocessing done... \n")
  
  if (cleanup){
    cat(sprintf("Cleaning up: removing temporary files\n   %s\n",
                toString(c(tmpfile.name,tmpfile.name.html))))
    file.remove(tmpfile.name)
    file.remove(tmpfile.name.html)
  }
  
  return(invisible(NULL))
}

HTMLreport <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL,
                       cleanup=TRUE, ...){
  cat("HTMLreport is deprecated; please use Rmarkup instead.\n")
}

Rscript2HTML <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL,
                         cleanup=TRUE, ...){
  cat("Rscript2HTML is deprecated; please use Rmarkup instead.\n")
}

RweaveHTMLreport <- function(){
  list(setup    = RweaveHTMLreportSetup,
       writedoc = RweaveHTMLreportWritedoc,
       finish   = RweaveHTMLreportFinish)
}

RweaveHTMLreportSetup <- function(srclines, details=0){
  
  key <- c(
           "^$",				## empty line
           "^#",    				## text line
           "^ *##? *@@|^ *##? *<<",	        ## begin code chunk
           "^ *##? *@ *$" 			## end code chunk
           )
  
  state <- c("RCODE", "emptyLine", "textLine", "beginCode", "endCode")
  
  get.token <- function(lll, key){
    ans <- which(c(lapply(key, grepl, lll),recursive=T))
    if (length(ans)==0)
      ans <- 0
    ans
  }
  
  
  anslines <- srclines
  inCode   <- 0
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]
    tok <- max(get.token(lll, key))    
    if (tok==3){
      inCode <- 1
    } else if (tok==4) {
      inCode <- 0
    }

    if (details>0)
      cat(sprintf("   -> tok: %2s; inCode: %i; state: %10s; line: %s\n",
                  toString(tok), inCode, state[tok+1], lll))
    
    if (inCode==0){
      lll <- .handleTextLine(lll,tok)
    } else {
      if (inCode==1){
        lll <- .handleRCodeLine(lll,tok)
      }
    }
    
    anslines[ii] <- lll
  }
  anslines
}

RweaveHTMLreportWritedoc <- function(srclines, filename.postfix, tmpfile.name, parms){
  anslines <- srclines
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]

    sss <- sprintf("<img height=%i width=%i ", parms$height, parms$width)
    lll <- gsub("<img height= width= ",          sss, lll)
    ##lll <- gsub("<img height= width= ",          "<img ", lll)

    lll <- gsub("<p align= center >",            "<p align= left >", lll)
    lll <- gsub("<p align='center'>",            "<p align= left >", lll)
    lll <- gsub("(<!--\\\\end\\{Schunk\\}!-->)", "</p> \\1",lll)
    
    lll <- gsub(tmpfile.name, filename.postfix, lll)
    anslines[ii] <- lll
  }
  anslines
}

RweaveHTMLreportFinish <- function(tmpfile.name, filename, destdir.filename){

  ##   cat("... RweaveHTMLreportFinish\n")
  ##   cat(sprintf("... tmpfile.name=%s \n... filename=%s \n... destdir.filename=%s\n",
  ##   tmpfile.name, filename, destdir.filename))

  ## Rename image files
  figfiles    <- glob2rx(paste(tmpfile.name, "-*.png", sep=""))
  currfiglist <- list.files(pattern=figfiles)
  #cat(sprintf(" currfiglist    : %s\n", toString(currfiglist)))
  if (length(currfiglist)>0){
    newfiglist  <- gsub(tmpfile.name, destdir.filename, currfiglist)
    #cat(sprintf(" newfiglist     : %s\n", toString(newfiglist)))
    for (ii in 1:length(currfiglist)){
      file.copy(currfiglist[ii], newfiglist[ii], overwrite=TRUE)
      file.remove(currfiglist[ii])
    }
  }
  ## FIXME: There might be a problem if we assign names to code chunks

}


.handleTextLine <- function(lll,tok){
    ## blank lines -> <br>
    input <- lll
    lll <- gsub("^*$", "<br>", lll)
    lll <- gsub("^##?$", "<br>", lll)

    ## Headers
    lll <- gsub("##+[[:blank:]]*======[[:blank:]]*(.*)[[:blank:]]*======[[:blank:]]*", "<h6>\\1</h6>", lll)
    lll <- gsub("##+[[:blank:]]*=====[[:blank:]]*(.*)[[:blank:]]*=====[[:blank:]]*",   "<h5>\\1</h5>", lll)
    lll <- gsub("##+[[:blank:]]*====[[:blank:]]*(.*)[[:blank:]]*====[[:blank:]]*",     "<h4>\\1</h4>", lll)
    lll <- gsub("##+[[:blank:]]*===[[:blank:]]*(.*)[[:blank:]]*===[[:blank:]]*",       "<h3>\\1</h3>", lll)
    lll <- gsub("##+[[:blank:]]*==[[:blank:]]*(.*)[[:blank:]]*==[[:blank:]]*",         "<h2>\\1</h2>", lll)
    lll <- gsub("##+[[:blank:]]*=[[:blank:]]*(.*)[[:blank:]]*=[[:blank:]]*",           "<h1>\\1</h1>", lll)
    ## Italics
    lll <- gsub("//([^/]*)//",
                '<span style="font-style: italic;">\\1</span>', lll)
    ## Bold
    lll <- gsub("\\*\\*([^\\*]*)\\*\\*",
                '<span style="font-weight: bold;">\\1</span>', lll)
    ## Underline
    lll <- gsub("__([^_]*)__",
                '<span style="text-decoration: underline;">\\1</span>', lll)
    ## True type (Courier New)
    lll <- gsub("&&([^&]*)&&",
                '<span style="font-family: Courier New;">\\1</span>', lll)

    ## Date
    lll <- gsub("##+[[:blank:]]*%%date",
                '\\<\\<echo=FALSE\\>\\>=\nSys.time()\n@', lll)
    ## 3 hashes -> a comment
    lll <- gsub("^[[:blank:]]*###+(.*)",
                "<!-- \\1 !-->", lll)
    ## 1 or 2 hashes -> text
    lll <- gsub("^[[:blank:]]*##?[[:blank:]]*(.*)",
                "\\1", lll)

    ans <- lll
    ##cat(sprintf(".handleTextLine:\n input: %s\n ans:    %s\n",input, ans))
    ans
}


.handleRCodeLine <- function(lll,tok){
    input <- lll

    lll <- gsub("^[[:blank:]]*##? *<<(.*)", "\\<\\<\\1", lll)

    ## Vanilla code chunk (<<>>=)
    lll <- gsub("^[[:blank:]]*##?@@$", "<<>>=", lll)

    ## Vanilla code chunk with figure (<<fig=T>>=)
    lll <- gsub("^[[:blank:]]*##?@@fig$", "<<fig=T>>=", lll)

    ans <- lll
    ##cat(sprintf("...handleRCodeLine:\n.....input: %s\n.....  ans: %s\n",input, ans))
    ans
}




## ## blank lines -> <br>
## lll <- gsub("^*$", "<br>", lll)
## lll <- gsub("^##?$", "<br>", lll)

## ## Headers
## lll <- gsub("##+[[:blank:]]*======[[:blank:]]*(.*)[[:blank:]]*======[[:blank:]]*", "<h6>\\1</h6>", lll)
## lll <- gsub("##+[[:blank:]]*=====[[:blank:]]*(.*)[[:blank:]]*=====[[:blank:]]*",   "<h5>\\1</h5>", lll)
## lll <- gsub("##+[[:blank:]]*====[[:blank:]]*(.*)[[:blank:]]*====[[:blank:]]*",     "<h4>\\1</h4>", lll)
## lll <- gsub("##+[[:blank:]]*===[[:blank:]]*(.*)[[:blank:]]*===[[:blank:]]*",       "<h3>\\1</h3>", lll)
## lll <- gsub("##+[[:blank:]]*==[[:blank:]]*(.*)[[:blank:]]*==[[:blank:]]*",         "<h2>\\1</h2>", lll)
## lll <- gsub("##+[[:blank:]]*=[[:blank:]]*(.*)[[:blank:]]*=[[:blank:]]*",           "<h1>\\1</h1>", lll)
## ## Italics
## lll <- gsub("//([^/]*)//",
##             '<span style="font-style: italic;">\\1</span>', lll)
## ## Bold
## lll <- gsub("\\*\\*([^\\*]*)\\*\\*",
##             '<span style="font-weight: bold;">\\1</span>', lll)
## ## Underline
## lll <- gsub("__([^_]*)__",
##             '<span style="text-decoration: underline;">\\1</span>', lll)
## ## True type (Courier New)
## lll <- gsub("&&([^&]*)&&",
##             '<span style="font-family: Courier New;">\\1</span>', lll)

## ## Vanilla code chunk (<<>>=)
## lll <- gsub("^[[:blank:]]*##?@@$", "<<>>=", lll)

## ## Vanilla code chunk with figure (<<fig=T>>=)
## lll <- gsub("^[[:blank:]]*##?@@fig$", "<<fig=T>>=", lll)

## ## Date
## lll <- gsub("##+[[:blank:]]*%%date",
##             '\\<\\<echo=FALSE\\>\\>=\nSys.time()\n@', lll)
## ## 3 hashes -> a comment
## lll <- gsub("^[[:blank:]]*###+(.*)",
##             "<!-- \\1 !-->", lll)
## ## 1 or 2 hashes -> text
## lll <- gsub("^[[:blank:]]*##?[[:blank:]]*(.*)",
##             "\\1", lll)


