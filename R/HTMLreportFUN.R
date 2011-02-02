RweaveHTMLreportSetup <- function(srclines){
  anslines <- srclines
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]
    ## blank lines -> <br>
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

    ## Vanilla code chunk (<<>>=)
    lll <- gsub("^[[:blank:]]*##?@@$", "<<>>=", lll)

    ## Vanilla code chunk with figure (<<fig=T>>=)
    lll <- gsub("^[[:blank:]]*##?@@fig$", "<<fig=T>>=", lll)
      
    ## Date
    lll <- gsub("##+[[:blank:]]*%%date",
                '\\<\\<echo=FALSE\\>\\>=\nSys.time()\n@', lll)
    ## 3 hashes -> a comment
    lll <- gsub("^[[:blank:]]*###+(.*)",
                "<!-- \\1 !-->", lll)
    ## 1 or 2 hashes -> text
    lll <- gsub("^[[:blank:]]*##?[[:blank:]]*(.*)",
                "\\1", lll)
    anslines[ii] <- lll
  }
  anslines
}

RweaveHTMLreportWritedoc <- function(srclines, filename.postfix, tmpfile.name){
  anslines <- srclines
  for (ii in 1:length(srclines)){
    lll <- srclines[ii]
    lll <- gsub("<img height= width= ",          "<img ", lll)
    lll <- gsub("<p align= center >",            "<p align= left >", lll)
    lll <- gsub("<p align='center'>",            "<p align= left >", lll) 
    lll <- gsub("(<!--\\\\end\\{Schunk\\}!-->)", "</p> \\1",lll)

    lll <- gsub(tmpfile.name, filename.postfix, lll)    
    anslines[ii] <- lll
  }
  anslines
}


##<p><xmp class=command>> plot(rate ~ conc, data = Puromycin, col = as.numeric(state))</xmp></p>

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


RweaveHTMLreport <- function(){
  list(setup    = RweaveHTMLreportSetup,
       writedoc = RweaveHTMLreportWritedoc,
       finish   = RweaveHTMLreportFinish)
}

HTMLreport <- function(srcfile, driver=RweaveHTMLreport(), destdir=".", postfix="REPORT", cssfile=NULL, ...){
  
  if(!file.exists(srcfile)){
    stop(sprintf("file %s does not exist\n", srcfile))
  }
  
  if (.Platform$OS.type == "windows"){ 
    srcfile <- gsub("\\\\", "/", srcfile)
    destdir <- gsub("\\\\", "/", destdir)
  }
  ## File has format: path/filename.extension
  ##
  path.filename  <- gsub("(.*)\\..*" ,"\\1", srcfile)            ## path/filename
  filename       <- gsub(".*/(.*)$","\\1",   path.filename)      ## filename
  destdir.outfile    <- paste(destdir, "/", filename, "-",postfix, ".html",sep="") ## The final report file
  destdir.filename   <- paste(destdir, "/", filename, "-",postfix, sep="")
  filename.postfix   <- paste(filename,"-",postfix, sep="")
  
  ## We need two temporary files
  ##
  tmpfile.name <- paste(filename,tempfile(tmpdir=""),sep="-")
  if (.Platform$OS.type == "windows") 
    tmpfile.name <- gsub("\\\\", "/", tmpfile.name)
  tmpfile.name <- gsub("/","", tmpfile.name)  
  tmpfile1  <- tmpfile.name
  tmpfile2  <- paste(tmpfile.name,".html",sep="")
  cat(sprintf("tmpfile.name: %s \nfilename %s\ndestdir.filename: %s\n", tmpfile.name, filename, destdir.filename))
  
  cat("Preprocessing... \n")
  ff <- file.copy(srcfile, tmpfile1, overwrite=TRUE)   ## Create copy of the source file
  if (!ff){
    stop("Can not create temporary file in working directory. Is the directory write protected?")
  }
  
  inlines   <- readLines(tmpfile1)
  outlines  <- driver$setup(inlines)                    ## Preprocess source file
  write(outlines, file=tmpfile1)
  cat(sprintf(" source file      : %s\n",  srcfile))
  cat(sprintf(" filename         : %s\n",  filename))
  cat(sprintf(" temp source file : %s\n",  tmpfile1))
  
  Sweave(tmpfile1, driver=RweaveHTML())
  
  cat("Postprocessing... \n")
  inlines    <- readLines(tmpfile2)
  outlines   <- driver$writedoc(inlines, filename.postfix, tmpfile.name) ## Postprocess html file

  if (!is.null(cssfile)){
    #cssline <- "<link rel=stylesheet href=\"R2HTML.css\" type=text/css>"
    cssline <- paste("<link rel=stylesheet href=\"",cssfile,"\" type=text/css>",sep="")
    outlines <- c(cssline, outlines)
  }
  
  write(outlines, file=destdir.outfile)
  cat(sprintf(" target file    : %s\n", destdir.outfile))

  driver$finish(tmpfile.name, filename.postfix, destdir.filename)
  
##   file.remove(tmpfile1)
##   file.remove(tmpfile2)

  return(invisible(NULL))
}
