

.w = function(x=NULL){
    msg=.p(
    '\t                                                           \n',
    '\t888       888 8888888b.   .d88888b.  888b    888  .d8888b. \n',
    '\t888   o   888 888   Y88b d88P" "Y88b 8888b   888 d88P  Y88b\n',
    '\t888  d8b  888 888    888 888     888 88888b  888 888    888\n',
    '\t888 d888b 888 888   d88P 888     888 888Y88b 888 888       \n',
    '\t888d88888b888 8888888P"  888     888 888 Y88b888 888  88888\n',
    '\t88888P Y88888 888 T88b   888     888 888  Y88888 888    888\n',
    '\t8888P   Y8888 888  T88b  Y88b. .d88P 888   Y8888 Y88b  d88P\n',
    '\t888P     Y888 888   T88b  "Y88888P"  888    Y888  "Y8888P88\n\n')
    warning(paste0(msg,if(!is.null(x))paste0("\t",x,"\n")),call.=FALSE)
    #if(!is.null(x)){
    #cat("\n---->\t")
    #cat(x)
    #cat("\n\n")
    #}
    }
    
    
.cat = function(i,n.loop,n.dotline=50,msg=NULL){
    # i: iteration number
    # n.loop: number of loop
    # n.dotline: number dot per line
    if(any(seq(0,n.loop,n.dotline)==i)){cat(i,msg,date(),"\n")}else{cat(".")}
    }    
    
    
.adf = function (matrix, colnames = NULL, rownames = NULL){
    dfw = as.data.frame(matrix)
    if (!is.null(colnames)) {
        colnames(dfw) = colnames
    }
    if (!is.null(rownames)) {
        rownames(dfw) = rownames
    }
    dfw
}   
    
.an = function (..., warning = FALSE){
    if (warning) {
        as.numeric(...)
    }
    else {
        suppressWarnings(as.numeric(...))
    }
}

.ac = function(...){as.character(...)}
.expit = function(x){exp(x)/(1+exp(x))}

.alrna = function(integer,names=NULL){
    out=as.list(rep(NA,integer))
    if(!is.null(names)){names(out)=names}
    out
    }
    
.p =  function(...,sep=""){paste(...,sep=sep)}    

 .ep = function (xlim = c(0, 1), ylim = c(0, 1))
{
    plot(1, 1, pch = "", axes = FALSE, xlab = "", ylab = "",
        main = "", ylim = ylim, xlim = xlim)
}

.nf = function(file){
    temp = file
    vect.colw = seq(1, dim(temp)[2])[sapply(temp, class) == "factor"]
    if (length(vect.colw) > 0) {
        for (colw in 1:length(vect.colw)) {
            temp[, vect.colw[colw]] = as.character(temp[, vect.colw[colw]])
        }
    }
    temp
}

.sig = function (pval){
    out = rep("", length(pval))
    out[pval <= 0.1 & !is.na(pval)] = "."
    out[pval <= 0.05 & !is.na(pval)] = "*"
    out[pval <= 0.01 & !is.na(pval)] = "**"
    out[pval <= 0.001 & !is.na(pval)] = "***"
    out
}

 .pval = function (pval, digit = 4){
    out = format(c(.an(.p("0.", .p(rep(1, digit), collapse = ""))),
        round(pval, digits = digit)))[-1]
    out[is.na(pval)] = ""
    out[!is.na(match(out, c("0.00000", "0.0000", "0.000", "0.00",
        "0.0", "0")))] = .p("<0.", .p(rep(0, digit - 1), collapse = ""),
        "1")
    out
}

.idf=function (input, name = NULL)
{
    w1 = ifelse(class(input) == "table", length(unique(names(input))) !=
        length(input), length(unique(input)) != length(input))
    w2 = is.null(name)
    if (w1 | w2) {
        if (w1) {
            .w("duplicted id not allowed")
        }
        if (w2) {
            .w("name can't be empty")
        }
    }
    else {
        n = length(input)
        if (class(input) == "table") {
            id = names(input)
            if (all(!is.na(.an(id)))) {
                id = .ac(id[order(.an(id))])
            }
        }
        else {
            id = input
        }
        id = data.frame(pos = 1:n, id = .ac(id), stringsAsFactors = FALSE,
            row.names = id)
        if (class(input) == "table") {
            id$n = unlist(c(input))
            if (!any(is.na(suppressWarnings(.an(names(input),
                warning = FALSE))))) {
                id$value = .an(names(input), warning = FALSE)
            }
        }
        else {
            if (!any(is.na(suppressWarnings(.an(input, warning = FALSE))))) {
                id$value = .an(input, warning = FALSE)
            }
        }
        assign(.p("n.", name), n, pos = .GlobalEnv)
        assign(.p("id.", name), id, pos = .GlobalEnv)
    }
}