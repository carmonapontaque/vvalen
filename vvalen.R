vvalen <- function(data,group,test="F",scale=TRUE,alternative="two.sided")
{
  x <- drop(as.matrix(data))
  if (!is.numeric(x))
    stop("input data must be numeric")
  if(!is.factor(group)) 
    stop("input group must be a factor")
  p = dim(data)[2]
  # scale==TRUE to ensure all variables are given equal weight
  if(scale==TRUE) data <- scale(data)
  groupnum <- as.numeric(group)
  clases <- length(table(groupnum))
  if(test=="F") {
    distance <- vvalen1(data, group, 1)
    dd.all <- data.frame(distance,group=rep(1,table(groupnum)[1]))
    for (i in 2:clases) {
      a <- vvalen1(data, group, i)
      dd <- data.frame(distance=a,group=rep(i,table(groupnum)[i]))
      dd.all <- rbind(dd.all, dd)
    }
    if(clases==2)
    {f <- t.test(distance ~ factor(group),var.equal=TRUE,
                  alternative=alternative,data=dd.all)}
    else
    {f <- anova(lm(distance ~ factor(group),data=dd.all))}
    return(f)
  }
  else
  {
    testlist <- vvalen1(data, group, 1)
    testlist <- list(testlist)
    for (i in 2:clases) {
      a <- vvalen1(data, group, i)
      testlist <- c(testlist, list(a))
    }
    distance <- testlist
    z <- kruskal.test(distance)
    return(z)
  }
}

vvalen1 <- function(data, group, classn) 
{
  p <- dim(data)[2]
  groupnum <- as.numeric(group)
  data1 <- data[groupnum == classn, ]
  med1 <- apply(data1, 2, median)
  data2 <- sweep(data1, 2, med1, FUN = "-")
  dij = sqrt(apply(data2^2, 1, sum))
  return(dij)
}

