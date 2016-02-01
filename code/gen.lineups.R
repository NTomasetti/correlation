library(ggplot2)
library(mnormt)
library(reshape2)
library(gridExtra)
library(mFilter)
library(nullabor)
library(dplyr)
library(gridSVG)
library(grid)

js <- "http://www.hofroe.net/examples/lineup/fhaction.js"

gen_true_data <- function(n, r, smoothed = FALSE, a) {
  d <- data.frame(rmnorm(n, c(0, 0), matrix(c(1, r, r, 1), 2, 2)))
  d$X1 <- as.vector(arima.sim(list(ar=a), n, innov=d$X1))
  d$X2 <- as.vector(arima.sim(list(ar=a), n, innov=d$X2))
  if (smoothed) {
    d$X1 <- as.vector(hpfilter(d$X1, freq=1, type="lambda", drift = FALSE)[[2]])
    d$X2 <- as.vector(hpfilter(d$X2, freq=1, type="lambda", drift = FALSE)[[2]])
    d <- as.data.frame(d)
  }
  d$X1 <- scale(d$X1)
  d$X2 <- scale(d$X2)
  d$t <- 1:n
  return(d)
}

gen_null <- function(n, m=20, smoothed = FALSE, a){
  nd <- NULL
  for(i in 1:(m-1)) { 
    d <- data.frame(X1=as.vector(arima.sim(list(ar=a), n)),
                    X2=as.vector(arima.sim(list(ar=a), n)))
    if (smoothed) {
      d$X1 <- as.vector(hpfilter(d$X1, freq=1, type="lambda", drift = FALSE)[[2]])
      d$X2 <- as.vector(hpfilter(d$X2, freq=1, type="lambda", drift = FALSE)[[2]])
      d <- as.data.frame(d)
    }
    d$X1 <- scale(d$X1)
    d$X2 <- scale(d$X2)
    nd <- rbind(nd, d)
  } 
  nd$.n <- rep(1:(m-1), each = n) 
  nd$t <- 1:n
  return(nd)
}

nc <- function(null) {
  b <- c(rep(0, 19))
  for (i in 1:19) {
    b[i] <- cor(subset(null, .n==i)[,1], subset(null, .n==i)[,2])
  }
  return(b)
}

interactive_lineup <- function(plotobj, fname, script, width=w, height=h, toggle="toggle", trial=FALSE) {
  print(plotobj)
  grobs <- grid.ls(print=FALSE)
  idx <- grep("panel-", grobs$name)
  for (i in idx) { 
    grid.garnish(grobs$name[i],
                 onmouseover=paste("frame('",grobs$name[i+2], ".1')", sep=""),
                 onmouseout=paste("deframe('",grobs$name[i+2], ".1')", sep=""), 
                 onmousedown=paste(sprintf("%shigh(evt, '", toggle),grobs$name[i+2], ".1')", sep=""))
  }
  
  # use script on server to get locally executable javascript code
  # or use inline option
  grid.script(filename=script)
  grid.export(name=paste0("MTurk/"), ifelse(trial, "trials/", "svgs/"), fname, ".svg"), uniqueNames=FALSE, exportJS="inline", exportCoords="inline", exportMappings="inline")
  dev.off()
}

lineupscatter <- function(name, td, nd, pos, save=TRUE, w=8, h=8) { 
  p <- ggplot(data=lineup(true=td, samples=nd, pos = pos), aes(x=X1, y=X2)) + geom_point() +
    facet_wrap(~ .sample, scales="free") + theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none", aspect.ratio=1)
  if (save) 
    interactive_lineup(p, paste0("s", name), js, w, h)
  else
    print(p)
}

lineupline <- function(name, td, nd, pos, save=TRUE, w=8, h=8) {
  nd_l <- as.data.frame(melt(nd, id=c(".n", "t")))
  td <- data.frame(.n=rep(pos, nrow(td)), td)
  # Make data structure ourself
  if (pos == 1) {
    nd_l$.n <- nd_l$.n + 1
    d <- rbind(td, nd_l)
  }
  else if (pos == max(nd$.n)) {
    nd_l$.n <- nd_l$.n + 1
    d <- rbind(nd_l, td)
  }
  else {
    lg <- nd_l$.n < pos
    nd_l1 <- data.frame(.n=nd_l$.n[lg], t=nd_l$t[lg], variable=nd_l$variable[lg], 
                        value=nd_l$value[lg])
    nd_l2 <- data.frame(.n=nd_l$.n[!lg], t=nd_l$t[!lg], variable=nd_l$variable[!lg], 
                        value=nd_l$value[!lg])
    nd_l2$.n <- nd_l2$.n + 1
    d <- rbind(nd_l1, td, nd_l2)
  }
  
  p <- ggplot(data=d, aes(x=t, y=value, colour=variable)) + geom_line() + facet_wrap(~.n, ncol=4,
                                                                                     scales="free_y")  +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none")
  if (save) 
    interactive_lineup(p, paste0("l", name), js, w, h)
  else
    print(p)
}

#inputs: sample size, number of lineups, repetitions of factor combinations, smoothing, ar(1) coefficient, return correlation matrix
Lineups <- function(n, m=20, c = 3, smoothed = FALSE, a=n/100, cor.matrix=FALSE) {
  count <- c(rep(0, 8))
  csv <- matrix(0, 16*c, 6)
  nullcor = matrix(0, 16*c, 22)
  id <- 1
  for(z in 1:1000){
    real <- gen_true_data(n, r = (-0.9 + 1.8*z/1000), smoothed, a)
    real_long <- melt(real, id="t")
    cor <- cor(real$X1, real$X2)
    pos <- sample(1:20, 1)
    if (cor > 0.885 & cor < 0.915 & count[1] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      count[1] <- count[1] + 1
      id <- id + 2
    } 
    else if(cor > 0.685 & cor < 0.715 & count[2] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[2] <- count[2] + 1
    } 
    else if(cor > 0.485 & cor < 0.515 & count[3] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[3] <- count[3] + 1
    } 
    else if(cor > 0.285 & cor < 0.315 & count[4] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[4] <- count[4] + 1
    }
    else if(cor > -0.315 & cor < -0.285 & count[5] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[5] <- count[5] + 1
    }
    else if(cor > -0.515 & cor < -0.485 & count[6] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[6] <- count[6] + 1
    }
    else if(cor > -0.715 & cor < -0.685 & count[7] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[7] <- count[7] + 1
    }
    else if(cor > -0.915 & cor < -0.885 & count[8] < c) { 
      null <- gen_null(n, m, smoothed, a)
      if(cor.matrix) {
        null.cor <- nc(null)
        nullcor[id,] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
        nullcor[(id+1),] <- c(id, pos, if(pos!=1) null.cor[1:(pos-1)], cor, if(pos!=20) null.cor[pos:19])
      } else {
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupscatter(name, real, null, pos)
        csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos)
        name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
        lineupline(name, real_long, null, pos) 
        csv[(id+1),] <- c(id, cor, paste0("s", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos)
      }
      id <- id + 2
      count[8] <- count[8] + 1
    }
    else if (count[1] == c & count[2] == c & count[3] == c & count[4] == c & count[5] == c & count[6] == c & count[7] == c & count[8] == c){
      break
    }
  }
  csv <- as.data.frame(csv)
  colnames(csv) <- c("data_id", "correlation", "pic_name", "test_param", "smoothed", "true_pos")
  csv$n <- rep(n, (16*c))
  if(nullcor)
    return(nullcor)
  else
    return(csv)
}


##Generate lineups
set.seed(123456)
csv1 <- Lineups(12)
set.seed(123457)
csv2 <- Lineups(24)
set.seed(123458)
csv3 <- Lineups(48)
set.seed(123459)
csv5 <- Lineups(48, smoothed = TRUE)
set.seed(123460)
csv6 <- Lineups(96, a=0.5)
set.seed(123461)
csv4 <- Lineups(96, smoothed = FALSE, a=0.5) 
csv <- rbind(csv1, csv2, csv3, csv4, csv5, csv6) ##Combine individual CSV, fix ID's
csv$pic_id <- 1:288
csv$data_id <- rep(1:144, rep(2, 144))
csv$correlation <- as.numeric(as.character(csv$correlation))
write.csv(csv, "MTurk.csv")

#repeat to get correlation matrix
set.seed(123456)
nc1 <- Lineups(12, cor.matrix=TRUE)
set.seed(123457)
nc2 <- Lineups(24, cor.matrix=TRUE)
set.seed(123458)
nc3 <- Lineups(48, cor.matrix=TRUE)
set.seed(123459)
nc5 <- Lineups(48, smoothed = TRUE, cor.matrix=TRUE)
set.seed(123460)
nc6 <- Lineups(96, a=0.5, cor.matrix=TRUE)
set.seed(123461)
nc4 <- Lineups(96, smoothed = FALSE, a=0.5, cor.matrix=TRUE) 
corcsv <- rbind(nc1, nc2, nc3, nc4, nc5, nc6) ##Combine individual CSV, fix ID's
corcsv[,1] <- 1:288
colnames(corcsv) <- c("pic_id", "pos", "plot 1", 2:20)
write.csv(corcsv, "correlation.csv")


##Generates high correlation trial lineups to use as examples
Lineups.trial <- function(n, dep=TRUE, m=20, c = 10, smoothed = FALSE) {
  count <- 0
  csv <- matrix(0, 2*c, 6)
  id <- 1
  for(z in 1:1000){
    real <- gen_true_data(n, r = (-0.9 + 1.8*z/1000), dep, smoothed)
    real_long <- melt(real, id="t")
    cor <- cor(real$X1, real$X2)
    pos1 <- sample(1:20, 1)
    pos2 <- sample(1:20, 1)
    if (cor > 0.885 & cor < 0.915 & count[1] < c) { 
      null <- gen_null(n, m, smoothed)
      null.cor <- nc(null)
      name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
      lineupscatter(name, real, null, pos1)
      csv[id,] <- c(id, cor, paste0("s", name, ".svg"), "scatter", ifelse(smoothed, "yes", "no"), pos1)
      name <- paste(sample(c(0:9, letters, LETTERS), 12, replace=TRUE),collapse="")
      lineupline(name, real_long, null, pos2) 
      csv[(id+1),] <- c(id, cor, paste0("l", name, ".svg"), "line", ifelse(smoothed, "yes", "no"), pos2)
      id <- id + 2
      count <- count + 1
    } 
    else if (count == c){
      break
    }
  }
  csv <- as.data.frame(csv)
  colnames(csv) <- c("data_id", "correlation", "pic_name", "test_param", "smoothed", "true_pos")
  csv <- cbind(pic_id=1:(2*c), csv, sample_size=rep(5, (2*c)), n=rep(n, (2*c)))
  csv$data_id <- rep(1:(1*c), rep(2, (1*c)))
  return(csv)
}
set.seed(1234567)
csvtrial <- Lineups.trial(24, c = 5, smoothed = FALSE)
write.csv(csvtrial, "MTurk/Trial.csv")

