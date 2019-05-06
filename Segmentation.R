path <- 'e:\\publikacje\\headmotionclassification\\r\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))


#góra dó³ lewo prawo, zgodnie z ruchem wskazówek
#filePath <- (paste(path, '1555085815889.log', sep = ''))
#filePath <- (paste(path, '1556055423001T.log', sep = ''))
#filePath <- (paste(path, '1556286701647M.log', sep = ''))
#filePath <- (paste(path, '1556615026168T2.log', sep = ''))
#filePath <- (paste(path, '1556654678369.log', sep = ''))
filePath <- (paste(path, '1556743059141.log', sep = ''))



df.helper <- read.csv(filePath, sep = ";", header = FALSE, stringsAsFactors = FALSE)
df.motion <- read.log.quaternion(filePath)

save.log.quaternion <- function(motion.df, path.to.file, from = 1, to = -1)
{
  #path.to.file <- 'e:\\Publikacje\\headmotionclassification\\test\\aaa.log'
  #motion.df <- df.motion
  fileConn<-file(path.to.file)
  #writeLines(c("Hello","World"), fileConn)
  allLines <- list()
  if (to == -1)
    to = length(motion.df$time)
  if (to > length(motion.df$time))
    to = length(motion.df$time)
  if (length(motion.df$time) == 0)
    return (0)
  if (from > length(motion.df$time))
    return (0)
  if (from > to)
    return (0)
  for (a in from:to)
  {
    line <- paste('q;',motion.df$time[a], ";", sep='')
    #for (b in 1:4)
    line <- paste(line, motion.df$quaternion[a,1], ',',
                  motion.df$quaternion[a,2], ',',
                  motion.df$quaternion[a,3], ',',
                  motion.df$quaternion[a,4], ',',
                  sep='')
    #writeLines(line, fileConn)
    allLines[[2*a - 1]] <- line
    
    line <- paste('v;',motion.df$time[a], ";", sep='')
    
    qq <- as.numeric(motion.df$quaternion[a,])
    
    xx <- getPitchRad(qq)
    yy <- getYawRad(qq)
    zz <- getRollRad(qq)
    #for (b in 1:3)
    line <- paste(line,xx,',',yy,',',zz, sep='')
    #writeLines(line, fileConn)
    allLines[[2*a]] <- line
  }
  writeLines(unlist(allLines), fileConn)
  close(fileConn)
}
#save.log.quaternion(df.motion, 'e:\\Publikacje\\headmotionclassification\\test\\aaa.log')


dist <- list()
for (a in 1:(length(df.motion$quaternion$x) - 1))
{
  dist[[a]] <- angleBetweenQuaternions(as.numeric(df.motion$quaternion[a,]), as.numeric(df.motion$quaternion[a+1,]))
}
dist <- unlist(dist) * 180 / pi
plot(dist, col="black", type='l')


mmed <- function(x,k=11){runmed(x,k)}
filtered <- mmed(dist, k=15)
filtered[filtered < 1] <- 0
filtered[filtered > 1] <- 1
filtered <- mmed(filtered, k=15)
lines(filtered, type="l", col='red')

min.length <- 10
result <- list()

start <- 0
a <- 1
results.list <- list()
b <- 1
while (a < length(filtered) && start == 0)
{
  if (filtered[a] == 1)
    start = a
  a <- a + 1
}
if (start == 0)
{
  
} else
if (start == length(filtered))
{
  results.list[[b]] <- c(start, start)
  b <- b + 1
} else
{
  for (a in (start + 1):length(filtered))
  {
    if (filtered[a] == 0 && start == 0)
    {
      
    }
    else if (filtered[a] == 1 && start == 0)
      start <- a
    else if (filtered[start] == 1 && filtered[a] == 0)
    {
      results.list[[b]] <- c(start, a-1)
      start = 0
      b <- b + 1
    }
  }
}

results.list2 <- list()
c <- 1
for (a in 1:length(results.list))
{
  vec <- results.list[[a]]
  if (vec[2] - vec[1] > min.length)
  {
    results.list2[[c]] <- results.list[[a]]
    print(vec)
    c <- c + 1
  }
}


for (a in 1:length(results.list))
{
  vec <- results.list[[a]]
  lines(x=c(vec[1],vec[1], vec[1], vec[2], vec[2], vec[2]), y=c(0,2,2,2,2,0),col="blue")
}

#color = plot.in.3d(df.motion$coord[,1],
#                   df.motion$coord[,2],
#                   df.motion$coord[,3], color = NULL)


#######################
#plot
for (a in 1:length(results.list))
{
  vec <- results.list[[a]]
  df.motion2 <- rotateRecording(df.motion,
                                as.numeric(df.motion$quaternion[vec[1],]),
                                vec[1], vec[2])
  color = plot.in.3d(df.motion$coord[vec[1]:vec[2],1],
                     df.motion$coord[vec[1]:vec[2],2],
                     df.motion$coord[vec[1]:vec[2],3], color = NULL)
  
  path.help <- paste('e:\\publikacje\\headmotionclassification\\samplesT3\\',a,'.log', sep = '')
  save.log.quaternion(df.motion, path.help, from = vec[1], to = vec[2])
}
