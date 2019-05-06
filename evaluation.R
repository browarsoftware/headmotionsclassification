path <- 'e:\\publikacje\\headmotionclassification\\r\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))
source(paste(path, 'averaging.R', sep = ''))

set.seed(1)

read.data.from.folder <- function(dir.path)
{
  rd <- list()
  files <- list.files(dir.path)
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.path, "\\", files[a], sep=''))
    
    #df.motion <- rotateRecording(df.motion,
    #                              as.numeric(df.motion$quaternion[1,]),
    #                              1, nrow(df.motion$quaternion), FALSE)
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    rd[[a]] <- df.motion
  }
  return(rd)
}



dir.path <- "e:\\publikacje\\headmotionclassification\\samplesM\\"
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "eight", "right")
p1 <- list()
for (a in 1:length(motions))
{
  dir.path.help <- paste(dir.path, motions[a], sep = "")
  p1[[a]] <- read.data.from.folder(dir.path.help)
  #a <- 6
  #for (b in 1:length(p2[[a]]))
  #  plot.in.3d(p2[[a]][[b]]$coord$x,
  #             p2[[a]][[b]]$coord$y,
  #             p2[[a]][[b]]$coord$z,
  #             color = NULL)
}

dir.path <- "e:\\publikacje\\headmotionclassification\\samplesT\\"
#motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "eight", "right")
motions.names <- c("Clockwise", "Counterclockwise",  "Left", "Nod", "Omega left", "Omega right", "Eight", "Right")
p2 <- list()
for (a in 1:length(motions))
{
  dir.path.help <- paste(dir.path, motions[a], sep = "")
  p2[[a]] <- read.data.from.folder(dir.path.help)
  #a <- 5
  #for (b in 1:length(p2[[a]]))
  #  plot.in.3d(p2[[a]][[b]]$coord$x,
  #             p2[[a]][[b]]$coord$y,
  #             p2[[a]][[b]]$coord$z,
  #             color = NULL)
}


template.id <- 2
template.signal <- p2
signal.signal <- p1

template <- list()
#if (FALSE)
{
  for (a in 1:length(template.signal))
  {
    averaged.sig <- my_dbaCmp(template.signal[[a]], eps = 0.001)
    save.log.quaternion(averaged.sig, paste('e:\\publikacje\\headmotionclassification\\r\\',template.id,'_',motions[a],".csv",sep=''))
    #a <- 5
    qqq <- read.log.quaternion(paste('e:\\publikacje\\headmotionclassification\\r\\',template.id,'_',motions[a],".csv",sep=''))
    
    #qqq <- rotateRecording(qqq,
    #                             as.numeric(qqq$quaternion[1,]),
    #                             1, nrow(qqq$quaternion), FALSE)
    
    template[[a]] <- qqq

    if (TRUE)
    {
      colors.helper <- c()
      x.helper <- c()
      y.helper <- c()
      z.helper <- c()
      pch.helper <- c()
      for (b in 1:length(template.signal[[a]]))
      {

        if (b == 1)
        {
          colors.helper = c(colors.helper,plot.in.3d(template.signal[[a]][[b]]$coord$x,
                   template.signal[[a]][[b]]$coord$y,
                   template.signal[[a]][[b]]$coord$z, color = NULL))
          x.helper <- c(x.helper,template.signal[[a]][[b]]$coord$x)
          y.helper <- c(y.helper,template.signal[[a]][[b]]$coord$y)
          z.helper <- c(z.helper,template.signal[[a]][[b]]$coord$z)
        }
        else
        {
          colors.helper = c(colors.helper, plot.in.3d(template.signal[[a]][[b]]$coord$x,
                     template.signal[[a]][[b]]$coord$y,
                     template.signal[[a]][[b]]$coord$z, color = NULL, new.plot = FALSE))
          x.helper <- c(x.helper,template.signal[[a]][[b]]$coord$x)
          y.helper <- c(y.helper,template.signal[[a]][[b]]$coord$y)
          z.helper <- c(z.helper,template.signal[[a]][[b]]$coord$z)
        }
      }
      
      pch.helper <- c(pch.helper, rep(1, length(x.helper)))
      
      colors.helper = c(colors.helper, plot.in.3d(qqq$coord$x,
                 qqq$coord$y,
                 qqq$coord$z, color.base = 'b', new.plot = FALSE))
      x.helper <- c(x.helper, qqq$coord$x)
      y.helper <- c(y.helper, qqq$coord$y)
      z.helper <- c(z.helper, qqq$coord$z)
      pch.helper <- c(pch.helper, rep(19, length(qqq$coord$x)))
      
      library(scatterplot3d)

      scatterplot3d(x.helper, z.helper, y.helper, color = colors.helper,
                    pch=pch.helper,
                    xlab="X",
                    ylab="Z",
                    zlab="Y", #type='l'
                    xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
                    main = motions.names[a], angle = 160, scale = 1
                    )
      
      if (FALSE)
      {
        for (b in 1:length(signal.signal[[a]]))
        {
          if (b == 1)
            plot.in.3d(signal.signal[[a]][[b]]$coord$x,
                       signal.signal[[a]][[b]]$coord$y,
                       signal.signal[[a]][[b]]$coord$z, color = NULL)
          else
            plot.in.3d(signal.signal[[a]][[b]]$coord$x,
                       signal.signal[[a]][[b]]$coord$y,
                       signal.signal[[a]][[b]]$coord$z, color = NULL, new.plot = FALSE)
        }
      }
    }
  }
}

template <- list()
for (a in 1:length(motions))
{
  template[[a]] <- read.log.quaternion(paste('e:\\publikacje\\headmotionclassification\\r\\',template.id,'_',motions[a],".csv",sep=''))
}


#signals <- list()
#for (a in 1:length(p2))
#{
#  template[[a]] <- p1[[a]][[3]]
#  #signals[[a]] <- p1[[a]][-1]
#}

signals <- signal.signal

results <- c()
correct <- c()

templates.dtw <- list()
for (a in 1:length(template))
{
  #templates.dtw[[a]] <- to.dtw.list(template[[a]]$euler.angles)
  templates.dtw[[a]] <- to.dtw.list(template[[a]]$coord)
}

results <- c()
correct <- c()
for (a in 1:length(signals))
  for (b in 1:length(signals[[a]]))
  {
    #my.signal.dtw <- to.dtw.list(signals[[a]][[b]]$euler.angles)
    my.signal.dtw <- to.dtw.list(signals[[a]][[b]]$coord)
    
    
    
    #templates.dtw.trans <- list()
    #for (c in 1:length(templates.dtw))
    #{
    #  templates.dtw.trans[[c]] <- transform.column(templates.dtw[[c]], my.signal.dtw)
    #}
    results <- c(results, DTWClassifier(my.signal.dtw, templates.dtw, euc.distCmp)$class.id)
    #results <- c(results, DTWClassifier(my.signal.dtw, templates.dtw.trans, euc.dist2DCmp)$class.id)
    correct <- c(correct, a)
    print(a)
    print(results)
    print(correct)
  }
cm <- table(motions[correct], motions[results])

cm2 <- cm
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
cbind(cm2, err)

cmxx <- cm
cmvv <- cmxx + cm


#additional plots showing erros
template.id <- 1
template.signal <- p1
signal.signal <- p2

id.signal.type <- 1
id.signal.examplar <- 1
id.template1 <- 1
id.template2 <- 7
colors.helper <- c()
x.helper <- c()
y.helper <- c()
z.helper <- c()

colors.helper = c(colors.helper, plot.in.3d(signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$x,
                                            signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$y,
                                            signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$z, color = NULL, new.plot = TRUE))

x.helper <- c(x.helper, signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$x)
y.helper <- c(y.helper, signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$y)
z.helper <- c(z.helper, signal.signal[[id.signal.type]][[id.signal.examplar]]$coord$z)

colors.helper = c(colors.helper, plot.in.3d(template[[id.template1]]$coord$x,
                                            template[[id.template1]]$coord$y,
                                            template[[id.template1]]$coord$z, color = NULL, new.plot = FALSE, color.base = 'b'))

x.helper <- c(x.helper, template[[id.template1]]$coord$x)
y.helper <- c(y.helper, template[[id.template1]]$coord$y)
z.helper <- c(z.helper, template[[id.template1]]$coord$z)

colors.helper = c(colors.helper, plot.in.3d(template[[id.template2]]$coord$x,
                                            template[[id.template2]]$coord$y,
                                            template[[id.template2]]$coord$z, color = NULL, new.plot = FALSE, color.base = 'g'))

x.helper <- c(x.helper, template[[id.template2]]$coord$x)
y.helper <- c(y.helper, template[[id.template2]]$coord$y)
z.helper <- c(z.helper, template[[id.template2]]$coord$z)

library(scatterplot3d)

scatterplot3d(x.helper, z.helper, y.helper, color = colors.helper,
              xlab="X",
              ylab="Z",
              zlab="Y", #type='l'
              xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
              angle = 40, scale = 1)
