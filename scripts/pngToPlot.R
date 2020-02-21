# libraries ---------------------------------------------------------------


if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(imager, dplyr ,plotly, tidyr, ggplot2)


# create a function to convert an image to a plot -------------------------------


# inputs: file_path - .jpg, .png or other acceptable by imager
#          colours_n - number of color clusters
#         algorithm - algorithm for kmeans fuction. One from the list. 
#         sample_size - number of point for output plot  
# output of the function is ggplot with transformed image

pngToPlot  <- function(file_path, colours_n, algorithm = c("Hartigan-Wong","Lloyd", "Forgy", "MacQueen")
                       ,sample_size =10000) {
  img <- load.image(file_path) #load data as cimg
  
  channel.labels <- c('R','G','B') # define labels for evry channel
  img_DT <- as.data.frame(img) # transform to data frame 
  img_DT_chan <- img_DT %>%  mutate(channel= factor(cc,labels=channel.labels)) #creat new column with new labels for channels
  img_DT_chan_dcast <- reshape2::dcast(img_DT_chan, x+y ~ channel, value.var = "value") #  long-to-wide reshaping tool
  
  
## transform RGB values to hex names of colors -----------------------------

  img_DT_chan_dcast$colour <- apply(img_DT_chan_dcast, 1, function(row){  
    hex <- rgb(row[3], row[4], row[5], maxColorValue=1) # max value for evry chanel is 1 insted of 255
    hex
    })

## using kmeans to reduce the number of colors to the desired amount -------

  clusters <-  kmeans(img_DT_chan_dcast[c('R','G','B')],centers =  colours_n,iter.max = 50,algorithm = algorithm,nstart = 3)
  img_clusters <- data.frame(img_DT_chan_dcast, cluster = as.character (clusters$cluster))
  
  kColours <- rgb(clusters$centers[clusters$cluster,]) #define  the color of each cluster based on its center parameters
  img_clusters <- data.frame(img_clusters, kColour = kColours) # add colour information to data frame
  

## sampling  ---------------------------------------------------------------
  img_clusters_samp <- sample_n(img_clusters,sample_size) 
  
## create a  ggplot theme --------------------------------------------------
  
  plotTheme <- function() {
    theme(
      panel.background = element_rect(
        size = 3,
        colour = "black",
        fill = "gray"),
      axis.ticks = element_line(
        size = 2),
      panel.grid.major = element_line(
        colour = "gray80",
        linetype = "dotted"),
      panel.grid.minor = element_line(
        colour = "gray90",
        linetype = "dashed"),
      axis.title.x = element_text(
        size = rel(1.2),
        face = "bold"),
      axis.title.y = element_text(
        size = rel(1.2),
        face = "bold"),
      plot.title = element_text(
        size = 20,
        face = "bold",
        vjust = 1.5)
      )}

## output ------------------------------------------------------------------
  ggplot(data = img_clusters_samp, aes(x = x, y = -y)) + 
    geom_point(colour = img_clusters_samp$kColour) +
    labs(title = paste0(colours_n, " color clusters \n", sample_size," points")) +
    xlab("x") +
    ylab("y") + 
    plotTheme()
}

# Exampel of use ---------------------------------------------------------

pngToPlot(file_path = "images/norway.png", colours_n = 3, sample_size = 15000,algorithm = "MacQueen")



  
