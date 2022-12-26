if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, rlang, factoextra, Rtsne, ggplot2, imager)

jouyou <- scan("Jouyou.txt", what="character")

### Kuzushiji #####

kanji_df<-data.frame(matrix(ncol = 4097, nrow = 0))

dirs<-list.dirs(path='kkanji2', recursive=FALSE)

labels<-list.files(path='kkanji2')
labels <- gsub(" ", "", paste("<",labels,">"))
labels <- chr_unserialise_unicode(labels)

for (i in seq_along(dirs)){
  if(labels[i]%in%jouyou){
    images_in_folder<-list.files(path=dirs[i])
    for (j in seq_along(images_in_folder)){
      im <- readPNG(paste(dirs[i], images_in_folder[j],sep='/'))
      im<-as.vector(im)
      kanji_df<-rbind(kanji_df, append(labels[i], im))
      if (j==4){
        break
      }
    }
  }
}

### Characters as images ####

### Saving characters as image ####

char_img <- function(kanji){
  par(mar=rep(0,4), oma=rep(0,4))
  png(file=gsub(" ", "", paste("jkanji/", kanji, ".png")))
  par(mar=rep(0,4))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(kanji), cex = 40, col = "black", family="serif", font=2)
  dev.off()
}

for (kanji in jouyou[c(1:100)]){
  char_img(kanji)
}

jouyou_in_folder<-list.files(path="jkanji")
for (kanji in jouyou_in_folder) {
  im <- load.image(gsub(" ", "", paste("jkanji/", kanji)))
  im <- resize(im, 64, 64)
  save.image(im, file=gsub(" ", "", paste("jkanji/", kanji)))
}

### Readin images ####

kanji_df<-data.frame(matrix(ncol = 4097, nrow = 0))

jouyou_in_folder<-list.files(path="jkanji")

for (i in seq_along(jouyou_in_folder)){
  im <- readPNG(paste("jkanji", jouyou_in_folder[i], sep='/'))
  im <- matrix(im[,,1], 64, 64)
  im <- matrix(t(im)[,ncol(im):1], 64, 64)
  im <- as.vector(im)
  kanji_df<-rbind(kanji_df, append(substring(jouyou_in_folder[i],1,1), im))
}

### PCA ####
kanji_df[, c(2:4097)] <- lapply(kanji_df[, c(2:4097)], as.numeric)

kanji_pca <- prcomp(kanji_df[2:4097])

fviz_eig(kanji_pca)

# Cumulative variance
cumsum(kanji_pca$sdev^2 / sum(kanji_pca$sdev^2))

pca_df <- data.frame(kanji_pca[['x']][,c(1:2)])
pca_df$label <- kanji_df[, 1]

ggplot(pca_df) +
geom_text(aes(x=pca_df[,1], y=pca_df[,2], label=label, color=label), show.legend = FALSE)

### Plotting image ####
retrieved_df <- t(t(kanji_pca$x[,c(1:2)] %*% t(kanji_pca$rotation[,c(1:2)])) + kanji_pca$center)

retrieved_df <- cbind(pca_df$label, retrieved_df)

test <- matrix(as.numeric(retrieved_df[100, c(2:4097)]), nrow=64, ncol=64)

image(test,axes=FALSE,useRaster=TRUE,col=gray.colors("10"))

### T-SNE ####
kanji_tsne <- Rtsne(kanji_df, dims=2, initial_dims=60, max_iter=1000, pca=FALSE,
                    perplexity=3)

tsne_df <- data.frame(kanji_tsne[['Y']])
tsne_df$label <- kanji_df[, 1]

ggplot(tsne_df) +
  geom_text(aes(x=X1, y=X2, label=label, color=label), show.legend = FALSE)

  # for zooming in
  # coord_cartesian(xlim=c(10,11), ylim=c(0,3))