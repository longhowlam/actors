############################################################################
####
#### process videoland boxarts (coversimages) through pretrained resnet and vvg19

############################################################################
####
#### process rtl nieuws images through pretrained resnet and vvg19

library(keras)
library(tidyr)
library(dplyr)
library(text2vec)
library(stringr)



###################################################################################################
## import pretrained model without the fully connected top layers

vgg16_notop = application_vgg16(weights = 'imagenet', include_top = FALSE)



###################################################################################

### iterate trough actor images for feature extraction

actors = list.files("images")

for(i in 1:length(actors))
{
  print(i)
  img = image_load(
    paste0("images/", actors[i]),
    target_size = c(224,224)
  )
  x = image_to_array(img)
  
  dim(x) <- c(1, dim(x))
  x = imagenet_preprocess_input(x)
  
  ## extract features
  features = vgg16_notop %>% predict(x)
  
  f1 = as.numeric(features)
  if(i==1){
    M1 <- as(matrix(f1, ncol = length(f1)), "dgCMatrix")
  }
  else{
    M1 = rbind(M1, f1)
  }
}






afstanden = text2vec::dist2(M1,M1)
dim(afstanden)


afstanden[2,]
colnames(afstanden) = 1:dim(VLND)[1]

topnNames = function(x){
  names(sort(x)[1:10])
}
topnScores = function(x){
  sort(x)[1:10]
}

BATCH_afstanden = t(apply(afstanden, 1, topnScores )) %>% 
  data.frame(
    stringsAsFactors = FALSE
  )


BATCH_afstanden$titel = row.names(BATCH_afstanden)
ZZ = tidyr::gather(BATCH_afstanden, rank1, score, -titel) %>%
  mutate(rank1 = str_replace(rank1,"X","") %>% as.numeric) 

BATCH_afstanden_names = t(apply(afstanden, 1, topnNames )) %>% 
  data.frame(
    stringsAsFactors = FALSE
  )

BATCH_afstanden_names$titel = row.names(BATCH_afstanden_names)
YY = tidyr::gather(BATCH_afstanden_names, rank2, film, -titel) %>%
  mutate(rank2 = str_replace(rank2,"X","") %>% as.numeric) 


XX = bind_cols(ZZ,YY %>% select(film))
XX = XX %>% arrange(titel,rank1)

images = XX %>% 
  left_join(VLND %>% select(id, boxart), by = c("titel"="id")) %>%
  left_join(VLND %>% select(id, boxart), by = c("film"="id"))

saveRDS(images, "VideolandImageSim.RDs")



