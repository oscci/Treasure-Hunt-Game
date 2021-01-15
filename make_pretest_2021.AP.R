#This file creates items for pre- and posttest by creating arrangements of pictures using the magick function.

# Used to make a set of 8 items with complex level 4 structure, but easily could be adapted for other item types

#Couldn't seem to get this to work as notebook?, hence basic R script


# Pictures from Multipic
# Link: http://www.bcbl.eu/databases/multipic/
#  Reference: Duñabeitia, J.A., Crepaldi, D., Meyer, A.S., New, B., Pliatsikas, C., Smolka, E., & Brysbaert, M. (2017). MultiPic: A standardized set of 750 drawings with norms for six European languages. Quarterly Journal of Experimental Psychology. doi:10.1080/17470218.2017.1310261

#NB - currently directories for reading pictures and writing items are hard-coded here.
library(groundhog)
groundhog.library(magick,"2020-12-01")

thisset <- 'A' #switch to B to make posttest
picdir <- "~/Dropbox/magic images/colored PNG/"
mydir <-paste0("~/Dropbox/magic images/assembled_pics",thisset,"/")

set.seed(2)



myitems<-read.csv(paste0(picdir,'multipic_items.csv'))
if(thisset=='B'){
myitems<-myitems[sample(nrow(myitems)),] #reorder pictures for 2nd set
}
#NB! picture1 has been overwritten with blank

# For pretest + posttest we will need 24 unique items each
# 
#Make data frame to hold sentences
sentencedf <- data.frame(matrix(NA,nrow=24,ncol=3))
colnames(sentencedf)<-c('Sentence','Correct','Type')
# We now want to build sentences and a set of 3-picture items for each sentence.

# Pre-test and posttest will have 3 items of each type, of following.  

# Note that 1-2, 3-4, 5-6, 7-8 are parallel but differ in the prepositions

sentenceform<- c('The_X_is_above_the_Y_and_next_to_the_Z',
                 'The_X_is_on_top_of_the_Y_and_beside_the_Z',
                 'The_X_is_next_to_the_Y_and_above_the_Z',
                 'The_X_is_beside_the_Y_and_on_top_of_the_Z',
                 'The_X_is_underneath_the_Y_and_next_to_the_Z',
                 'The_X_is_below_the_Y_and_beside_the_Z',
                 'The_X_is_next_to_the_Y_and_underneath_the_Z',
                 'The_X_is_beside_the_Y_and_below_the_Z')
                 
            
# 
# The distractors can just be the alternative arrangements.
# We'll make all sets in order as above, but note that the correct picture choice changes as we go through



#function to assemble pics
assemblepics <- function(itemname,top1,top2,bot1,bot2){
  
  imgtop <- c(top1,top2)
  imgtop <- image_append(image_scale(imgtop, "x100"))
  imgbot<-c(bot1,bot2)
  imgbot <- image_append(image_scale(imgbot, "x100"))
  imgall <-c(imgtop,imgbot)
  imageall<-image_append(image_scale(imgall, "x100"),stack=T)
  imageall<-image_border(image_background(imageall, "white"), "black", "2x2")
  image_write(imageall, path=itemname, format = "png")
}

typelist <- c(1,2,4,1,3,2,4,1,3,3,2,4,4,3,2,1,2,1,3,4,3,1,4,2) #quasirandom order of correct picture

nitem=24

i <- 0 #initialise
for(rep in 1:3){
for (j in 1:8){
 itemtype<-j #this specifies which sentence frame from 1-8
  i<-i+1 #item number for this set
  mynum1<-(3*(i-1)+1) #increment in 3s to select triplet of pictures for this items
  Xword <- myitems$pic[mynum1]
  Yword <- myitems$pic[(mynum1+1)]
  Zword <- myitems$pic[(mynum1+2)]
 
  xx<-round((.45+j/2),0)
  sentencedf$Correct[i] <- LETTERS[xx]
  
  thissent<-sentenceform[itemtype]
  
  mysent1 <- gsub("X", Xword, thissent)
  mysent1 <- gsub("Y",Yword,mysent1)
  mysent1 <- gsub("Z",Zword,mysent1)
  sentencedf$Sentence[i]<-mysent1
  sentencedf$Type[i]<-j
 
imageX<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1)],'.png'))
imageY<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1+1)],'.png'))
imageZ<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1+2)],'.png'))
blank<-image_read(paste0(picdir,'Picture_1.png')) #this is now a blank


imX<-image_append(image_scale(imageX, "100"), stack = TRUE)
imY<-image_append(image_scale(imageY, "100"), stack = TRUE)
imZ<-image_append(image_scale(imageZ, "100"), stack = TRUE)

itemname <- paste0(mydir,'Item_',i,'A.png')

# For correct and distractors we need to create all 4 combination types
#array type A : X and Z side by side, Y below X - corresponds to sentence type 1-2
top1<-imX
top2<-imZ
bot1<-imY
bot2<-blank
assemblepics(itemname,top1,top2,bot1,bot2)

# Array type B: The X is next to the Y and above the Z - sentence type 3-4
itemname <- paste0(mydir,'Item_',i,'B.png')
top1<-imX
top2<-imY
bot1<-imZ
bot2<-blank
assemblepics(itemname,top1,top2,bot1,bot2)

#array type C: X and Z side by side and below Y - sentence type 5-6
itemname <- paste0(mydir,'Item_',i,'C.png')
top1<-imY
top2<-blank
bot1<-imX
bot2<-imZ
assemblepics(itemname,top1,top2,bot1,bot2)


# Array type D: The X is next to the Y and below the Z - sentence type 7-8
itemname <- paste0(mydir,'Item_',i,'D.png')
top2<-imZ
top1<-blank
bot1<-imY
bot2<-imX
assemblepics(itemname,top1,top2,bot1,bot2)

 }
}
write.csv(sentencedf,paste0(mydir,'SentenceList.csv'),row.names=T)


#First randomise the order of sentences in SentenceList

nrow<-nrow(sentencedf)
randorder<-sample(nrow)
sentencedf<-sentencedf[randorder,]

#We now need to randomise the order of the 4 pictures for presentation in Gorilla

#Make a spreadsheet for Gorilla
gvars <- c('randomise_blocks','randomise_trials','display',	'answer','b1','b2','b3','b4',
'Sentence',	'flag_trial','permutation')
gdf <- data.frame(matrix(NA,nrow=(2+nitem*3),ncol=length(gvars)))
colnames(gdf)<-gvars
rowbits<-c('fixation','recall','break')
gdf$display<-c('Introduction',rep(rowbits,nitem),'end')
gdf$flag_trial[2:(nrow(gdf)-1)]<-rep(1:nitem,each=3)
w<-which(gdf$display=='recall')
gdf$permutation[w]<-1:nitem
allsent<-  paste0(gsub(" ", "_", sentencedf$Sentence),'.mp3')
gdf$Sentence[w]<-allsent

rdm.order <- as.numeric(rownames(sentencedf))

startcol<-which(colnames(gdf)=='b1')
for (myn in 1:length(w)){
  worder <- sample(1:4)
gdf[w[myn],(startcol-1+worder[1])] <- paste0("Item_",rdm.order[myn],'A.png')
gdf[w[myn],(startcol-1+worder[2])] <- paste0("Item_",rdm.order[myn],'B.png')
gdf[w[myn],(startcol-1+worder[3])] <- paste0("Item_",rdm.order[myn],'C.png')
gdf[w[myn],(startcol-1+worder[4])] <- paste0("Item_",rdm.order[myn],'D.png')
}
gdf$answer[w]<- paste0("Item_",rdm.order,sentencedf$Correct,'.png')

write.csv(gdf,paste0(mydir,'GorillaSheet',thisset,'.csv'),row.names=F)
