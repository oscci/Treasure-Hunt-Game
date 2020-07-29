#This file creates items for pre- and posttest by creating arrangements of pictures using the magick function.

# Used to make a set of 8 items with complex level 4 structure, but easily could be adapted for other item types

(#Couldn't seem to get this to work as notebook?, hence basic R script)


# Pictures from Multipic
# Link: http://www.bcbl.eu/databases/multipic/
#  Reference: Duñabeitia, J.A., Crepaldi, D., Meyer, A.S., New, B., Pliatsikas, C., Smolka, E., & Brysbaert, M. (2017). MultiPic: A standardized set of 750 drawings with norms for six European languages. Quarterly Journal of Experimental Psychology. doi:10.1080/17470218.2017.1310261

#NB - currently directories for reading pictures and writing items are hard-coded here.

require(magick)
picdir <- "/Users/dorothybishop/deevybee_repo/magic images/colored PNG/"
mydir <- "/Users/dorothybishop/deevybee_repo/magic images/assembled_pics/"

myitems<-read.csv(paste0(picdir,'multipic_items.csv'))

NB! picture1 has been overwritten with blank

# 
#Make data frame to hold sentences
sentencedf <- data.frame(matrix(NA,nrow=8,ncol=2))
colnames(sentencedf)<-c('Sentence','Correct')
# We now want to build sentences and a set of 3-picture items for each sentence.
# We will have 2 sentences of each of these types
# 
# The X is above the Y and next to the Z
# The X is below the Y and next to the Z
# The X is next to the Y and above the Z
# The X is next to the Y and below the Z
# 
# The distractors can just the alternative arrangements.
# We'll make all sets in order as above, but note that the correct item changes as we go through

# As the numbers are not large, we'll do this for each item type
# The X is above the Y and next to the Z


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

nitem=8
for (i in 1:nitem){
  mynum1<-(3*(i-1)+1)
  Xword <- myitems$pic[mynum1]
  Yword <- myitems$pic[(mynum1+1)]
  Zword <- myitems$pic[(mynum1+2)]
  typelist <- c(1,2,3,4,3,1,4,2) #quasirandom order of items
  sentencedf$Correct <- LETTERS[typelist]
  senttypes <- c('The X is above the Y and next to the Z',
    'The X is below the Y and next to the Z',
    'The X is next to the Y and above the Z',
    'The X is next to the Y and below the Z')
  thissent<-senttypes[typelist[i]]
  mysent1 <- gsub("X", Xword, thissent)
  mysent1 <- gsub("Y",Yword,mysent1)
  mysent1 <- gsub("Z",Zword,mysent1)
  sentencedf$Sentence[i]<-mysent1
 

picdir <- "/Users/dorothybishop/deevybee_repo/magic images/colored PNG/"
imageX<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1)],'.png'))
imageY<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1+1)],'.png'))
imageZ<-image_read(paste0(picdir,'Picture_',myitems$num[(mynum1+2)],'.png'))
blank<-image_read(paste0(picdir,'Picture_1.png')) #this is now a blank


imX<-image_append(image_scale(imageX, "100"), stack = TRUE)
imY<-image_append(image_scale(imageY, "100"), stack = TRUE)
imZ<-image_append(image_scale(imageZ, "100"), stack = TRUE)



itemname <- paste0(mydir,'Item_',i,'A.png')
#array type A : X and Y side by side, then Z below X
top1<-imX
top2<-imZ
bot1<-imY
bot2<-blank
assemblepics(itemname,top1,top2,bot1,bot2)

#array type B: X and Z side by side and below Y
itemname <- paste0(mydir,'Item_',i,'B.png')
top1<-imY
top2<-blank
bot1<-imX
bot2<-imZ
assemblepics(itemname,top1,top2,bot1,bot2)


# Array type C: The X is next to the Y and above the Z
itemname <- paste0(mydir,'Item_',i,'C.png')
top1<-imX
top2<-imY
bot1<-imZ
bot2<-blank
assemblepics(itemname,top1,top2,bot1,bot2)

# Array type C: The X is next to the Y and above the Z
itemname <- paste0(mydir,'Item_',i,'C.png')
top1<-imX
top2<-imY
bot1<-imZ
bot2<-blank
assemblepics(itemname,top1,top2,bot1,bot2)

# Array type D: The X is next to the Y and below the Z
itemname <- paste0(mydir,'Item_',i,'D.png')
top2<-imZ
top1<-blank
bot1<-imY
bot2<-imX
assemblepics(itemname,top1,top2,bot1,bot2)
}

write.csv(sentencedf,paste0(mydir,'SentenceList.csv'),row.names=T)

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
gdf$b1[w] <- paste0("Item_",1:nitem,'A.png')
gdf$b2[w] <- paste0("Item_",1:nitem,'B.png')
gdf$b3[w] <- paste0("Item_",1:nitem,'C.png')
gdf$b4[w] <- paste0("Item_",1:nitem,'D.png')
gdf$answer[w]<- paste0("Item_",1:nitem,sentencedf$Correct,'.png')

write.csv(gdf,paste0(mydir,'GorillaSheet.csv'),row.names=F)



