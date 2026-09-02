title: Problem Set 4
author: Mei Iwamoto
date: 9/18/2024
---
  

#read warblers csv file into r
warblers <- read.csv('C:/Users/18086/OneDrive/Documents/school/R/warblers.csv')
print(warblers)

#Question 1
#for the numerator, we begin by multiplying the values in the first column (the first resource class) of two different species, and go on with the next two values in the next column for the same two species. You will do this for all the columns for the two species, and eventually you will add them all together. For the denominator, you would square the value in the first column for the first species, and go on down the rest of the columns and add them all together. You would then do the same for the second species, and multiply the two sums you get. Finally, you would take the square root of whatever you get. 

#Question 2
#The overlap should be smaller than expected by chance because that would show that they are partitioning the resources to reduce competition. 

#first make function for x and y, don't directly put in warblers rn. flexible function
adult <- function(x,y){
  (sum(x*y))/(((sum(x^2))*(sum(y^2)))^0.5)
}

#maybe save each species as a vector first?
Cm <- vector()
Cm <- as.numeric(warblers[1,2:17])
print(Cm)
Mw <- vector()
Mw <- as.numeric(warblers[2,2:17])
po1 <- adult(Cm, Mw)
#double check answer - is it 0.34?
print(po1)

#Question 3 - for all possible pairs - use combn?
combn(warblers[1:5, 1], 2)
#set all species as vector
Bgw <- vector()
Bgw <- as.numeric(warblers[3, 2:17])
Black <- as.numeric(warblers[4, 2:17])
Bay <- as.numeric(warblers[5, 2:17])

po2 <- adult(Cm, Bgw)
po3 <- adult(Cm, Black)
po4 <- adult(Cm, Bay)
po5 <- adult(Mw, Bgw)
po6 <- adult(Mw, Black)
po7 <- adult(Mw, Bay)
po8 <- adult(Bgw, Black)
po9 <- adult(Bgw, Bay)
po10 <- adult(Black, Bay)


#combn(5,2) - for five species of birds, ten different possible pairs, reshuffling for each pair of birds? what are all the options if you have five things and taking it in pairs. 
# comparing warblers[1, 2:17] vs as.numeric(warblers[1, 2:17]) - as numeric removes column names 

#Question 4, new vector for Cape May Warbler and Myrtle Warbler
Cm1 <- vector()
Cm1 <- sample(Cm, 16)
print(Cm1)

Mw1 <- vector()
Mw1 <- sample(Mw, 16)
print(Mw1)

#example random overlap function run 
adult(Cm1, Mw1)

#Question 5

#use replicate to sample each species and reshuffle values 10000, then put into for loop with the adult function to calculate the overlap value 10,000 times. 
Cmr <- replicate(10000, sample(Cm, 16))
Mwr <- replicate(10000, sample(Mw, 16))
for (t in 1:10000) {overlap[t] <- adult(Cmr[,t], Mwr[,t])
  
}
print(overlap)

#question 6
hist(overlap)
