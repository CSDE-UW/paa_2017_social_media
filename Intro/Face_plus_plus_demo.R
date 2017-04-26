### PAA 2017
### Workshop Social Media for Demographic Research  
### Emilio Zagheni
### Intro to API
### Hands-on activity with the Face++ API

## If this is the first time you sue the R packages rjson and RCurl
## you would have to install them:
install.packages("rjson")
install.packages("RCurl")

## load the packages "rjson" and "RCurl"
library(rjson)
library(RCurl)


###################################
#### 1. PUT TOGETHER THE INPUTS ####
###################################

## consider the URL for Ron's image and save it a string:
pic_Ron<- "http://www.demog.berkeley.edu/images/ron_lee2.jpg"


## Register your Face++ APP
## go to http://www.faceplusplus.com/create-a-new-app/
## and follow the directions to create an APP
## copy your API Key and Api Secret and paste them below.

my_api_key<-	"7d5e35c7081a1dde5aa9510c804eeecf"
my_api_secret<-	"T9IwAEIERr4RjgUWpDXEv5w8VXKsDvKC"

###########################
## 2. PREPARE A REQUEST ####
###########################

## Make sure you understand the various components following URL
## then copy it and paste it in your web browser 
url_for_request<- paste("http://apius.faceplusplus.com/v2/detection/detect?api_key=",my_api_key,"&api_secret=",my_api_secret,"&url=",pic_Ron,"&attribute=age%2Cgender%2Crace%2Csmiling%2Cpose%2Cglass",sep="")
url_for_request
## what did you observe?


##################################
## 3. GET THE OUTPUT FROM THE REQUEST
##################################

plain_json_output<- getURL(url_for_request)
plain_json_output

#writeLines(plain_json_output, "Ron_output.json")
#plain_json_output<-readLines("Ron_output.json")

## convert the json output into a list
output_list <- fromJSON(plain_json_output)
output_list

## extract attributes that interest you about Ron (or anybody else)
##estimate for age
output_list$face[[1]]$attribute$age$value
##range for age
output_list$face[[1]]$attribute$age$range
##value for race 
output_list$face[[1]]$attribute$race$value 
##value for gender 
output_list$face[[1]]$attribute$gender$value


####################################
## You have gone though the basic steps
## Once you are comfortable with those steps, you can automate the process

## First of all we can write a function that takes the url of the picture
## as input and returns the content of the json file as output

figure_details<- function(pic_url){
  url_for_request<- paste("http://apius.faceplusplus.com/v2/detection/detect?api_key=",my_api_key,"&api_secret=",my_api_secret,"&url=",pic_url,"&attribute=age%2Cgender%2Crace%2Csmiling%2Cpose%2Cglass",sep="")
  
  return(fromJSON(getURL(url_for_request)))
}


## We can now apply the function to the URL for Ron's image
## in order to obtain the json file with the estimated attributes
Ron_estimate<- try(figure_details(pic_Ron),silent=TRUE)

Ron_estimate

Ron_estimate$face[[1]]$attribute$age$value

## Note the we uses the "try" function. What does it do? Why is it important?
## some information about "try" can be found by typing
## help(try)

pic_bil <- "http://www.population-europe.eu/sites/default/files/styles/bxslider_node_detailpage/public/media-images/pe_event_2012_brussels_3.jpg?itok=Qy-6FeVA"

Billari_estimate<- try(figure_details(pic_bil),silent=TRUE)

Billari_estimate
Billari_estimate$face[[1]]$attribute$age$value
Billari_estimate$face[[1]]$attribute$age$range
Billari_estimate$face[[1]]$attribute$race$value 
Billari_estimate$face[[1]]$attribute$gender$value


## Now imagine that you had a list of URLs, for example for images  of faculty in Demography at UC Berkeley: 
#http://www.demog.berkeley.edu/faculty/dfcm.shtml

pics_Berkeley<- c("http://www.demog.berkeley.edu/images/Irene_Bloemraad_Sociology.jpg","http://www.demog.berkeley.edu/images/wdow.jpg","http://www.demog.berkeley.edu/images/feehan_144.jpeg","http://www.demog.berkeley.edu/images/joshgoldstein.jpg","http://www.demog.berkeley.edu/images/geneb.jpg","http://www.demog.berkeley.edu/images/baseball350_UCBarticle_144.jpg","http://www.demog.berkeley.edu/images/lucas.jpg")
 
############### 
### TO DO ###### 
## Write a loop that calls the function at each iteration to store an estimate of age of each person in a new vector.



########

age_Berkeley_folks<- rep(NA,length(pics_Berkeley))
for (ii in 1:length(pics_Berkeley)){
  
  temp_output <- figure_details(pics_Berkeley[ii])
  age_Berkeley_folks[ii]<- temp_output$face[[1]]$attribute$age$value
  }




