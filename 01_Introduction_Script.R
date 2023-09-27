##########------   Installation  -------########

##########------   R  -------########

# To install R we can install it from the internet by either googling or
# 
# For Windows: <https://cran.r-project.org/bin/windows/base/>
#   
#   For Mac: <https://cran.r-project.org/bin/macosx/>
#   
#   For Linux: <https://cran.r-project.org/bin/linux/>
  
##########------   RStudio  -------########  

#   R is an open source statistical programming language. It can be used through many Graphic User Interfaces (GUI) my preference is 
# to use RStudio but VSCode is good and you can also code in base R (as well as many others). 
# 
# We can install RStudio from <https://posit.co/download/rstudio-desktop/>
  
##########------   Basics  -------########  

#   This tutorial will rely on using code written in RStudio and locations of things (Script, Console, Environment, Plots/Help) will be RStudio specific but the code could be run in any GUI.
# 
# Scripts are saved code that you are editing (What I am writing in currently), you then execute (run) the code in the 'console' (Normally located below the script window)
# 
# You can execute one line of code by having your cursor on that line in the script or select many lines then click the run button or cmd+enter (mac) or ctrl+enter (pc)
# 
# Everything to the right of a hastag '# ' is not executed, therefore we can use this to make comments or write notes in our scripts
# 
# R code can be used to do simple calculations with values or even create lists, vectors, values, dataframes and more complex objects in the "global environment", where R stores our information in a session. (normally top right)

##########------   Simple Mathematics  -------########

6*6

4*3/4

4+3/5

(4+3)/5 #  mathematical ordering matters!

sqrt(144)

pi


# We can use either <- or = to assign a value, list or dataframe into an object, thus saving it to R's global environment for use later

# An object is something (usually some sort of data) that is saved in temporary memory (global environment)


a<- 17


##########------   Functions  -------########

# In R we can use functions to do tasks for us, they normally precede a parenthesis (), some are named after what they do and some are less well named,  
# 
# Within functions there are 'arguments' (like options), what you put into these arguments will define how they perform. 
# 
# One function used very often is c(),
# 
# We use c() to concatenate elements together, which means combine them into a vector, which is a series of values
 

b<- c(1,5,5,3,7)


# We can apply functions to an object


mean(b)


# If we want to check the documentation for a package we can go to the Help window, or type ? before the function name.


?mean()



# We can then perform different functions between objects


a*b

mean(a*b)


# We can even save the results to a new object


p<-a*b


# Then we can look at what is in the new object by running the object or printing it (print())


p

print(p)


# We can also create data systematically with R
# 
# For example a sequence of 10 values going up by 1


 
Sequence<-seq(from=1,to=10,by=0.5)

Sequence

AnotherSequence<-c(1:200)

AnotherSequence



# We will come back to generating data systematically later.

##########------   Data Type   -------########

# In R there are many different types of data, the most common four are Numeric, Integer, Character and Factor. 
# 
# Logical and Complex are also data types but very rarely used explicitly. (logical is used a lot by functions but we rarely use it ourselves)
# 
# Numeric data is any real numbers so 8 or 12.3 or 1.00000002 etc, while Integer data is just whole numbers 3, 4, 111 etc
# 
# Character data are words or letters surrounded by quotations (either " or ') such as "A", "Red", 'Treated', Character data has no order to it in Rs 'mind'
# 
# Factor data is like character data but r (or you) have assigned an order to it e.g. "A", "B", "C"
# 
##########------   Objects  -------######### 

# As we saw above we can store data in R as an Object, these can be many different types and combinations, 
# 
# The most common Object types are Vectors, Lists, Matrices, DataFrames and Arrays, 
# 
# The main differences of these Object types are what types and combinations of data can be stored in them and how many dimensions they have, 
# 
##########------   Vectors  -------######### 

# A single group of one data type (it could be Numeric, Character, Integer, Factor), with one dimension is called a Vector.
# 

Vector_Numeric<- c(1.3,5.8,5.122,3.00,7.12)

Vector_Integer<- as.integer(c(1,5,5,3,7)) #  we change between data types with these functions 

Vector_Character<- c("This","is","A","Character Vector")

Vector_Factor<-as.factor(c("This","is","A","Character", "Vector")) #  Notice how r automatically orders alphabetically if we don't tell it the order



# We can also change between types (even if they don't fit that description)



Convert_Numbers_To_Characters<-as.character(Vector_Numeric)

Convert_Numbers_To_Characters


# Now our numbers are thought of as characters, so we can't apply numeric operations to them!
  

##########------   Matrix  -------######### 

# Multiple groups of one data type (it could be Numeric, Character, Integer, Factor), with two dimensions is called a Matrix.



Matrix_Numeric<- as.matrix(c(1.3,5.8,5.122,3.00,7.12))

Matrix_Character<- as.matrix(c("This","is","A","Character Matrix"))




##########------   Dataframes  -------######### 

# Multiple groups of a combination of data types (it could be Numeric, Character, Integer, Factor), with two dimensions is called a Dataframe. Each element of a dataframe must be the last length as the other elements.



df<-data.frame(Column1=c(1.3,5.8,5.122,3.00,7.12),Column2=c(1,5,5,3,7),Column3=Vector_Factor)




##########------   List  -------######### 

# Multiple groups of a combination of data types or object types (it could be Numeric, Character, Integer, Factor or vectors, dataframes or matrices of these), with two dimensions is called a List. Each element in a list can be a different length to the other elements.



List_Numeric<-list(c(1.3,5.8,5.122,3.00,7.12),
                   c(1,5,5,3,7))

List_From_Vectors<-list(Vector_Character,Matrix_Numeric,Matrix_Numeric)

List_Different_Lengths<-list(Item1=c(1,2,3,4,5,6),Item2=c("a","B","C","D"), Item3=seq(from=1,to=100,by=1))

List_Different_Lengths



##########------   Array  -------######### 

# Multiple groups of one data type (it could be Numeric, Character, Integer, Factor or vectors or matrices), with more than two dimensions is called an Array.



Array_1d<-array(c(Matrix_Numeric,c(1.3,5.8,5.122,3.00,7.12)),dim=c(5))
Array_2d<-array(c(Matrix_Numeric,c(1.3,5.8,5.122,3.00,7.12)),dim=c(5,2))
Array_3d<-array(c(Matrix_Numeric,c(1.3,5.8,5.122,3.00,7.12)),dim=c(5,2,2))



# Arrays are rarely used so probably won't discuss much further.



##########------   Indexing  -------######### 

# Objects have dimensions and we can use a technique called indexing to select specific elements of an object
# 
# We use square brackets to do this,
# 
# If the object is 1 dimensional, one number will return one value


Vector_Numeric[4]


# If the object is 2 dimensional, one number will return that column


df[2]


# by adding a comma we can select the from both dimensions (rows first, then columns)


df[4,2]


# If we want all rows but only a specific column we add a comma without a number


df[,2]


# And vice verse


df[2,]


# We can also use multiple numbers inside c() to select multiple elements

# For example, row 4 and 2 of all columns 


df[c(2,4),]


# Or we can use -c() to select all but the mentioned elements
# 
# For example, all rows but not columns 2 and 4


df[,-c(2,4)]



##########------   Packages  -------######### 

# R relies upon packages, groups of specific functions, which can be installed from the internet and then loaded into a script. 
# 
# Base R, a package already installed and loaded within R, is very powerful and useful but less user friendly for some tasks.
# 
# From Base R we can use the install.packages() function to install a package from online repositories.
# 
# R assumes you want to download packages from CRAN (the official online repository but sometimes you might want to download from other repositories)


# install.packages("dplyr") 


# You only have to do this when you first want the package or want to update it or when you have updated r. 
# 
# Once a package is installed we have to tell R that we want to use functions from this package so we load it


library(dplyr) 


# This needs to be run every new R session when this package is used. 
# 
# We can now run functions from the dplyr library, specifically dplyr is a package, which is part of a group or 'ecosystem' of packages called the tidyverse
# 
# We will use this group of packages for reading and writing data into and out of R (readr), manipulating and organising data (dplyr and tidyr) and visualisng data (ggplot2)
# 

##########------   Data Inspection  -------######### 
# 
# First we can make some data into a dataframe, explore this data, then save it as a file and then read the file back into r.
# 
# R has some very useful random and non-random data generation functions 


Year <- seq(from=1950,to=2023,by=1)
Treatment <- c("Control","Treatment 1","Treatment 2")
Rep<- seq(from=1,to=10,by=1)


# These are three vectors, which we can check information about them with a few simple functions


length(Year)
summary(Year)

length(Treatment)
summary(Treatment)

length(Rep)
summary(Rep)


# We want to combine these vectors so we have a row for each rep, year and treatment, we can do this by expanding the grid and create a new dataframe called df.
# 
# We can inspect specific elements of a dataframe too


df<-expand.grid(Year=Year,Treatment=Treatment,Rep=Rep)

class(df) #  type of object

nrow(df) #  number of rows

ncol(df) #  number of columns

dim(df) #  dimensions of object

head(df) #  the first 6 rows of the df

tail(df) #  the last 6 rows of the df



# This df is all the meta data we want for our dataframe that we want to now make up some response data


Response<-rnorm(n=nrow(df),mean = 15,sd=8) #  we need the response to be same length as the df so we use nrow() for the number of values we want.


# We can then combine this to our df, the dollar sign is used to select one dimension (column) from within an object (here a dataframe)
# 
# the column Response isn't present in the data but by assigning our Response vector to it with df$Response <- Response it adds a new column called Response to the dataframe df.


df$Response<-Response



##########------   Saving and Loading Data  -------######### 


##########------   Data Writing  -------######### 

# Once we have our data set we can save it to our computer, but where that is on our computer is important.
# 
# To do this we need to know where R is looking for files on your computer. This is called your current working directory. 
# 
# This information is displayed at the top of the console in Rstudio or you can use the base R function getwd().
# 
# We can set our working directory to change where this is in r (not recommended normally), or we can use our saving/loading functions to look in the correct folders (recommended).
# 
# Side Note: there is a method for not really needing either of these called using Projects (highly recommended) but that is a bit more advanced so lets leave that for now.
# 
# Lets find out where our current working directory is, we can then create a new folder in that location, then save our df to that location.



getwd()

dir.create("NewFolderName/")



# We now can save the df we created to this new folder using the write.csv() function from base r, or even better the write_csv() function from the readr package
# 
# To save to our folder we only need to say the directory we want the file saved to, followed by a /, then the name of the file with file extension.
# 
# Inside reading and writing functions such as write_csv() the main argument will be where is the file to be save to or taken from and we write this out as a character string inside quotations. 



#  install.packages("readr")

library(readr)

write_csv(df,"NewFolderName/R_is_Really_Cool.csv")




##########------   Data Reading  -------######### 

# Often we don't want to make fake data as done here, but we will have our own data set that we want to read in from our computer to then clean, organise, manipulate, visualise, analyse and report on.
# 
# These data are normally saved as excel spreadsheets. However, Excel is awful and should never be used for reproducible science! That being said it is often where a data spreadsheet starts before we bring it into R.
# 
# Excel spreadsheets (.xsl) have lots of added information that actually is not needed and becomes complicated to work with so the easiest file format to read into R is a Comma Separated Values sheet (.csv)
# 
# We can convert our spreadsheet in Excel to a .csv file, then we read in the .csv file with the base function read.csv() or even better the readr function read_csv().
# 
# Again, inside reading and writing functions such as read_csv() the main argument will be where the file is. we write this out with a character string inside quotations. 
# 
# To navigate up or down inside folders on your computer you use / to signify a folder, with the highest level folder on the far left
# 
# For example:

My_DF<-read_csv("NewFolderName/OurNewFile.csv")


##########------   Cheatsheets  -------######### 

# For almost all popular and well used packages there are "cheatsheets" available that provide info on all their most used functions and how to use them.
# 
# You can google them. However, as Packages update there may be deprecated functions (not in use any more), but normally the package will tell you the name of the new function.
# 
# Here is a short list of some of the packages we will use (these links were all found from googling so may break over time):

#  - [Tidyverse](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
#  
#  - [dplyr](https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf)
#  
#  - [readr](https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_data-import.pdf)
#  
#  - [tidyr](https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf)
#  
#  - [ggplot2](https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf)

