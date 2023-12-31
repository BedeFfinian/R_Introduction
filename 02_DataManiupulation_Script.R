
#####----Real World Data  
  
#  #  #####----Overview of Data
  
#  Packages often have their own example datasets within them, or sometimes a package can be used to store just data without functions etc.
#  
#  We will look at the palmer penguins dataset


#  install.packages("palmerpenguins")

library(palmerpenguins)

data()

data(penguins)


#  This becomes a 'promise' of a data set, we have to do something with it to get it properly, lets take a look inside



library(tidyverse)
glimpse(penguins)



#  This gives us two datasets in our global environment
#  
#  Using summary() we can see which columns have NAs and which don't.


summary(penguins)


#  The penguins data set is fairly well organised but we can still do a bit more with it if we want


summary(penguins_raw)


#  The raw data has a lot of extra information that may or may not be important for us. The raw data has lots of difficult to deal with column names. 

#####----All Hail Hadley Wickham: tidyverse 


#  For almost all basic tasks in r I prefer and would recommend using the tidyverse, but there are many other packages for data manipulation, organisation, visualisation and analysis
#  
#  Hadley Wickham is an amazing package writer and is heavily involved with creating the tidyverse, he is also amazing at explaining very complex things (Check out his Youtube).
#  
#  The main drawbacks of the tidyverse are to do with speed at scale, tidyverse code is less efficient with big data (10s of Gigabytes)
#  
#  The main advantages in my eyes is readability and intuition. Consistency of syntax (the order of arguments and naming of functions etc.) is also very important in the eyes of the tidyverse, where the main idea is using and aiming for what is called 'tidy' data. 

#  #####----What is 'Tidy' data? 

#  From the [original paper](https://vita.had.co.nz/papers/tidy-data.pdf) discussing this: "Tidy datasets provide a standardized way to link the structure of a dataset (its physical layout) with its semantics (its meaning)."

#  This means that Tidy data is a standard way of mapping the meaning of a dataset to its structure. A dataset is messy or tidy depending on how rows, columns and tables are matched up with observations, variables and types. 

#  In tidy data:
#  
#  - Every column is a variable.
#  
#  - Every row is an observation.
#  
#  - Every cell is a single value.
#  
#  This may not seem that important or even intelligible now but when we start plotting data it becomes very important.

#  #####----Enough Theory - dplyr and tidyr

#  For data manipulation and organisation we will rely heavily on the dplyr and tidyr packages, which have a suite of functions that can be used in isolation or combined to perform complex data manipulation and organisation.

#  #  #####----Easy to Read Code

#  For writing easy to follow and understand code/scripts with complex sequences of functions, putting our code across multiple lines is a technique we can use.
#  
#  This can be done by doing an enter/carriage return after a comma inside of a function between arguments.
#  
#  This technique changes nothing of how the function works (we can check if the two outputs are equal with all.equal())



df_without_enters<-data.frame(Column1=c(1.3,5.8,5.122,3.00,7.12),Column2=c(1,5,5,3,7))

df_with_enters<-data.frame(
  Column1=c(1.3,5.8,5.122,3.00,7.12),
  Column2=c(1,5,5,3,7)
  )

all.equal(df_without_enters,df_with_enters)


#  #  #####----Piping (Native and maggittr)


#  What do we do when we want to apply multiple functions in a sequence but don't want to create loads of objects in our global environment?
#####---- 
#  One option is putting one function inside of another etc (Sometimes fine). This is called nesting.



NestingFunctions<-summary(subset(df_with_enters, Column1==1.3))



#  This can be okay but generally is hard to follow, as the last function that is applied is the first one you read from left to right.
#  
#  In R there is an operator that allows you to pass the result from one function into the next function this is called the Native Pipe |> 
#
#  Again we can check this creates the same thing with all.equal()



NativePipingingFunctions<-df_with_enters|>
  subset(Column1==1.3)|>
  summary()

all.equal(NestingFunctions,NativePipingingFunctions)



#  This operator is actually quite new and was based on another commonly used pipe (and more superior in my mind).
#  
#  The magittr pipe %>% (shift+cmd+m or shift+ctrl+m) was from a package called magittr that is automatically loaded by any tidyverse package.
#  
#  It works very similarly to the native pipe with some subtle changes. 
#  
#  Again I feel the magittr pipe makes code that is easier to read.



MaggittrPipingingFunctions<-df_with_enters %>% 
  subset(Column1==1.3) %>% 
  summary()

all.equal(MaggittrPipingingFunctions,NativePipingingFunctions)



#  The pipe can be thought of as "and then"
#  
#  So above, the df_with_enters is subset where Column1 is equal to 1.3 and then the summary function is used.
#  
#  When using one function it is not needed, but when using multiple functions in a row piping makes code a lot easier to read and understand what order functions have been carried out in.
#  
#  Again this readability has drawbacks in being slower (for small data of 100s of rows this may be 0.0001 of a second but big data 10000000000s of rows it might be a few seconds)
#  
#  Later on we will use pipes in long sequences of functions and it will become clearer how useful they are.

#  #  #####----Filtering

#  The dplyr function filter() is a row wise subsetter, based on a statement from the dataframe.
#  
#  When we looked at the summary() of penguins we saw some NAs in the biometric columns and also in the sex column.
#  
#  If we want to remove NA's there are many ways, to be selective we can filter our dataset. 
#  
#  To subset data, we create a logic clause that then filters the dataset by that clause/statement,
#  
#  For example if we want to select all rows of the data set where the data is from a female penguin we can do this by:



female_penguins<- penguins %>% 
  filter(sex=="female")

female_penguins



#  Notice there are two ='s!! 
#
#  This is used to create our clause/statement, we filter (keep) the rows of the pengiuns dataset if the sex column contains "females", if just one equals (=) is used it won't work.

#  Or we might want all the penguins above 5 kg.



heavier_penguins<- penguins %>% 
  filter(body_mass_g>= 5000)

heavier_penguins



#  There are a range of symbols we can use such as more than (>), less than (<), more than or equal to (>=), less than or equal to (<=), is equal to (==), and (&), or (|).
#  
#  We can even use multiple clauses or statements in one call to filter, 
#  
#  So if we want all the heavier female penguins 



heavier_female_penguins<- penguins %>% 
  filter(body_mass_g>= 5000 & sex=="female")


heavier_female_penguins




#  Sometimes we might want to filter with multiple answers of a categorical variable, 
#  
#  For example if we wanted all penguins from Biscoe and Torgersen island
#  
#  To do this we can make a vector of the names we want, then filter by that vector (%in%).




Islands_we_Want<-c("Biscoe","Torgersen")

Biscoe_Torgersen_penguins<- penguins %>% 
  filter(island%in%Islands_we_Want)

Biscoe_Torgersen_penguins




#  Here we will make use of !, this means the opposite of the clause (not this), a subtraction sign can also be used (-).
#  
#  We also use %in% which is used to tell filter there are more than one elements or we can use it for NAs that we want to get rid of as NA is not classed like normal data (It is a lack of data not a character or number).


penguins_someNAs<-penguins %>% 
  filter(!body_mass_g%in%NA)


#  If we now look at the number of rows of the datasets we can see only two rows were removed. (not all the NAs)
#  
#  We just removed the rows with NA in the body_mass_g column.


nrow(penguins)
nrow(penguins_someNAs)

summary(penguins_someNAs)


#  Still 9 NAs in sex


penguins_noNAs<-penguins_someNAs %>% 
  filter(!sex%in%NA)

summary(penguins_noNAs)


#  We might want to remove all rows where an NA is in any of the columns (not always advisable)



penguins_noNAs_quickly<-penguins %>% 
  drop_na()

all.equal(penguins_noNAs,penguins_noNAs_quickly)





#  #  #####----Selecting 

#  The dplyr function select() is a column wise subsetter based on a statement of column names.
#  
#  So we can select or deselect a few named columns using select.
#  
#  And as with filter we can use - or ! to say not this column/statement.



Three_Columns<-penguins_raw %>% 
  select(studyName,Species,Island)

All_But_Three_Columns<-penguins_raw %>% 
  select(-studyName,-Species,-Island)

names(Three_Columns)

names(All_But_Three_Columns)



#  We can also use a statement for consistencies across columns (contains() or even starts_with() or ends_with())
#  
#  For example all columns that contains() an "s" or even combining a statement with other specific selections



S_Columns<-penguins_raw %>% 
  select(contains("s"))

S_Columns_No_Sex<-penguins_raw %>% 
  select(contains("s"),-Sex)

S_Columns_Plus_Region<-penguins_raw %>% 
  select(contains("s"),Region)

names(S_Columns)
names(S_Columns_No_Sex)
names(S_Columns_Plus_Region)




#  We can even make a vector of column names and then pass that vector to select() using the all_of() or any_of() functions.



Columns_We_Want<-c("Region","Island","studyName","Stage")

Columns_From_Vector<-penguins_raw %>% 
  select(all_of(Columns_We_Want))

Columns_Not_From_Vector<-penguins_raw %>% 
  select(-all_of(Columns_We_Want))

names(Columns_From_Vector)

names(Columns_Not_From_Vector)



#  Another nice feature of select is that the order is maintained, so the order of things we select is used to order the columns, 
#  
#  So if we want to move a certain column towards the beginning of the df we can do this using select() and put everything() to say everything else after the columns we put first



Region_First<-penguins_raw %>% 
  select(Region,everything())

Region_Then_Island_First<-penguins_raw %>% 
  select(Region,Island,everything())

names(penguins_raw)

names(Region_First)

names(Region_Then_Island_First)



#  #  #####----Mutating

#  We have data but maybe we want to transform that data and either replace the original column or create a new column.
#  
#  We can use dplyr's mutate() to do this. 
#  
#  Lets convert the body mass column into a new column that is in kg.


penguins_kgbodymass<-penguins %>% 
  mutate(body_mass_kg=body_mass_g/1000)

summary(penguins_kgbodymass)



#  We can also paste information from other columns together into another new column.
#  
#  We shall use the paste() function then we will put what character we want to separate each element by using the sep argument.



penguins_ExtraInfo<-penguins_noNAs %>% 
  mutate(Info=paste(species,island,sex,sep="_"))

unique(penguins_ExtraInfo$Info)



#  We can even do calculations that are based on and element in another column at the same row. 
#  
#  There are a few ways to do this, the simplest is an if_else() statement.
#  
#  With if_else() there are three arguments, the first argument is the statement (is it female), the second argument is what to do if the statement is true and the third argument is what to do if the statement is false.
#  
#  Lets pretend that when an Adelie penguin was studied they were incorrectly weighed by 200 g.
#  
#  We shall replace the old body_mass_g with new corrected weight, but only for Adelie penguins.
#  
#  To look at the change we will plot a histogram of body weights for both weights.


penguins_if_else<-penguins_noNAs %>% 
  mutate(body_mass_g=if_else(species=="Adelie",
                             body_mass_g+200,
                             body_mass_g))

hist(penguins_noNAs$body_mass_g)

hist(penguins_if_else$body_mass_g)



#  While fine for one single statement, multiple if_else() statements can create horrible code. 
#  
#  For this we can use case_when(),
#  
#  Where we take a statement, then use ~ to say the new column value, then a comma before the next statement
#  
#  Maybe Gentoos were also miss-measured but the other way round (too big).


penguins_case_when<-penguins_noNAs %>% 
  mutate(body_mass_g=case_when(species=="Adelie"~body_mass_g+200,
                               species=="Gentoo"~body_mass_g-200,
                               species=="Chinstrap"~body_mass_g))

hist(penguins_noNAs$body_mass_g)

hist(penguins_case_when$body_mass_g)



#  With case_when we have to be careful if all of our statements don't cover all the data.
#  
#  If there is a condition not covered it will return NA values. 
#  
#  To avoid this we can do a final statement with TRUE~Our_Default




penguins_case_when_Missing<-penguins_noNAs %>% 
  mutate(body_mass_g=case_when(species=="Adelie"~body_mass_g+200,
                               species=="Gentoo"~body_mass_g-200))

penguins_case_when_TRUE<-penguins_noNAs %>% 
  mutate(body_mass_g=case_when(species=="Adelie"~body_mass_g+200,
                               species=="Gentoo"~body_mass_g-200,
                               TRUE~body_mass_g))

all.equal(penguins_case_when_TRUE,penguins_case_when_Missing)



#  This creates 68 NA values in our data, so need to be aware of this.
#  
#  But we could use it for bug checking.

#  #  #####----Summarise by Groups

#  Often we will want to see summaries of data across groups, using a combination of group_by() and summarise() can give use these summary stats.
#  
#  We can group by one, two or many columns, the more groups the less data will summarised in each group.
#  
#  If we don't group by a column it will not be in the final dataset.



penguins_noNAs %>% 
  group_by(year) %>% 
  summarise(Mean_body_mass=mean(body_mass_g))

penguins_noNAs %>% 
  group_by(year,species) %>% 
  summarise(Mean_body_mass=mean(body_mass_g))

penguins_noNAs %>% 
  group_by(year,species, island) %>% 
  summarise(Mean_body_mass=mean(body_mass_g))

penguins_noNAs %>% 
  group_by(year,species, island,sex) %>% 
  summarise(Mean_body_mass=mean(body_mass_g))



#  We can also use groups to count numbers of rows within each group.
#  
#  Although the table() function in base r does the same but it is harder to read and use when lots of columns selected.


penguins_noNAs %>% 
  group_by(year,species,island,sex) %>% 
  summarise(Number=n())

penguins_noNAs %>% 
  select(year,species,island,sex) %>% 
  table()




#  #  #####----Wide and Long Data

#  Tidy data is generally in what could be considered a long format, where each row is an individual observations often having a column that repeats itself.
#  
#  But for some visualisation tools or for making nice looking tables it might be better to be in wide format.
#  
#  Lets take some of the summaries from above to create a wide database from our last summary which was hard to read because of its length.
#  
#  To go between wide and long data we will use pivot functions from tidyr, namely pivot_wider() and pivot_longer().


#  install.packages("tidyr")

library(tidyr)

penguins_noNAs %>% 
  group_by(year,species, island,sex) %>% 
  summarise(Mean_body_mass=mean(body_mass_g)) %>% 
  pivot_wider(names_from = species, values_from = Mean_body_mass)



#  It is still quite long but we could also add more info into the wider columns (e.g. year or sex)



penguins_noNAs %>% 
  group_by(year,species, island,sex) %>% 
  summarise(Mean_body_mass=mean(body_mass_g)) %>% 
  pivot_wider(names_from = c(species,year), values_from = Mean_body_mass)

penguins_noNAs %>% 
  group_by(year,species, island,sex) %>% 
  summarise(Mean_body_mass=mean(body_mass_g)) %>% 
  pivot_wider(names_from = c(species,sex), values_from = Mean_body_mass)



#  Often as ecologists we will be surveying a whole community and counting numbers of each different species at each site.
#  
#  This data often comes to us as wide data, our summary of counts could be turned into a wide df (we will fill NAs as 0s) and we can the convert it back to a long dataframe.



WideCounts<-penguins_noNAs %>% 
  group_by(year,species,island,sex) %>% 
  summarise(Number=n()) %>% 
  pivot_wider(names_from = species,values_from = Number, values_fill = 0)

WideCounts



#  Okay so we now have a count of different sexes of species of penguins measured in different years and islands
#  
#  Lets make this data long, to do this we have to tell the function which columns are to be pivoted, and what we want to call the new columns.
#  
#  We can either tell it which columns should or should not be pivoted, or we can even say which position columns to use with numbers. (using the colon means from one thing to the other thing)



LongCounts_1<-WideCounts %>% 
  pivot_longer(c(Adelie,Chinstrap,Gentoo),names_to = "species",values_to = "Number")

LongCounts_2<-WideCounts %>% 
  pivot_longer(-c(year,island,sex),names_to = "species",values_to = "Number")

LongCounts_3<-WideCounts %>% 
  pivot_longer(4:6,names_to = "species",values_to = "Number")

LongCounts_4<-WideCounts %>% 
  pivot_longer(-c(1:3),names_to = "species",values_to = "Number")


#  Lets check they are all the same to finish off.


all.equal(
  LongCounts_1,
  LongCounts_2
)

all.equal(
  LongCounts_3,
  LongCounts_4
)

all.equal(
  LongCounts_1,
  LongCounts_4
)










