# Variable Assignment
#
# - Can use `=` or `<-` characters to assign variables. 
# 
# Scalar Values
foo = 1
bar <- 1 # integer
moo = 1.4 # numberic
bool = TRUE # logical
str = "Foo bar!" # character
date = as.Date("01/01/2020", format("%m/%d/%y")) # Date type
factors = factor("summer", levels=c("summer", "fall", "winter", "spring"), ordered=TRUE)

# Type Checking & Printing
class(moo)   # what type of object is this?
names(moo)   # helps you find the various attributes and functions available on this object
print(moo)   # print to stout 

# Vectors
#
# Arrays and vectors are indexed to 1
x = c(4,3,2,1)
x[1] # 4 
x[10] # NA
x[0] # numberic(0)
x[1:3] # 4 3 2
x[x<=3] # 3 2 1
x + 10 # 14 13 12 11
x < 11 # TRUE TRUE TRUE TRUE

# Matrices 
A = matrix(c(1,2,3,4), nrow=2, ncol=2)
A[1,1] # 1
A[1,2] # 3
A[1,]  # 1 3
A[,1]  # 1 2
A * 10 # multiplies each value by 10 
A * A # multiplies based on each 

# Lists 
L = list(name="John", age=55, children.ages=c(15,20))
L$name # "John
L$age # 55
L$children.ages # 15 20

# Loops & Conditional Statements 
# for(name in variable) { }
# while(condition) { }
# if(condition) { }

# Function Declaration
square = function(x) { 
  return(x ** 2)
}

fizzbuzz <- function(x) { 
  fizz = (x %% 5 == 0)
  buzz = (x %% 3 == 0)
  if(fizz & buzz) { 
    return("FizzBuzz")
  } else if(fizz) {
    return("Fuzz")
  } else if(buzz) { 
    return("Buzz")  
  } else { 
    return(x)  
  }
}

# TODO: Build out DataFrame, Linear Regression, Plotting documentation
# DataFrame 
# head # first fiew entires of the dataframe, 6 by default
# tail # last few entries of teh dataframe, 6 by default 
# str(L) # breakdown of the  dataset

# Linear Regression
# lm      # linear model 
# anova   # anova analysis
# summary # summ

# Visualization 
#
# High-level plots 
# plot()
# hist()
# boxplot()
# qqnorm()
# qqline()

# Low-level plots 
# points(), lines(), abline()