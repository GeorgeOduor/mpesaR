# mpesaR
implementation of MPESA statement data extraction

With lots of mpesa statements to analyse,I got an urge to create a simple lite package for handling just that.This project will not work for any other statement
except for mpesa statements produced by Safaricom company.I have not tried it with any other pdf so dont be dissapinted if you fail to ge the desired results.

Its also in developements so any issues can be adressed to me.

# How to use.

In order to use this package,you need to have devtools installed in your github computer and load it first.This is because this package has not set its foot to CRAN repository.

```
install.packages(devtools)#if you donot have devtoools pre installed

library(devtools)

devtools::install_github("GeorgeOduor/mpesaR")

library(mpesaR)

```
# Load you mpesa file.

To load your MPESA file all you need is to call the `mpesa_file()` function .Agood practice is to save it in an object.

eg
```
filePath <- "path/to/my/MPESA_file.pdf"
rawfile <- mpesa_file()
```
The function needs a pasword,but for security reasons you can ommit the password and let it ask you interactivelly in a pop up window.

## Where did you Cash go to?

Answer this question by calling `statement_out()` function.
eg

 ```
 myCash = mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
statement_full() %>%
statement_out()

```
 
 ## Note on using this package.
 
 This package relies on pdftools,stringr and tidyverse for its functionality.Install these packages first in order to have a good experice.
 
 ## License
 
 MIT.
 
 
