# ---------------- mpesa file----------
##' @name mpesa_file
##'
##' @title Imports and formats MPESA file.
##'
##' @description This function loads and formats the raw pdf statement int your work space.Its important to note that this function is built over pdftools package.The function thus adds more functionality to pdftools::pdf_text() function.
##' @param path Path to your file.
##' @param password MPESA statement password.A pop up will appear if not filled in.hint your national id
##' @rdname mpesa_file
##' @export
##' @examples
##' mpesa_file(path = "path/to/my/mpesa_statement.pdf",password = "my secret password")

mpesa_file <- function(path,password = NULL) {
  if (is.null(password)) {
    rawfile <- str_split(pdf_text(pdf = path,opw = as.character(askpass::askpass())),"\n")
  }else{
    rawfile <- str_split(pdf_text(pdf = path,opw = as.character(password)),"\n")
  }
  return(rawfile)
}
# ---------------- statement header----------
##'
##' @rdname statement_header
##' @name statement_header
##' @title Get statement Bio Data
##' @description This function enables you to extract biodata information from an MPESA statement.
##' @param rawfile MPESA pdf file formated by mpesa_file() function
##' @return \code{statement_header(rawfile)} returns tidy dataframe of  MPESA statement header information
##' @examples
##' library(mpesaR)
##' rawfile = mpesa_file()
##' statement_header(rawfile)
##' #using pipe operator
##' mpesa_file("path/to/MPESAfile.pdf","my_national_id") %>%
##' statement_header()
##' @export
statement_header <- function(rawfile) {
  header <- rawfile[[1]][3:7] %>%
    map(~gsub("  ","_",.x)) %>%
    unlist() %>%
    enframe() %>%
    separate(value,into = c("Bio","Details"),sep = "____") %>%
    mutate(Details = trimws(gsub("_","",Details))) %>%
    select(-name)
  return(header)
}
# ------------statement summary-----
##'
##' @rdname statement_summary
##' @name  statement_summary
##' @title Get Statement Summary statistics
##' @description This function extracts the statement summary at the begining of every mpesa statement file
##' @inheritParams statement_header
##' @return \code{statement_summary(rawfile)} returns tidy dataframe of the MPESA summary statistics
##' @examples
##' library(mpesaR)
##' statement_summary(rawfile)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","my_national_id") %>%
##' statement_summary()
##' @export

statement_summary <- function(rawfile) {
  summary <-
    rawfile[[1]][(str_which(rawfile[[1]], pattern = "SUMMARY") + 1):(str_which(rawfile[[1]], pattern = "DETAILED") -1)] %>%
    map( ~ gsub("  ", "_", .x)) %>%
    unlist() %>%
    enframe() %>%
    mutate(value = str_remove(gsub(
      '([[:alpha:]])\\1{2,}',
      '\\1',
      str_replace_all(value, "_", "s")
    ), "\r")) %>%
    separate(value,
             into = c("TRANSACTION TYPE", "PAID IN", "PAID OUT"),
             sep = "s") %>%
    slice(2:9) %>%
    mutate(`PAID IN` = as.double(gsub(",", "", `PAID IN`)), `PAID OUT` = as.numeric(gsub(",", "", `PAID OUT`))) %>%
    select(-name)
  return(summary)
}
# ------------statement full-----
##'
##' @rdname statement_full
##' @name  statement_full
##' @title Get Full Statement
##' @description This function extracts all mpesa statement tables for all pages ,cleans and presents it as a tidy dataframe.
##' @inheritParams statement_header
##' @return \code{statement_full(rawfile)} returns tidy dataframe of the MPESA statements tables.
##' @examples
##' library(mpesaR)
##' statement_full(rawfile)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","my_national_id") %>%
##' statement_full()
##' @export

statement_full <- function(rawfile) {
  statement <-
    rawfile %>%
    # split all pages and combine back again
    map(function(x){x[(str_which(x,"Receipt No.")+2):(str_which(x,"Disclaimer:")-1)]}) %>%
    unlist() %>%
    # replace the tabbed soaces with an underscore
    map( ~ gsub("\t", "_", .x)) %>%
    unlist() %>%
    # transorm it into a dataframe
    enframe() %>%
    select(value) %>%
    # make the data tidy
    mutate(value = str_replace(gsub("\\s+\\s+", "_", str_trim(value)), "B", "B"),
           value = ifelse(!str_detect(value,"_Completed"),str_replace(value,"Completed","_Completed"),value)) %>%
    separate(value,into = c('Receipt No.', 'Completion Time','Details','Transaction','Paid In','Withdrawn'),sep = "_" ) %>%
    mutate(Balance = as.numeric(gsub(",","",Withdrawn)),
           Withdrawn = ifelse(as.numeric(gsub(",","",`Paid In`))<0,as.numeric(gsub(",","",`Paid In`)),NA),
           `Paid In` = ifelse(as.numeric(gsub(",","",`Paid In`))>0,as.numeric(gsub(",","",`Paid In`)),NA),
           Details = ifelse(is.na(Details[seq.int(nrow(.))+1]) & is.na(Details[seq.int(nrow(.))+2]),
                            paste(Details,`Receipt No.`[seq.int(nrow(.))+1],`Receipt No.`[seq.int(nrow(.))+2]),
                            ifelse(is.na(Details[seq.int(nrow(.))+1]),
                                   paste(Details,`Receipt No.`[seq.int(nrow(.))+1]),Details)))%>%
    filter(!is.na(`Completion Time`))

  return(statement)
}


# ------------number of transactions-----
##'
##' @rdname trx_no
##' @name  trx_no
##' @title How many transactions?
##' @description This function gives you to get the number of transactions in a given financial reporting period
##' @param statement This is the full MPESA statement table generated by statement_full() function.Please note that this function does not work without the statement.
##' @return \code{trx_no(rawfile)} returns a numeric value of the number of transactions.
##' @note The function doesnt count the mpesa charge cases.
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' trx_no(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","my_national_id") %>%
##' statement_full() %>%
##' trx_no()
##' @export

trx_no <- function(statement) {
  totalTransactions <-
    statement %>% distinct(`Receipt No.`) %>% nrow()
  return(totalTransactions)
}


# ------------KEY KPIS-----
##'
##' @rdname statement_kpis
##' @name  statement_kpis
##' @title Key KPIS
##' @description Quckly generate key kpis from the MPESA statement supplied.
##' @inheritParams trx_no
##' @return \code{statement_kpis(statement)} returns key kpis from the mpesa statement
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' statement_kpis(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
##' statement_full() %>%
##' statement_kpis()
##' @export

statement_kpis <- function(statement) {
  mpesaStatement <- statement
  kpis <- mpesaStatement %>%
    filter(!is.na(Withdrawn)) %>%
    summarise(KPI = "Cash_Out",Amount = abs(sum(Withdrawn)),Count = n(),) %>%
    bind_rows(
      mpesaStatement %>%
        filter(!is.na(`Paid In`)) %>%
        summarise(KPI = "Cash_In",Amount = abs(sum(`Paid In`)),Count = n(),)
    ) %>% bind_rows(
      #agentdeposits

      mpesaStatement %>%
        filter(str_detect(Details,"Deposit of Funds at Agent")) %>%
        summarise(KPI = "AgentDeposits",Amount = sum(`Paid In`),Count = n())
    ) %>% bind_rows(
      # agent withdrawals
      mpesaStatement %>%
        filter( grepl('Withdrawal At Agent|Withdrawal Charge', Details))%>%
        summarise(KPI = "Agent_Withdrawals",Amount = abs(sum(Withdrawn)),Count = n())

    ) %>% bind_rows(
      # paybilPayments
      mpesaStatement %>%
        filter( grepl('Pay Bill', Details))%>%
        summarise(KPI = "Pay_Bill_Cash",Amount = abs(sum(Withdrawn)),Count = n())

    )
  return(kpis)
}


# ------------where did the money come from-----
##'
##' @rdname statement_in
##' @name  statement_in
##' @title Where did money come from?
##' @description Quckly generate extract vital information on the sources of icoming cash to the statement.
##' @inheritParams trx_no
##' @return \code{statement_in(statement)} returns a dataframe with sources of income
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' statement_in(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
##' statement_full() %>%
##' statement_in()
##' @export

statement_in <- function(statement) {

  incash <- statement %>%
    filter(!is.na(`Paid In`)) %>%
    mutate(Source = ifelse(
      grepl("Deposit of Funds at Agent", Details),
      "MpesaDeposit",
      ifelse(
        grepl("M-Shwari Withdraw|M-Shwari", Details),
        "M-Shwari",
        ifelse(
          grepl("Funds received from", Details),
          "People",
          ifelse(
            grepl("Business Payment", Details),
            "Business Payment",
            "Others"
          )
        )
      )
    )) %>%
    separate(
      Details,
      into = c("Info", "Source1"),
      sep = " - ",
      remove = F
    ) %>%
    mutate(
      Source1 = ifelse(is.na(Source1), Info, Source1),
      Source1 = ifelse(Source == "MpesaDeposit", Source, Source1),
      Source1 = str_remove_all(Source1, "B2C ")
    ) %>%
    group_by(Source1) %>% summarise(NoTrx = n(), Amount = sum(`Paid In`))
  return(incash)
}


# ------------where did the money go to-----
##'
##' @rdname statement_out
##' @name  statement_out
##' @title Where did expense come from?
##' @description Quckly generate and extract vital information on where most expenses come from.
##' @inheritParams trx_no
##' @return \code{statement_out(statement)} returns a dataframe with a list of sources of expenses
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' statement_out(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
##' statement_full() %>%
##' statement_out()
##' @export

statement_out <- function(statement) {

  outCash <- statement %>%
    filter(!is.na(Withdrawn)) %>%
    separate(
      Details,
      into = c("Info", "Source"),
      sep = " - ",
      remove = T
    ) %>%
    mutate(
      Source = ifelse(is.na(Source), Info,
                      ifelse(
                        grepl("Customer Withdrawal", Info), "Mpesa Withdrawal", Source
                      )),
      Source = ifelse(
        grepl("Charge", Source),
        "Mpesa Charges",
        ifelse(grepl("Airtime Purchase", Info), "Airtime Purchase", Source)
      )
    ) %>%
    separate(Source, into = c("Source", NA), sep = "Acc.") %>%
    group_by(Source) %>%
    summarise(Count = n(), Total = abs(sum(Withdrawn))) %>%
    arrange(desc(Total))
  return(outCash)
}


# ------------where did the money go to-----
##'
##' @rdname statement_out
##' @name  statement_out
##' @title Where did expense come from?
##' @description Quckly generate and extract vital information on where most expenses come from.
##' @inheritParams trx_no
##' @return \code{statement_out(statement)} returns a dataframe with a list of sources of expenses
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' statement_out(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
##' statement_full() %>%
##' statement_out()
##' @export

statement_out <- function(statement) {

  outCash <- statement %>%
    filter(!is.na(Withdrawn)) %>%
    separate(
      Details,
      into = c("Info", "Source"),
      sep = " - ",
      remove = T
    ) %>%
    mutate(
      Source = ifelse(is.na(Source), Info,
                      ifelse(
                        grepl("Customer Withdrawal", Info), "Mpesa Withdrawal", Source
                      )),
      Source = ifelse(
        grepl("Charge", Source),
        "Mpesa Charges",
        ifelse(grepl("Airtime Purchase", Info), "Airtime Purchase", Source)
      )
    ) %>%
    separate(Source, into = c("Source", NA), sep = "Acc.") %>%
    group_by(Source) %>%
    summarise(Count = n(), Total = abs(sum(Withdrawn))) %>%
    arrange(desc(Total))
  return(outCash)
}


# ------------where statement Period-----
##'
##' @rdname statement_period
##' @name  statement_period
##' @title What is the transaction period?
##' @description Gives you the transaction period from the start of the statement?.
##' @inheritParams statement_header
##' @return \code{statement_period(rawfile)} returns period difference
##' @examples
##' library(mpesaR)
##' statement <- statement_full(rawfile)
##' statement_period(statement)
##' #using the pipe ( %>% ) operator
##' mpesa_file("path/to/MPESAfile.pdf","MYSECRETPASWWORD") %>%
##' statement_full() %>%
##' statement_period()
##' @export

statement_period <- function(rawfile) {
  header <- statement_header(rawfile)
  period <- header %>%
    filter(Bio == "Statement Period") %>%
    separate(Details,
             into = c("StartDate", "EndDate"),
             sep = " - ") %>%
    mutate_at(.vars = c("StartDate", "EndDate"),
              .funs = lubridate::dmy) %>%
    mutate(Period = round(as.numeric(difftime(EndDate, StartDate,) / 30)))
  return(period)
}




#
# ------------ statement Period-----
##'
##' @rdname loan_limit
##' @name  loan_limit
##' @title What is the possible limit?
##' @description Gives you the posible limit a client should be granted.The limit is calculated by getting 75% of the monthly average Credit ie "PaidIN" in this case
##' @inheritParams statement_header
##' @return \code{statement_period(rawfile)} .
##' @examples
##' library(mpesaR)
##' limit <- loan_limit(rawfile)
##' @export

loan_limit <- function(rawfile) {
  summary <- statement_summary(rawfile)
  limit <- summary %>% filter(`TRANSACTION TYPE` == "TOTAL") %>%
    mutate(AverageCr = (`PAID IN` / 6),
           Limit = floor((AverageCr  *.75)/50)*50)
  return(limit)
}



