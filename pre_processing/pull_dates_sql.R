#!/usr/bin/env Rscript

# Print available dates in a given sqlite file

library(DBI,quietly = T)
library(RSQLite,quietly = T)
library(harp,quietly = T)
library(argparse,quietly = T)
library(tibble,quietly = T)
library(knitr,quietly=T)
suppressPackageStartupMessages(library(dplyr))

parser <- ArgumentParser()

parser$add_argument("-dbase", type="character",
    default="None",
    help="Path to the database [default %(default)s]",
    metavar="SQlite file path")

parser$add_argument("-table", type="character",
    default="FC",
    help="Table name [default %(default)s]",
    metavar="The name of the table (FC for forecast,SYNOP,TEMP for obs,etc )")

parser$add_argument("-csv_file", type="character",
    default="None",
    help=" CSV file[default %(default)s]",
    metavar="The name of the CSV file to write currently available dates")


args <- parser$parse_args()
dbase <- args$dbase
table <- args$table
ofile <- args$csv_file

db <- dbConnect(SQLite(), dbase)
#to check the tables
#dbListTables(db)

#This prints an overview of the table
#tbl(db, table)
#print available dates
cat("Dates available in ",dbase,"\n")
if (table == "FC") {
  out <- tbl(db, table) %>% pull(fcdate) %>% unique() %>% unix2datetime()
  df <- enframe(out)

  if (ofile != "None") {
  write.table(df$value,ofile, col.names=FALSE,row.names=FALSE) 
  }
  else {
  kable(df)
  }

} else if (table == "SYNOP" || table == "TEMP") {
  out <- tbl(db, table) %>% pull(validdate) %>% unique() %>% unix2datetime()
  df <- enframe(out)
  kable(df)
} else {
  print(paste0("I do not know table: ",table,", sorry!"))
}

dbDisconnect(db)

