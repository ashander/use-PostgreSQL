#' Connect to a PostgreSQL database
#' @param auth.list a list including authorization arguments including host, user, password, dbname, port
#' @param drv.name database driver to use for DBI ('PostgreSQL' on mac/linux)
#' @details constructs a databse connection using DBI-compliant connection via RPostgreSQL package
#' returns list of connection and driver.
#' @import RPostgreSQL
#' @examples \dontrun{
#' options(pg.auth =  alist(host="localhost", user="user", password="", dbname="", port="5432"))
#' ## make a connection
#' c <- pgCon()
#' pgDiscon(c)
#'}
#' @export

pgCon<-function(auth.list=getOption("pg.auth", stop("Need to provide a list of authorization args with elements host, user, password, dbname, port")), drv.name = "PostgreSQL")
  {
    require(RPostgreSQL)
    drv <- dbDriver(drv.name) ## loads the PostgreSQL driver
    if (!is.null(auth.list)) {
      parmsNames <- names(auth.list)
      for (.i in seq(length(parmsNames)))
        assign(parmsNames[.i], auth.list[[.i]])
    }
    con<-dbConnect(drv=drv, host=host, user=user, password=password, dbname=dbname,port=port)
    return(list(connection=con, driver=drv))
  }


#' Close all connections to PostgreSQL DB
#' @param con.drv a list containing a non-expired connection to and driver for PgSQL database
#' @details closes databse connection using DBI-compliant connection via RPostgreSQL package
#' @import RPostgreSQL
#' @examples \dontrun{
#' ## make a connection
#' c <- pgCon() #running without argumetns requires pg.auth list set as global option
#' pgDiscon(c)
#' }
#' @export

pgDiscon<-function(con.drv=NULL){
  ## need error check
  allcons <-dbListConnections(con$driver)
  for (i in 1:length(allcons)){dbDisconnect(allcons[[i]])} ## Closes all connections
  dbUnloadDriver(con.drv$driver)  ## Frees all the resources on the driver
  return (NULL)
}

                   
#' Send/retrieve SQL queries from a PostgreSQL database
#' @param query a valid SQL query 
#' @param con.drv a list containing a non-expired connection to and driver for PgSQL database
#' @details Runs SQL query against and returns result
#' @import RPostgreSQL
#' @examples \dontrun{
#' ## make a connection
#' c <- pgCon()  #running without argumetns requires pg.auth list set as global option
#' mytable.df<-pgQ("select * from myschema.mytable", c)
#' pgDiscon(c)
#' }
#' @export

pgQry<-function(query, con.drv=NULL){
  cat("WARNING: currently\n (1) no error checking for SQL query,\n (2) no checking to make sure query is resonable size!\n (2 means if this is taking a long time break it)\n")
  # con.drv should be a list of connection and driver as returned by pgCon
  new.connect = FALSE
  if(is.null(con.drv)){
    con.drv = pgCon()
    new.connect = TRUE
  }
  rs = dbGetQuery(con.drv$connection, query)
  if(new.connect)
    pgDiscon(con.drv)
  return(rs)
}

#' Read a full table from PostgreSQL database
#' @param tab.name a valid table 
#' @param con.drv a list containing a non-expired connection to and driver for PgSQL database
#' @details Runs SQL query against and returns result
#' @import RPostgreSQL
#' @examples \dontrun{
#' ## make a connection
#' c <- pgCon()  #running without argumetns requires pg.auth list set as global option
#' mytable.df<-pgQ("mytable", c)
#' pgDiscon(c)
#' }
#' @export

pgTab<-function(tab.name, con.drv=NULL){
  query = paste("select * from", tab.name)
  rs = pgQry(query, con.drv)
  return(rs)
}
