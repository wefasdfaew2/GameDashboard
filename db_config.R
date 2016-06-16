## database connection
src <- src_sqlserver(server      = 'gamedatabase.cj7m6czqmasu.us-west-2.rds.amazonaws.com',
                     port        = 1433,
                     database    = 'encounterdb',
                     properties  = list(
                       user     = 'gameadmin',
                       password = 'ndowadmin'
                     ))