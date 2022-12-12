make_more_preds <- function(df, z, ...) {
  # Helper function
  make_prediction(df, "2008-19", 2008, 2019, z, ...) %>%
    bind_rows(make_prediction(df, "2009-19", 2009, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2010-19", 2010, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2011-19", 2011, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2012-19", 2012, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2013-19", 2013, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2014-19", 2014, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2015-19", 2015, 2019, z, ...)) %>%
    bind_rows(make_prediction(df, "2016-19", 2016, 2019, z, ...)) 
  
}