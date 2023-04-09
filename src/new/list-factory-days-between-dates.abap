CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
EXPORTING
  i_datab               = <ls_data>-badat_pr
  i_datbi               = <ls_cd_pr>-udate
  i_factid              = 'TR'
TABLES
  eth_dats              = lt_days
EXCEPTIONS
  date_conversion_error = 1
  OTHERS                = 2.