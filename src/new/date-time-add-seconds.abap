cl_abap_tstmp=>td_add(
    EXPORTING
      date     = sy-datum
      time     = sy-uzeit
      secs     = 82800
    IMPORTING
      res_date = ls_db-valid_until_date
      res_time = ls_db-valid_until_time ).