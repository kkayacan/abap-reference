  CALL FUNCTION 'RH_GET_PLVAR'
    EXPORTING
      no_message = abap_true
    IMPORTING
      plvar      = lv_plvar
    EXCEPTIONS
      no_plvar   = 1
      OTHERS     = 2.