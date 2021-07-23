DATA(lv_seconds) = CONV i( 0 ).
TRY.
    CALL METHOD cl_abap_tstmp=>td_subtract
      EXPORTING
        date1    = ls_list-aedat
        time1    = ls_list-aezet
        date2    = ls_list-erdat
        time2    = ls_list-erzet
      IMPORTING
        res_secs = lv_seconds.
  CATCH cx_parameter_invalid_type .
  CATCH cx_parameter_invalid_range .
ENDTRY.
IF lv_seconds IS NOT INITIAL.
  DATA(lv_hours) = CONV i( lv_seconds DIV 3600 ).
  DATA(lv_minutes) = CONV i( ( lv_seconds MOD 3600 ) / 60 ).
  ls_list-turnover = CONV string( lv_hours ) && ':' && |{ CONV char2( lv_minutes ) ALPHA = IN }|.
  CONDENSE ls_list-turnover NO-GAPS.
ENDIF.