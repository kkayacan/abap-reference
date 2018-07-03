  DATA: lt_t247 TYPE TABLE OF t247.

  CALL FUNCTION 'MONTH_NAMES_GET'
    TABLES
      month_names           = lt_t247
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.

    TRY.
        head-month_name = lt_t247[ mnr = head-monat ]-ltx.
      CATCH cx_root.
    ENDTRY.