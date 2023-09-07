DATA lt_wwgna TYPE TABLE OF ddshretval.
CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
  EXPORTING
    tabname             = ''
    fieldname           = ''
    searchhelp          = 'ZRT_SH_WWGNA'
    suppress_recordlist = 'X'
  TABLES
    return_tab          = lt_wwgna
  EXCEPTIONS
    field_not_found     = 1
    no_help_for_field   = 2
    inconsistent_help   = 3
    no_values_found     = 4
    OTHERS              = 5.