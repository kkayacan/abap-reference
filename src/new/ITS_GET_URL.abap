DATA: lv_url TYPE char1250.
CALL FUNCTION 'ITS_GET_URL'
  IMPORTING
    url               = lv_url
  EXCEPTIONS
    its_not_available = 1
    OTHERS            = 2.
IF sy-subrc <> 0.
*     Implement suitable error handling here
ENDIF.
IF lv_url IS INITIAL.
  gv_gui = 'X'.
ELSE.
  gv_gui = space.
ENDIF.
