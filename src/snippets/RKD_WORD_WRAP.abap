DATA lv_text TYPE c LENGTH 1000.
DATA lt_out_lines TYPE TABLE OF tline-tdline.

CALL FUNCTION 'RKD_WORD_WRAP'
  EXPORTING
    textline            = lv_text
    outputlen           = 72
  TABLES
    out_lines           = lt_out_lines
  EXCEPTIONS
    outputlen_too_large = 1
    OTHERS              = 2.

DATA lt_lines  TYPE STANDARD TABLE OF tline.

LOOP AT lt_out_lines ASSIGNING FIELD-SYMBOL(<lv_line>).
  APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
  <ls_line>-tdformat = '*'.
  <ls_line>-tdline   = <lv_line>.
ENDLOOP.