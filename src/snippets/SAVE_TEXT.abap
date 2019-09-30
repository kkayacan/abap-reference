DATA lt_lines  TYPE STANDARD TABLE OF tline.

LOOP AT lt_out_lines ASSIGNING FIELD-SYMBOL(<lv_line>).
  APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
  <ls_line>-tdformat = '*'.
  <ls_line>-tdline   = <lv_line>.
ENDLOOP.

CALL FUNCTION 'SAVE_TEXT'
  EXPORTING
    header          = VALUE thead( tdobject   = 'BELEG'
                                   tdname     = |{ ip_key+10(4) }{ ip_key(10) }{ ip_key+14(4) }|
                                   tdid       = '0002'
                                   tdspras    = sy-langu
                                   tdlinesize = '072'
                                   mandt      = sy-mandt )
    savemode_direct = abap_true
  TABLES
    lines           = lt_lines
  EXCEPTIONS
    id              = 1
    language        = 2
    name            = 3
    object          = 4
    OTHERS          = 5.
IF sy-subrc = 0.
  COMMIT WORK.
ENDIF.