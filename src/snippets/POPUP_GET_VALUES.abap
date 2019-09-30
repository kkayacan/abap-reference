FORM ask_for_values  CHANGING ep_doc_date
                              ep_ref_doc_no.

  DATA:
    lt_fields TYPE TABLE OF sval,
    ls_fields LIKE LINE OF lt_fields,
    lv_rc     TYPE c LENGTH 1.

  CLEAR: ep_doc_date, ep_ref_doc_no.

  ls_fields-tabname    = 'BKPF'.
  ls_fields-fieldname  = 'BUDAT'.
  ls_fields-fieldtext  = TEXT-t08.
  ls_fields-value = sy-datum.
  APPEND ls_fields TO lt_fields. CLEAR ls_fields.

  ls_fields-tabname    = 'BKPF'.
  ls_fields-fieldname  = 'XBLNR'.
  ls_fields-fieldtext  = TEXT-t09.
  ls_fields-novaluehlp = 'X'.
  APPEND ls_fields TO lt_fields. CLEAR ls_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-t07
    IMPORTING
      returncode      = lv_rc
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF lv_rc = 'A'.
    RETURN.
  ENDIF.

  READ TABLE lt_fields INTO ls_fields INDEX 1.
  IF sy-subrc = 0.
    ep_doc_date = ls_fields-value.
  ENDIF.

  READ TABLE lt_fields INTO ls_fields INDEX 2.
  IF sy-subrc = 0.
    ep_ref_doc_no = ls_fields-value.
  ENDIF.

ENDFORM.