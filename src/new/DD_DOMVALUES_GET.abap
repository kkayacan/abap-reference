FUNCTION z_get_domvalues.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOMNAME) TYPE  DD07L-DOMNAME
*"  EXPORTING
*"     VALUE(ET_DOMVAL) TYPE  DD07VTAB
*"----------------------------------------------------------------------

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = iv_domname
      text           = 'X'
      langu          = sy-langu
      bypass_buffer  = abap_true
    TABLES
      dd07v_tab      = et_domval
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

ENDFUNCTION.