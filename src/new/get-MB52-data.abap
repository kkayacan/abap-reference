FUNCTION zrt_f_get_mb52_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ITR_WERKS) TYPE  TRG_CHAR4
*"     REFERENCE(ITR_LGORT) TYPE  TRG_CHAR4
*"     REFERENCE(ITR_MATKL) TYPE  WDFR_MATKL_RANG_TYP
*"  EXPORTING
*"     REFERENCE(EV_LABST) TYPE  MARD-LABST
*"     REFERENCE(EV_SALK3) TYPE  SALK3
*"     REFERENCE(ET_LIST) TYPE  ZRTT_MB52
*"----------------------------------------------------------------------

  DATA lo_result TYPE REF TO data.
  FIELD-SYMBOLS <lt_result> TYPE ANY TABLE.

  cl_salv_bs_runtime_info=>set(
      display        = abap_false
      metadata       = abap_false
      data           = abap_true ).

  SUBMIT rm07mlbs
    WITH werks  IN itr_werks
    WITH lgort  IN itr_lgort
    WITH matkla IN itr_matkl
    WITH nozero   EQ abap_true
    WITH novalues EQ abap_false
    WITH pa_hsq   EQ abap_false
    WITH pa_flt   EQ abap_true
    AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data            = lo_result ).
    CATCH cx_salv_bs_sc_runtime_info .
  ENDTRY.
  ASSIGN lo_result->* TO <lt_result>.
  IF <lt_result> IS NOT ASSIGNED.
    RETURN.
  ENDIF.
  LOOP AT <lt_result> ASSIGNING FIELD-SYMBOL(<ls_result>).
    APPEND INITIAL LINE TO et_list ASSIGNING FIELD-SYMBOL(<ls_list>).
    MOVE-CORRESPONDING <ls_result> to <ls_list>.
    ASSIGN COMPONENT 'LABST' OF STRUCTURE <ls_result> TO FIELD-SYMBOL(<lv_value>).
    IF sy-subrc = 0.
      ADD <lv_value> TO ev_labst.
    ENDIF.
    ASSIGN COMPONENT 'WLABS' OF STRUCTURE <ls_result> TO <lv_value>.
    IF sy-subrc = 0.
      ADD <lv_value> TO ev_salk3.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.