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
*"----------------------------------------------------------------------

  DATA: ltr_werks TYPE RANGE OF mard-werks,
        ltr_lgort TYPE RANGE OF mard-lgort,
        ltr_matkl TYPE RANGE OF mara-matkl.
  DATA lo_result TYPE REF TO data.
  FIELD-SYMBOLS <lt_result> TYPE ANY TABLE.

  ltr_werks = VALUE #( ( sign = 'I' option = 'EQ' low = '1001' ) ).
  ltr_lgort = VALUE #( ( sign = 'I' option = 'EQ' low = '1001' ) ).
  ltr_matkl = VALUE #( ( sign = 'I' option = 'EQ' low = '100000' ) ).

  cl_salv_bs_runtime_info=>set(
      display        = abap_false
      metadata       = abap_false
      data           = abap_true ).

  SUBMIT rm07mlbs
    WITH werks  IN ltr_werks
    WITH lgort  IN ltr_lgort
    WITH matkla IN ltr_matkl
    WITH pa_hsq EQ abap_false
    WITH pa_flt EQ abap_true
    AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data            = lo_result ).
    CATCH cx_salv_bs_sc_runtime_info .
  ENDTRY.
  ASSIGN lo_result->* TO <lt_result>.

  LOOP AT <lt_result> ASSIGNING FIELD-SYMBOL(<ls_result>).
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