*&---------------------------------------------------------------------*
*& Report  ZFI_TCMB_INFLATION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_tcmb_inflation.

DATA: gt_inf  TYPE TABLE OF zrt_inf_rate,
      go_salv TYPE REF TO cl_salv_table,
      go_col  TYPE REF TO cl_salv_column_table.

PARAMETERS: p_start TYPE datum,
            p_end   TYPE datum,
            p_upd   AS CHECKBOX DEFAULT 'X'.

INITIALIZATION.
  p_start = sy-datum - 30.
  p_end   = sy-datum.

START-OF-SELECTION.

  gt_inf = zcl_tcmb_inflation=>get(
      iv_start_date = p_start
      iv_end_date   = p_end
      iv_update_db  = p_upd ).

END-OF-SELECTION.
  IF sy-batch = abap_false.
    CALL METHOD cl_salv_table=>factory
      IMPORTING
        r_salv_table = go_salv
      CHANGING
        t_table      = gt_inf.

    go_salv->get_functions( )->set_all( abap_true ).
    go_salv->get_columns( )->set_optimize( abap_true ).
*  go_salv->get_columns( )->set_key_fixation( value = abap_true ).
*  go_salv->get_columns( )->set_column_position( columnname = 'MANDT'
*                                               position   = 1 ).
    go_col ?= go_salv->get_columns( )->get_column( 'MANDT' ).
    go_col->set_visible( abap_false ).
    go_salv->display( ).
  ENDIF.