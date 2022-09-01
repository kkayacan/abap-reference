REPORT zfiori_launch_mb90.

START-OF-SELECTION.

  DATA gt_context TYPE STANDARD TABLE OF savwctxt.
  CALL FUNCTION 'ITS_IMPORT_CONTEXT'
    TABLES
      context           = gt_context
    EXCEPTIONS
      its_not_available = 1
      OTHERS            = 2.

  IF gt_context IS INITIAL.
    LEAVE PROGRAM.
  ENDIF.

  READ TABLE gt_context INTO DATA(gs_context) WITH KEY fieldname = '~TRANSACTION'.

  DATA: gv_mblnr TYPE mkpf-mblnr,
        gv_mjahr TYPE mkpf-mjahr.

  FIND 'MATERIALDOCUMENT=' IN gs_context-fieldcont MATCH OFFSET DATA(gv_offset).
  IF gv_offset > 0.
    ADD 17 TO gv_offset.
    gv_mblnr = gs_context-fieldcont+gv_offset(10).
  ENDIF.
  CLEAR gv_offset.
  FIND 'MATERIALDOCUMENTYEAR=' IN gs_context-fieldcont MATCH OFFSET gv_offset.
  IF gv_offset > 0.
    ADD 21 TO gv_offset.
    gv_mjahr = gs_context-fieldcont+gv_offset(4).
  ENDIF.

  DATA gv_objky TYPE nast-objky.
  gv_objky = gv_mblnr && gv_mjahr && '%'.
  SELECT SINGLE '1' FROM nast
    WHERE kappl = 'ME'
      AND objky LIKE @gv_objky
    INTO @DATA(gv_vermo).
  IF sy-subrc <> 0.
    gv_vermo = '2'.
  ENDIF.

end-of-SELECTION.

  DATA gtr_mblnr TYPE RANGE OF mkpf-mblnr.

  gtr_mblnr = VALUE #( ( sign = 'I' option = 'EQ' low = gv_mblnr ) ).

  SUBMIT mm70amea
  WITH pm_vermo = gv_vermo
  WITH pm_mjahr = gv_mjahr
  WITH rg_mblnr IN gtr_mblnr.