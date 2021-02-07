PARAMETERS: p_fname TYPE ibipparms-path OBLIGATORY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM file_open_dialog CHANGING p_fname.

FORM file_open_dialog CHANGING cp_fname.

  DATA: lt_filetable TYPE filetable,
        lv_rc        TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = 'xlsx'
      file_filter             = '(*.xlsx)|*.xlsx|'
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

  TRY.
      cp_fname = lt_filetable[ 1 ]-filename.
    CATCH cx_root.
  ENDTRY.

ENDFORM.