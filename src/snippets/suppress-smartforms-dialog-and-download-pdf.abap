FORM print_pdf.

  DATA: fmname         TYPE rs38l_fnam,
        ls_output      TYPE ssfcrescl,
        lt_otf         TYPE TABLE OF itcoo,
        lv_size        TYPE i,
        lt_lines       TYPE TABLE OF tline,
        lv_filename    TYPE string,
        lv_path        TYPE string,
        lv_fullpath    TYPE string,
        lv_user_action TYPE i.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSMARTFORM_NAME'
    IMPORTING
      fm_name            = fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

    CALL FUNCTION fmname
      EXPORTING
        control_parameters = VALUE ssfctrlop( no_dialog = abap_true
                                              getotf    = abap_true )
        output_options     = VALUE ssfcompop( tddest = 'PDF1' )
        user_settings      = abap_false
        lt_table           = <ls_carsaf>-item
        ls_header          = <ls_carsaf>-head
      IMPORTING
        job_output_info    = ls_output
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    APPEND LINES OF ls_output-otfdata TO lt_otf.

  ENDLOOP.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_size
    TABLES
      otf                   = lt_otf
      lines                 = lt_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.

  ENDIF.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension         = 'PDF'
      default_file_name         = 'CombinedOutput'
    CHANGING
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
      user_action               = lv_user_action
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.
  IF sy-subrc <> 0 OR
     lv_user_action = cl_gui_frontend_services=>action_cancel.
    RETURN.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_size
      filename                = lv_fullpath
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_lines
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.