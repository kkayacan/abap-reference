FORM excel.

    PERFORM print USING abap_true.
  
    DATA: lo_excel     TYPE REF TO zcl_excel,
          lo_worksheet TYPE REF TO zcl_excel_worksheet,
          lv_val       TYPE string,
          lv_row       TYPE i.
  
    CREATE OBJECT lo_excel.
    lo_worksheet = lo_excel->get_active_worksheet( ).
  
    lo_worksheet->get_column( ip_column = 'A' )->set_width( ip_width = '13' ).
    lo_worksheet->get_column( ip_column = 'B' )->set_width( ip_width = '4.29' ).
    lo_worksheet->get_column( ip_column = 'C' )->set_width( ip_width = '17.71' ).
    lo_worksheet->get_column( ip_column = 'D' )->set_width( ip_width = '4.43' ).
    lo_worksheet->get_column( ip_column = 'E' )->set_width( ip_width = '9.57' ).
    lo_worksheet->get_column( ip_column = 'F' )->set_width( ip_width = '19.14' ).
    lo_worksheet->get_column( ip_column = 'G' )->set_width( ip_width = '11.86' ).
    lo_worksheet->get_column( ip_column = 'H' )->set_width( ip_width = '53.29' ).
    lo_worksheet->get_column( ip_column = 'I' )->set_width( ip_width = '8.86' ).
    lo_worksheet->get_column( ip_column = 'I' )->set_width( ip_width = '8.86' ).
    lo_worksheet->get_column( ip_column = 'J' )->set_width( ip_width = '33.86' ).
    lo_worksheet->get_column( ip_column = 'K' )->set_width( ip_width = '9.29' ).
    lo_worksheet->get_column( ip_column = 'L' )->set_width( ip_width = '7.29' ).
    lo_worksheet->get_column( ip_column = 'M' )->set_width( ip_width = '5.29' ).
    lo_worksheet->get_column( ip_column = 'N' )->set_width( ip_width = '15.43' ).
    lo_worksheet->get_column( ip_column = 'O' )->set_width( ip_width = '17.29' ).
    lo_worksheet->get_column( ip_column = 'P' )->set_width( ip_width = '10.71' ).
    lo_worksheet->get_column( ip_column = 'Q' )->set_width( ip_width = '8' ).
    lo_worksheet->get_column( ip_column = 'R' )->set_width( ip_width = '10.57' ).
    lo_worksheet->get_column( ip_column = 'S' )->set_width( ip_width = '11.43' ).
    lo_worksheet->get_column( ip_column = 'T' )->set_width( ip_width = '4.43' ).
    lo_worksheet->get_column( ip_column = 'U' )->set_width( ip_width = '10' ).
  
    ADD 1 TO lv_row.
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'A' ip_value = TEXT-c01 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'B' ip_value = TEXT-c02 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'C' ip_value = TEXT-c03 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'D' ip_value = TEXT-c04 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'E' ip_value = TEXT-c05 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'F' ip_value = TEXT-c06 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'G' ip_value = TEXT-c07 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'H' ip_value = TEXT-c08 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'I' ip_value = TEXT-c09 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'J' ip_value = TEXT-c10 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'K' ip_value = TEXT-c11 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'L' ip_value = TEXT-c12 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'M' ip_value = TEXT-c13 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'N' ip_value = TEXT-c14 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'O' ip_value = TEXT-c15 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'P' ip_value = TEXT-c16 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'Q' ip_value = TEXT-c17 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'R' ip_value = TEXT-c18 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'S' ip_value = TEXT-c19 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'T' ip_value = TEXT-c20 ).
    lo_worksheet->set_cell( ip_row = lv_row ip_column = 'U' ip_value = TEXT-c21 ).
  
    LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
      LOOP AT <ls_excel>-item ASSIGNING FIELD-SYMBOL(<ls_item>).
        ADD 1 TO lv_row.
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'A' ip_value = <ls_excel>-header-iblnr ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'B' ip_value = <ls_excel>-header-gjahr ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'C' ip_value = <ls_excel>-header-gidat ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'D' ip_value = <ls_item>-zeili ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'E' ip_value = <ls_item>-matnr ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'F' ip_value = <ls_item>-maktx ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'G' ip_value = <ls_item>-atwrt ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'H' ip_value = <ls_item>-kmlmt ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'I' ip_value = <ls_item>-kunnr ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'J' ip_value = <ls_item>-name1 ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'K' ip_value = <ls_item>-kdauf ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'L' ip_value = <ls_item>-kdpos ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'M' ip_value = <ls_item>-lgort ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'N' ip_value = <ls_item>-lgobe ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'O' ip_value = <ls_item>-btext ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'P' ip_value = <ls_item>-charg ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'Q' ip_value = <ls_item>-atflv ).
  *      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'R' ip_value = space ).
  *      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'S' ip_value = space ).
        lo_worksheet->set_cell( ip_row = lv_row ip_column = 'T' ip_value = <ls_item>-meins ).
  *      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'U' ip_value = space ).
      ENDLOOP.
    ENDLOOP.
  
    PERFORM download_xls USING lo_excel.
  
  ENDFORM.
  
  FORM download_xls USING io_excel TYPE REF TO zcl_excel.
    DATA: cl_writer   TYPE REF TO zif_excel_writer,
          xdata       TYPE xstring,
          t_rawdata   TYPE solix_tab,
          bytecount   TYPE i,
          lv_fullpath TYPE string.
    CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
    xdata = cl_writer->write_file( io_excel ).
    t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring = xdata ).
    bytecount = xstrlen( xdata ).
    PERFORM file_save_dialog CHANGING lv_fullpath.
    IF lv_fullpath IS INITIAL.
      RETURN.
    ENDIF.
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                      filename     = lv_fullpath
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = t_rawdata ).
  ENDFORM.
  
  FORM file_save_dialog CHANGING VALUE(rv_file_path) TYPE string.
  
    DATA: lv_filename TYPE string,
          lv_path     TYPE string,
          lv_sep      TYPE c LENGTH 1.
  
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = rv_file_path.
    cl_gui_cfw=>update_view( ).
  
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = lv_sep
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        others               = 4 ).
  
    CONCATENATE rv_file_path lv_sep sy-datum sy-uzeit '.xlsx' INTO rv_file_path.
  
    DATA: lt_file_table TYPE STANDARD TABLE OF string.
  
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension = 'XLSX'
        default_file_name = rv_file_path
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = rv_file_path.
  
  ENDFORM.