CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
EXPORTING
  i_line_header        = abap_true
  i_tab_raw_data       = lt_raw
  i_filename           = lv_file
TABLES
  i_tab_converted_data = it_excel2
EXCEPTIONS
  conversion_failed    = 1
  OTHERS               = 2.