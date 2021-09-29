DATA lv_extension TYPE sood-file_ext.
CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
  EXPORTING
    filename  = iv_filename
  IMPORTING
    extension = lv_extension.