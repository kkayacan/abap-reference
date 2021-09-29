DATA lv_mimetype TYPE mimetypes-type.
CALL FUNCTION 'SDOK_MIMETYPE_GET'
  EXPORTING
    extension = ls_document_data-obj_type
  IMPORTING
    mimetype  = lv_mimetype.