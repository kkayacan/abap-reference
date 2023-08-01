DATA: lv_binary_json TYPE xstring,
lo_zip         TYPE REF TO cl_abap_zip.
CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
EXPORTING
text   = content
IMPORTING
buffer = lv_binary_json
EXCEPTIONS
failed = 1
OTHERS = 2.
CREATE OBJECT lo_zip.
lo_zip->add( name    = 'request.json'
       content = lv_binary_json ).
zip = lo_zip->save( ).