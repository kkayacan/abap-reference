DATA: "lv_base64        TYPE string,
lv_decoded_data TYPE xstring,
lv_file_content TYPE xstring,
lt_bin          TYPE TABLE OF x255,
lv_len          TYPE i,
lv_xml_string   TYPE string.

" 1. Base64 çözme
CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
EXPORTING
  input  = lv_binary_data           " Base64 kodlu veri
IMPORTING
  output = lv_decoded_data.    " Çözülen veri (zipli hali)

" 2. CL_ABAP_ZIP sınıfı ile zip'i yükleme
DATA(lo_zip) = NEW cl_abap_zip( ).

TRY.
  " Zip içeriğini yükle
  lo_zip->load( lv_decoded_data ).

  " 3. Zip'ten XML dosyasını çıkar
  lo_zip->get( EXPORTING index = 1  " İlk dosya varsayılan alınır
               IMPORTING content = lv_file_content ).

  " 4. XSTRING'i BINARY'e dönüştürme
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_file_content
    IMPORTING
      output_length = lv_len
    TABLES
      binary_tab    = lt_bin.

  " 5. BINARY'den STRING'e dönüştürme
  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = lv_len
    IMPORTING
      text_buffer  = lv_xml_string
    TABLES
      binary_tab   = lt_bin
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc = 0.
    " Çözülen XML verisi burada kullanılabilir
    WRITE: / 'Çözülen XML Verisi:', lv_xml_string.
  ELSE.
    WRITE: / 'Dönüşüm hatası meydana geldi.'.
  ENDIF.

CATCH cx_root INTO DATA(lx_zip_error).
  WRITE: / 'Zip çıkarma hatası: ', lx_zip_error->get_text( ).
ENDTRY.