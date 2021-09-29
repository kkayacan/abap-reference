FUNCTION z_upload_attachment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJECT_ID) TYPE  SIBFBORIID
*"     VALUE(IV_FILENAME) TYPE  SO_OBJ_DES
*"     VALUE(IV_VALUE) TYPE  XSTRING
*"  EXPORTING
*"     VALUE(ES_ATTACHMENT) TYPE  ZST_ATTACHMENT
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA ls_folder_id TYPE soodk.
  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region                = 'B'
    IMPORTING
      folder_id             = ls_folder_id
    EXCEPTIONS
      communication_failure = 1
      owner_not_exist       = 2
      system_failure        = 3
      x_error               = 4
      OTHERS                = 5.

  DATA lv_extension TYPE sood-file_ext.
  CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
    EXPORTING
      filename  = iv_filename
    IMPORTING
      extension = lv_extension.

  DATA: lv_output_length TYPE i,
        lt_solix         TYPE solix_tab.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = iv_value
    IMPORTING
      output_length = lv_output_length
    TABLES
      binary_tab    = lt_solix.

  DATA lt_soli  TYPE soli_tab.
  CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
    EXPORTING
      ip_solixtab = lt_solix
    IMPORTING
      ep_solitab  = lt_soli.

  DATA: ls_object_hd_change TYPE sood1,
        lt_objhead          TYPE STANDARD TABLE OF soli.

  ls_object_hd_change-objla = sy-langu.
  ls_object_hd_change-objnam = 'MESSAGE'.
  ls_object_hd_change-objdes = iv_filename.
  ls_object_hd_change-file_ext = lv_extension.
  ls_object_hd_change-objlen = lv_output_length.

  DATA lv_mimetype TYPE mimetypes-type.
  CALL FUNCTION 'SDOK_MIMETYPE_GET'
    EXPORTING
      extension = lv_extension
    IMPORTING
      mimetype  = lv_mimetype.

  lt_objhead = VALUE #( ( line = '&SO_FILENAME=' && iv_filename )
                        ( line = '&SO_FORMAT=BIN' )
                        ( line = '&SO_CONTTYPE=' && lv_mimetype ) ).

  DATA ls_object_id TYPE soodk.
  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = ls_folder_id
      object_hd_change           = ls_object_hd_change
      object_type                = 'EXT'
    IMPORTING
      object_id                  = ls_object_id
    TABLES
      objcont                    = lt_soli
      objhead                    = lt_objhead
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_exist              = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      object_type_not_exist      = 7
      operation_no_authorization = 8
      owner_not_exist            = 9
      parameter_error            = 10
      substitute_not_active      = 11
      substitute_not_defined     = 12
      system_failure             = 13
      x_error                    = 14
      OTHERS                     = 15.
  IF sy-subrc <> 0.
    es_return-type   = 'E'.
    es_return-id     = sy-msgid.
    es_return-number = sy-msgno.
    es_return-message_v1 = sy-msgv1.
    es_return-message_v2 = sy-msgv2.
    es_return-message_v3 = sy-msgv3.
    es_return-message_v4 = sy-msgv4.
    IF es_return-id IS INITIAL.
      es_return-id     = 'SV'.
      es_return-number = '171'.
    ENDIF.
    RETURN.
  ENDIF.

  es_attachment-object_type = 'BUS2012'.
  es_attachment-object_cat  = 'BO'.
  es_attachment-document_id = ls_folder_id-objtp && ls_folder_id-objyr && ls_folder_id-objno &&
                              ls_object_id-objtp && ls_object_id-objyr && ls_object_id-objno.
  es_attachment-file_name   = iv_filename.
  es_attachment-mime_type   = lv_mimetype.


  DATA: ls_object_a TYPE sibflporb,
        ls_object_b TYPE sibflporb.

  ls_object_a-instid = iv_object_id.
  ls_object_a-typeid = 'BUS2012'.
  ls_object_a-catid  = 'BO'.
  ls_object_b-instid  = es_attachment-document_id.
  ls_object_b-typeid = 'MESSAGE'.
  ls_object_b-catid  = 'BO'.

  TRY.
      cl_binary_relation=>create_link(
        EXPORTING
          is_object_a = ls_object_a
          is_object_b = ls_object_b
          ip_reltype  = 'ATTA' ).
    CATCH cx_obl_parameter_error.
    CATCH cx_obl_model_error.
    CATCH cx_obl_internal_error.
  ENDTRY.

ENDFUNCTION.

FUNCTION z_get_attachments.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJECT_ID) TYPE  SIBFBORIID
*"  EXPORTING
*"     VALUE(ET_ATTACHMENT) TYPE  ZTT_ATTACHMENT
*"----------------------------------------------------------------------

  TRY.
      cl_binary_relation=>read_links_of_objects(
        EXPORTING
          it_objects = VALUE #( ( instid = iv_object_id typeid = 'BUS2012' catid = 'BO' ) )
        IMPORTING
          et_links_a = DATA(lt_links) ).
    CATCH cx_obl_model_error.
    CATCH cx_obl_parameter_error.
    CATCH cx_obl_internal_error.
  ENDTRY.

  LOOP AT lt_links ASSIGNING FIELD-SYMBOL(<ls_link>).
    SELECT SINGLE * FROM sood
      WHERE objtp = @<ls_link>-instid_b+17(3)
        AND objyr = @<ls_link>-instid_b+20(2)
        AND objno = @<ls_link>-instid_b+22
      INTO @DATA(ls_sood).
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO et_attachment ASSIGNING FIELD-SYMBOL(<ls_attach>).
      <ls_attach>-object_id    = <ls_link>-instid_a.
      <ls_attach>-object_type  = <ls_link>-typeid_a.
      <ls_attach>-object_cat   = <ls_link>-catid_a.
      <ls_attach>-document_id  = <ls_link>-instid_b.
      <ls_attach>-file_name    = ls_sood-objdes.
      <ls_attach>-creator      = ls_sood-crono.
      <ls_attach>-creator_name = ls_sood-cronam.
      <ls_attach>-created_on   = <ls_link>-utctime.
      DATA lv_mimetype TYPE mimetypes-type.
      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING
          extension = ls_sood-file_ext
        IMPORTING
          mimetype  = lv_mimetype.
      <ls_attach>-mime_type    = lv_mimetype.
      CLEAR lv_mimetype.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.

FUNCTION z_download_attachment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOCUMENT_ID) TYPE  DOCUMENTID
*"  EXPORTING
*"     VALUE(EV_VALUE) TYPE  XSTRING
*"     VALUE(EV_MIME_TYPE) TYPE  STRING
*"     VALUE(EV_FILENAME) TYPE  SO_OBJ_DES
*"----------------------------------------------------------------------

  DATA: ls_document_data TYPE sofolenti1,
        lt_content       TYPE STANDARD TABLE OF solix.

  CALL FUNCTION 'SO_DOCUMENT_READ_API1'
    EXPORTING
      document_id                = CONV sofolenti1-doc_id( iv_document_id )
    IMPORTING
      document_data              = ls_document_data
    TABLES
      contents_hex               = lt_content
    EXCEPTIONS
      document_id_not_exist      = 1
      operation_no_authorization = 2
      x_error                    = 3
      OTHERS                     = 4.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = CONV i( ls_document_data-doc_size )
    IMPORTING
      buffer       = ev_value
    TABLES
      binary_tab   = lt_content
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  DATA lv_mimetype TYPE mimetypes-type.
  CALL FUNCTION 'SDOK_MIMETYPE_GET'
    EXPORTING
      extension = ls_document_data-obj_type
    IMPORTING
      mimetype  = lv_mimetype.
  ev_mime_type = lv_mimetype.
  ev_filename = ls_document_data-obj_descr.

ENDFUNCTION.