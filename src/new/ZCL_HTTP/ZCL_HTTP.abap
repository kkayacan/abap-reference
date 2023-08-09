class ZCL_HTTP definition
  public
  create public .

public section.

  class-data MESSAGE type STRING read-only .
  class-data LONG_MESSAGE type STRING read-only .

  type-pools ABAP .
*    raising
*      ZCX_HTTP
  class-methods REQUEST
    importing
      !IP_PROTOCOL type STRING default 'https'
      !IP_HOST type STRING
      !IP_ENDPOINT type STRING
      !IP_USERNAME type STRING optional
      !IP_PASSWORD type STRING optional
      !IP_PROXY_HOST type STRING optional
      !IP_PROXY_PORT type STRING optional
      !IP_METHOD type STRING default IF_HTTP_REQUEST=>CO_REQUEST_METHOD_POST
      !IP_TOKEN type STRING optional
      !IP_BODY type STRING optional
      !IP_SEND_LENGTH type ABAP_BOOL default ABAP_FALSE
      !IT_FORM type WDY_KEY_VALUE_LIST optional
      !IP_MULTIPART type ABAP_BOOL default ABAP_FALSE
      !IP_FILE type XSTRING optional
      !IP_FILE_NAME type STRING optional
    returning
      value(RESPONSE) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_HTTP=>REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PROTOCOL                    TYPE        STRING (default ='https')
* | [--->] IP_HOST                        TYPE        STRING
* | [--->] IP_ENDPOINT                    TYPE        STRING
* | [--->] IP_USERNAME                    TYPE        STRING(optional)
* | [--->] IP_PASSWORD                    TYPE        STRING(optional)
* | [--->] IP_PROXY_HOST                  TYPE        STRING(optional)
* | [--->] IP_PROXY_PORT                  TYPE        STRING(optional)
* | [--->] IP_METHOD                      TYPE        STRING (default =IF_HTTP_REQUEST=>CO_REQUEST_METHOD_POST)
* | [--->] IP_TOKEN                       TYPE        STRING(optional)
* | [--->] IP_BODY                        TYPE        STRING(optional)
* | [--->] IP_SEND_LENGTH                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IT_FORM                        TYPE        WDY_KEY_VALUE_LIST(optional)
* | [--->] IP_MULTIPART                   TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IP_FILE                        TYPE        XSTRING(optional)
* | [--->] IP_FILE_NAME                   TYPE        STRING(optional)
* | [<-()] RESPONSE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD request.

  DATA: lo_client         TYPE REF TO if_http_client,
        lv_exc            TYPE string,
        lv_content_length TYPE i.

  CLEAR message.

  cl_http_client=>create_by_url(
    EXPORTING
      url                    = ip_protocol && `://` && ip_host && ip_endpoint
      proxy_host             = ip_proxy_host
      proxy_service          = ip_proxy_port
    IMPORTING
      client                 = lo_client
    EXCEPTIONS
      argument_not_found     = 1
      plugin_not_active      = 2
      internal_error         = 3
*      pse_not_found          = 4
*      pse_not_distrib        = 5
*      pse_errors             = 6
      OTHERS                 = 7 ).
  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_http
*      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH 'create_by_url' 'ERROR' '' ''.
    MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH 'create_by_url' 'ERROR' '' '' INTO message.
    RETURN.
  ENDIF.

  IF ip_username IS NOT INITIAL AND ip_password IS NOT INITIAL.
    lo_client->authenticate( username = ip_username
                             password = ip_password ).
  ENDIF.

  IF ip_token IS NOT INITIAL.
    lo_client->request->set_header_field( name  = 'Authorization'
                                          value = `Bearer ` && ip_token ).
  ENDIF.

  IF ip_file IS INITIAL.
    lo_client->request->set_method( ip_method ).

    lo_client->request->set_header_field( name  = 'User-Agent'
                                          value = 'SAP NetWeaver Application Server (1.0;' && sy-saprl && ')' ).
    lo_client->request->set_header_field( name  = 'Accept'
                                          value = '*/*' ).
    lo_client->request->set_header_field( name  = 'Host'
                                          value = ip_host ).
    lo_client->request->set_header_field( name  = 'Accept-Encoding'
                                          value = 'gzip, deflate, br' ).
    lo_client->request->set_header_field( name  = 'Connection'
                                          value = 'keep-alive' ).
  ENDIF.

  FIELD-SYMBOLS <ls_form> LIKE LINE OF it_form.

  IF ip_body IS NOT INITIAL.
    lo_client->request->set_content_type( content_type = if_rest_media_type=>gc_appl_json ).
    lo_client->request->set_cdata( ip_body ).
    lv_content_length = strlen( ip_body ).
  ELSEIF it_form IS NOT INITIAL AND ip_multipart = abap_false.
    lo_client->request->set_content_type( content_type = if_rest_media_type=>gc_appl_www_form_url_encoded ).
    LOOP AT it_form ASSIGNING <ls_form>.
      lv_content_length = lv_content_length + strlen( <ls_form>-key ).
      lv_content_length = lv_content_length + strlen( <ls_form>-value ).
      lo_client->request->set_form_field( name  = <ls_form>-key
                                          value = <ls_form>-value ).
    ENDLOOP.
    lv_content_length = lv_content_length + lines( it_form ) * 2 - 1.
  ELSEIF ip_file IS NOT INITIAL.
    lo_client->propertytype_logon_popup = 0.
    DATA: lo_rest_client    TYPE REF TO cl_rest_http_client,
          lo_upload_request TYPE REF TO if_rest_entity,
          lo_form_data      TYPE REF TO cl_rest_multipart_form_data.
    CREATE OBJECT lo_rest_client
      EXPORTING
        io_http_client = lo_client.
    cl_http_utility=>set_request_uri( request = lo_client->request uri = ip_endpoint ).
    lo_upload_request = lo_rest_client->if_rest_client~create_request_entity( ).
    CREATE OBJECT lo_form_data.
    lo_form_data->set_file( iv_name = 'file' iv_filename = ip_file_name iv_type = 'application/octet-stream' iv_data = ip_file ).
    lo_form_data->write_to( lo_upload_request ).
  ELSE.
    lo_client->request->set_content_type( content_type = if_rest_media_type=>gc_all ).
  ENDIF.

  IF ip_send_length = abap_true.
    DATA lv_content_length_s TYPE string.
    lv_content_length_s = lv_content_length.
    CONDENSE lv_content_length_s.
    lo_client->request->set_header_field( name  = 'Content-Length'
                                          value = lv_content_length_s ).
  ENDIF.

  IF ip_file IS INITIAL.

    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_http
*      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH 'send' 'ERROR' '' ''.
      MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH 'send' 'ERROR' '' '' INTO message.
      RETURN.
    ENDIF.

    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      DATA lv_code           TYPE sysubrc.
      DATA lv_message        TYPE string.
*    DATA lv_message_class  TYPE arbgb.
*    DATA lv_message_number TYPE msgnr.
      lo_client->get_last_error(
        IMPORTING
          code           = lv_code
          message        = message
*        message_class  = lv_message_class
*        message_number = lv_message_number
          ).

*    DATA(ls_msg) = lcl_string=>to_bapiret2( lv_message ).
*    RAISE EXCEPTION TYPE zcx_http
*      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH ls_msg-message_v1 ls_msg-message_v2
*      ls_msg-message_v3 ls_msg-message_v4.
      RETURN.
    ENDIF.

    response = lo_client->response->get_cdata( ).

    lo_client->close( ).

  ELSE.
    DATA lx_rest_client TYPE REF TO cx_rest_client_exception.
    TRY.
        lo_rest_client->if_rest_client~post( lo_upload_request ).
      CATCH cx_rest_client_exception INTO lx_rest_client.
        message = lx_rest_client->get_text( ).
        long_message = lx_rest_client->get_longtext( ).
        RETURN.
    ENDTRY.
    DATA lv_status TYPE i.
    lv_status = lo_rest_client->if_rest_client~get_status( ).
    response = lo_rest_client->if_rest_client~get_response_entity( )->get_string_data( ).
    lo_rest_client->if_rest_client~close( ).
  ENDIF.

ENDMETHOD.
ENDCLASS.