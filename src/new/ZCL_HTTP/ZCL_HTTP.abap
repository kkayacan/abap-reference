class ZCL_HTTP definition
  public
  create public .

public section.

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
      !IT_FORM type WDY_KEY_VALUE_LIST optional
    returning
      value(RESPONSE) type STRING
    raising
      ZCX_HTTP .
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
* | [--->] IT_FORM                        TYPE        WDY_KEY_VALUE_LIST(optional)
* | [<-()] RESPONSE                       TYPE        STRING
* | [!CX!] ZCX_HTTP
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD request.

  DATA: lo_client         TYPE REF TO if_http_client,
        lv_exc            TYPE string,
        lv_content_length TYPE i.

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
    RAISE EXCEPTION TYPE zcx_http
      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH 'create_by_url' 'ERROR' '' ''.
  ENDIF.

  IF ip_username IS NOT INITIAL AND ip_password IS NOT INITIAL.
    lo_client->authenticate( username = ip_username
                             password = ip_password ).
  ENDIF.

  lo_client->request->set_method( ip_method ).

  IF ip_token IS NOT INITIAL.
    lo_client->request->set_header_field( name  = 'Authorization'
                                          value = `Bearer ` && ip_token ).
  ENDIF.
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
  IF ip_body IS NOT INITIAL.
    lo_client->request->set_content_type( content_type = if_rest_media_type=>gc_appl_json ).
    lo_client->request->set_cdata( ip_body ).
    lv_content_length = strlen( ip_body ).
  ELSEIF it_form IS NOT INITIAL.
    lo_client->request->set_header_field( name  = 'content-type'
                                          value = 'application/x-www-form-urlencoded' ).
    LOOP AT it_form ASSIGNING FIELD-SYMBOL(<ls_form>).
      lv_content_length = lv_content_length + strlen( <ls_form>-key ).
      lv_content_length = lv_content_length + strlen( <ls_form>-value ).
      lo_client->request->set_form_field( name  = <ls_form>-key
                                          value = <ls_form>-value ).
    ENDLOOP.
    lv_content_length = lv_content_length + lines( it_form ) * 2 - 1.
  ENDIF.

  lo_client->request->set_header_field( name  = 'Content-Length'
                                        value = condense( CONV string( lv_content_length ) ) ).

  lo_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_http
      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH 'send' 'ERROR' '' ''.
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
    DATA lv_message_class  TYPE arbgb.
    DATA lv_message_number TYPE msgnr.
    lo_client->get_last_error(
      IMPORTING
        code           = lv_code
        message        = lv_message
        message_class  = lv_message_class
        message_number = lv_message_number ).
    DATA(ls_msg) = zcl_string=>to_bapiret2( lv_message ).
    RAISE EXCEPTION TYPE zcx_http
      MESSAGE ID '00' TYPE 'E' NUMBER 398 WITH ls_msg-message_v1 ls_msg-message_v2
      ls_msg-message_v3 ls_msg-message_v4.
  ENDIF.

*  DATA(lv_xstring) = lo_client->response->get_data( ).
  response = lo_client->response->get_cdata( ).

  lo_client->close( ).

ENDMETHOD.
ENDCLASS.