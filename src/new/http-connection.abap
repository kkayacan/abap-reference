DATA lv_url    TYPE string VALUE 'https://socialize.eu1.gigya.com/socialize.getToken'.
DATA lo_client TYPE REF TO if_http_client.

cl_http_client=>create_by_url(
  EXPORTING
    url                    = lv_url
  IMPORTING
    client                 = lo_client
  EXCEPTIONS
    argument_not_found     = 1
    plugin_not_active      = 2
    internal_error         = 3
    pse_not_found          = 4
    pse_not_distrib        = 5
    pse_errors             = 6
    OTHERS                 = 7 ).
IF sy-subrc <> 0.
*   Implement suitable error handling here
ENDIF.

lo_client->request->set_method( if_http_request=>co_request_method_post ).

lo_client->request->set_header_field( name  = 'User-Agent'
                                      value = 'SAP NetWeaver Application Server (1.0;755)' ).
lo_client->request->set_header_field( name  = 'Accept'
                                      value = '*/*' ).
lo_client->request->set_header_field( name  = 'Host'
                                      value = 'socialize.eu1.gigya.com' ).
lo_client->request->set_header_field( name  = 'Accept-Encoding'
                                      value = 'gzip, deflate, br' ).
lo_client->request->set_header_field( name  = 'Connection'
                                      value = 'keep-alive' ).
lo_client->request->set_header_field( name  = 'content-type'
                                      value = 'application/x-www-form-urlencoded' ).
lo_client->request->set_header_field( name  = 'Content-Length'
                                      value = '153' ).
lo_client->request->set_form_field( name  = 'grant_type'
                                    value = 'none' ).
lo_client->request->set_form_field( name  = 'client_id'
                                    value = 'YoURJvFr4kia2U6iOYl6H1IPgkbYzwjGNeHVrFPRRZ7IHEMw84iJ7qnlxujexxVfjb' ).
lo_client->request->set_form_field( name  = 'userKey'
                                    value = 'uqmMnsVwGLMK' ).
lo_client->request->set_form_field( name  = 'secret'
                                    value = 'Cqps9EFJUPYwvKAKkk8oqMayTzq4cyyc' ).
lo_client->send(
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4
    OTHERS                     = 5 ).
IF sy-subrc <> 0.
*   Implement suitable error handling here
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
ENDIF.

DATA(lv_xstring) = lo_client->response->get_data( ).
DATA(lv_string) = lo_client->response->get_cdata( ).

lo_client->close( ).