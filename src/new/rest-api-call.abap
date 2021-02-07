REPORT y.


DATA lv_val TYPE string.

CALL METHOD cl_http_client=>create_by_url
  EXPORTING
    url                = 'http://185.22.184.249:3002/api/Ozsut/token'
  IMPORTING
    client             = DATA(o_post)
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4.

CALL METHOD o_post->request->set_method
  EXPORTING
    method = 'POST'.

CALL METHOD o_post->request->if_http_entity~set_content_type
  EXPORTING
    content_type = 'application/x-www-form-urlencoded'.

CALL METHOD o_post->request->set_header_field
  EXPORTING
    name  = 'Accept'
    value = 'application/json'.

CALL METHOD o_post->request->if_http_entity~set_form_field
  EXPORTING
    name  = 'grant_type'
    value = 'password'.

CALL METHOD o_post->request->if_http_entity~set_form_field
  EXPORTING
    name  = 'username'
    value = 'omerd'.

CALL METHOD o_post->request->if_http_entity~set_form_field
  EXPORTING
    name  = 'password'
    value = 'omerd!'.

o_post->send(
   EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state        = 2 ).

o_post->receive(
 EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state        = 2
   http_processing_failed    = 3 ).

DATA lv_response TYPE string.

lv_response = o_post->response->get_cdata( ).

BREAK-POINT.

TYPES: BEGIN OF lst_token,
         access_token TYPE string,
         token_type   TYPE string,
         expires_in   TYPE string,
       END OF lst_token.

DATA ls_token TYPE lst_token.

/ui2/cl_json=>deserialize(
  EXPORTING
    json        = lv_response
  CHANGING
    data        = ls_token ).

BREAK-POINT.

CALL METHOD cl_http_client=>create_by_url
  EXPORTING
    url                = 'http://185.22.184.249:3002/api/OzSutService/GetKupon?KuponKodu=OZST7TDHFS97'
  IMPORTING
    client             = DATA(o_get)
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4.

CALL METHOD o_get->request->set_method
  EXPORTING
    method = 'GET'.

lv_val = ls_token-token_type && ` ` && ls_token-access_token.

CALL METHOD o_get->request->set_header_field
  EXPORTING
    name  = 'Authorization'
    value = lv_val.

o_get->send(
   EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state        = 2 ).

o_get->receive(
 EXCEPTIONS
   http_communication_failure = 1
   http_invalid_state        = 2
   http_processing_failed    = 3 ).

CLEAR lv_response.
lv_response = o_get->response->get_cdata( ).

TYPES: BEGIN OF lst_return_value,
         kuponid                 TYPE string,
         gecerliliktarihi        TYPE string,
         kupondurumu             TYPE string,
         kampanyaid              TYPE string,
         kuponuretimiid          TYPE string,
         kuponkodu               TYPE string,
         kullanimsayisi          TYPE string,
         durum                   TYPE string,
         aciklama                TYPE string,
         bayikatiliorani         TYPE string,
         netfiyat                TYPE string,
         faturalandirilacaktutar TYPE string,
         kupontanitimi           TYPE string,
         kampanyatanitimi        TYPE string,
       END OF lst_return_value,

       BEGIN OF lst_response,
         success     TYPE string,
         errordetail TYPE string,
         errorcode   TYPE string,
         returnvalue TYPE lst_return_value,
       END OF lst_response.

DATA ls_response TYPE lst_response.

/ui2/cl_json=>deserialize(
  EXPORTING
    json        = lv_response

  CHANGING
    data        = ls_response ).

BREAK-POINT.