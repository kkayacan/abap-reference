FUNCTION z_cdc_get_token.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(TOKEN) TYPE  STRING
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA: lv_apikey  TYPE string,
        lv_userkey TYPE string,
        lv_secret  TYPE string.

  SELECT SINGLE low FROM tvarvc INTO lv_apikey  WHERE name = 'Z_CDC_APIKEY'.
  SELECT SINGLE low FROM tvarvc INTO lv_userkey WHERE name = 'Z_CDC_USERKEY'.
  SELECT SINGLE low FROM tvarvc INTO lv_secret  WHERE name = 'Z_CDC_SECRET'.

  TRY.
      DATA(lv_response) = zcl_http=>request( ip_host       = 'socialize.eu1.gigya.com'
                                             ip_endpoint   = '/socialize.getToken'
                                             ip_proxy_host = c_proxy_host
                                             ip_proxy_port = c_proxy_port
                                             it_form       = VALUE #( ( key = 'grant_type' value = 'none' )
                                                                      ( key = 'client_id'  value = lv_apikey )
                                                                      ( key = 'userKey'    value = lv_userkey )
                                                                      ( key = 'secret'     value = lv_secret ) ) ).
    CATCH zcx_http INTO DATA(lr_http_error).
      et_return = lr_http_error->get_msg( ).
      RETURN.
  ENDTRY.

  DATA(lr_response) = /ui2/cl_json=>generate( json = lv_response ).
  /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `ACCESS_TOKEN`)->value( IMPORTING ev_data = token ).
  IF token IS INITIAL.
    DATA lv_msg TYPE string.
    /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `ERRORDETAILS`)->value( IMPORTING ev_data = lv_msg ).
    DATA(ls_bapiret2) = zcl_string=>to_bapiret2( lv_msg ).
    APPEND ls_bapiret2 TO et_return.
  ENDIF.

ENDFUNCTION.