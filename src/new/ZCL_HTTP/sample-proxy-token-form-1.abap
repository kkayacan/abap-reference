FUNCTION z_cdc_get_user .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EMAIL) TYPE  STRING OPTIONAL
*"     REFERENCE(IV_PHONE) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_USER) TYPE  ZCDC_USER
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA: lv_token TYPE string,
        lt_form  TYPE wdy_key_value_list.

  CALL FUNCTION 'Z_CDC_GET_TOKEN'
    IMPORTING
      token     = lv_token
      et_return = et_return.
  IF line_exists( et_return[ type = 'E' ] ).
    RETURN.
  ENDIF.

  DO 2 TIMES.

    CASE sy-index.
      WHEN 1.
        IF iv_email IS NOT INITIAL.
          lt_form = VALUE #( ( key = 'query' value = `SELECT * FROM emailAccounts WHERE profile.email="` && iv_email && `"` ) ).
        ELSE.
          lt_form = VALUE #( ( key = 'query' value = `SELECT * FROM emailAccounts WHERE phoneNumber="` && iv_phone && `"` ) ).
        ENDIF.
      WHEN 2.
        IF iv_email IS NOT INITIAL.
          lt_form = VALUE #( ( key = 'query' value = `SELECT * FROM accounts WHERE profile.email="` && iv_email && `"` ) ).
        ELSE.
          lt_form = VALUE #( ( key = 'query' value = `SELECT * FROM accounts WHERE phoneNumber="` && iv_phone && `"` ) ).
        ENDIF.
    ENDCASE.
    TRY.
        DATA(lv_response) = zcl_http=>request( ip_host       = 'accounts.eu1.gigya.com'
                                               ip_endpoint   = '/accounts.search'
                                               ip_proxy_host = c_proxy_host
                                               ip_proxy_port = c_proxy_port
                                               ip_token      = lv_token
                                               it_form       = lt_form ).
      CATCH zcx_http INTO DATA(lr_http_error).
        et_return = lr_http_error->get_msg( ).
        RETURN.
    ENDTRY.

    DATA(lr_response) = /ui2/cl_json=>generate( json = lv_response ).
    /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `RESULTS[1]-UID`)->value( IMPORTING ev_data = es_user-uid ).
    IF es_user-uid IS INITIAL.
      DATA lv_msg TYPE string.
      /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `ERRORDETAILS`)->value( IMPORTING ev_data = lv_msg ).
      DATA(ls_bapiret2) = zcl_string=>to_bapiret2( lv_msg ).
      APPEND ls_bapiret2 TO et_return.
      RETURN.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-COLLECTINGDATA-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-collecting_data ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-COLLECTINGMARKETINGDATA-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-collecting_marketing_data ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-COLLECTINGSTATISTICSDATA-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-collecting_statistics_data ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-NOTIFICATIONSFROMCALLCENTER-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-notifications_from_call_center ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-NOTIFICATIONSVIASMS-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-notifications_via_sms ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-NOTIFICATIONSVIAWHATSAPPANDVIB-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-notifications_via_whatsapp ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-PERMANENTDATA-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-permanent_data ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-PRIVACY-PRIVACYPOLICY-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-privacy_policy ).
  /ui2/cl_data_access=>create( ir_data = lr_response
                               iv_component = `RESULTS[1]-PREFERENCES-THMONELOYALTY-ISCONSENTGRANTED`
                               )->value( IMPORTING ev_data = es_user-thm_one_loyalty ).

ENDFUNCTION.