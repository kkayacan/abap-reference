FUNCTION z_cdc_set_account_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_ACCOUNT) TYPE  ZCDC_ACCOUNT
*"  EXPORTING
*"     REFERENCE(EV_UID) TYPE  STRING
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA: lv_token       TYPE string,
        lv_endpoint    TYPE string,
        lt_form        TYPE wdy_key_value_list,
        lv_token_field TYPE string.
  CALL FUNCTION 'Z_CDC_GET_TOKEN'
    IMPORTING
      token     = lv_token
      et_return = et_return.
  IF line_exists( et_return[ type = 'E' ] ).
    RETURN.
  ENDIF.

  CASE is_account-new_account.
    WHEN abap_true.
      lv_endpoint = '/accounts.initRegistration'.
      lt_form = VALUE #( ( key = 'isLite' value = `true` ) ).
      lv_token_field = 'REGTOKEN'.
    WHEN abap_false.
      lv_endpoint = '/accounts.getLiteToken'.
      lt_form = VALUE #( ( key = 'email' value = is_account-email ) ).
      lv_token_field = 'TOKEN'.
  ENDCASE.

  TRY.
      DATA(lv_response) = zcl_http=>request( ip_host       = 'accounts.eu1.gigya.com'
                                             ip_endpoint   = lv_endpoint
                                             ip_proxy_host = c_proxy_host
                                             ip_proxy_port = c_proxy_port
                                             ip_token      = lv_token
                                             it_form       = lt_form ).
    CATCH zcx_http INTO DATA(lr_http_error).
      et_return = lr_http_error->get_msg( ).
      RETURN.
  ENDTRY.

  DATA(lr_response) = /ui2/cl_json=>generate( json = lv_response ).
  DATA lv_regtoken TYPE string.
  /ui2/cl_data_access=>create( ir_data = lr_response iv_component = lv_token_field )->value( IMPORTING ev_data = lv_regtoken ).
  IF lv_regtoken IS INITIAL.
    DATA lv_msg TYPE string.
    /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `ERRORDETAILS` )->value( IMPORTING ev_data = lv_msg ).
    DATA(ls_bapiret2) = zcl_string=>to_bapiret2( lv_msg ).
    APPEND ls_bapiret2 TO et_return.
    RETURN.
  ENDIF.

  CLEAR lt_form.
  lt_form = VALUE #( ( key = 'regToken' value = lv_regtoken ) ).
  IF is_account-phone IS NOT INITIAL.
    APPEND VALUE #( key = 'phoneNumber' value = is_account-phone ) TO lt_form.
  ENDIF.
  IF is_account-email IS NOT INITIAL.
    APPEND VALUE #( key = 'profile' value = `{ "email": "` && is_account-email && `" }` ) TO lt_form.
  ENDIF.
  TYPES: BEGIN OF collecting_marketing_data,
           is_consent_granted TYPE boolean,
         END OF collecting_marketing_data,
         BEGIN OF notifications_via_sms,
           is_consent_granted TYPE boolean,
         END OF notifications_via_sms,
         BEGIN OF collecting_data,
           is_consent_granted TYPE boolean,
         END OF collecting_data,
         BEGIN OF collecting_statistics_data,
           is_consent_granted TYPE boolean,
         END OF collecting_statistics_data,
         BEGIN OF privacy_policy,
           is_consent_granted TYPE boolean,
         END OF privacy_policy,
         BEGIN OF privacy,
           privacy_policy TYPE privacy_policy,
         END OF privacy,
         BEGIN OF permanent_data,
           is_consent_granted TYPE boolean,
         END OF permanent_data,
         BEGIN OF notifications_via_whatsapp,
           is_consent_granted TYPE boolean,
         END OF notifications_via_whatsapp,
         BEGIN OF notifications_from_call_center,
           is_consent_granted TYPE boolean,
         END OF notifications_from_call_center,
         BEGIN OF thm_one_loyalty,
           is_consent_granted TYPE boolean,
         END OF thm_one_loyalty,
         BEGIN OF preferences,
           collecting_marketing_data      TYPE collecting_marketing_data,
           notifications_via_sms          TYPE notifications_via_sms,
           collecting_data                TYPE collecting_data,
           collecting_statistics_data     TYPE collecting_statistics_data,
           privacy                        TYPE privacy,
           permanent_data                 TYPE permanent_data,
           notifications_via_whatsapp     TYPE notifications_via_whatsapp,
           notifications_from_call_center TYPE notifications_from_call_center,
           thm_one_loyalty                TYPE thm_one_loyalty,
         END OF preferences.
  DATA: ls_preferences TYPE preferences,
        lt_mapping     TYPE /ui2/cl_json=>name_mappings.
  ls_preferences-collecting_marketing_data-is_consent_granted      = is_account-collecting_marketing_data.
  ls_preferences-notifications_via_sms-is_consent_granted          = is_account-notifications_via_sms.
  ls_preferences-collecting_data-is_consent_granted                = is_account-collecting_data.
  ls_preferences-collecting_statistics_data-is_consent_granted     = is_account-collecting_statistics_data.
  ls_preferences-privacy-privacy_policy-is_consent_granted         = is_account-privacy_policy.
  ls_preferences-permanent_data-is_consent_granted                 = is_account-permanent_data.
  ls_preferences-notifications_via_whatsapp-is_consent_granted     = is_account-notifications_via_whatsapp.
  ls_preferences-notifications_from_call_center-is_consent_granted = is_account-notifications_from_call_center.
  ls_preferences-thm_one_loyalty-is_consent_granted                = is_account-thm_one_loyalty.
  lt_mapping = VALUE #( ( abap = `IS_CONSENT_GRANTED`             json = `isConsentGranted` )
                        ( abap = `COLLECTING_MARKETING_DATA`      json = `collectingMarketingData` )
                        ( abap = `NOTIFICATIONS_VIA_SMS`          json = `notificationsViaSMS` )
                        ( abap = `COLLECTING_DATA`                json = `collectingData` )
                        ( abap = `COLLECTING_STATISTICS_DATA`     json = `collectingStatisticsData` )
                        ( abap = `PRIVACY`                        json = `privacy` )
                        ( abap = `PRIVACY_POLICY`                 json = `privacyPolicy` )
                        ( abap = `PERMANENT_DATA`                 json = `permanentData` )
                        ( abap = `NOTIFICATIONS_VIA_WHATSAPP`     json = `notificationsViaWhatsAppAndViber` )
                        ( abap = `NOTIFICATIONS_FROM_CALL_CENTER` json = `notificationsFromCallCenter` )
                        ( abap = `THM_ONE_LOYALTY`                json = `thmOneLoyalty` ) ).
  DATA(lv_preferences) = /ui2/cl_json=>serialize( data = ls_preferences compress = abap_true name_mappings = lt_mapping ).
  APPEND VALUE #( key = 'preferences' value = lv_preferences ) TO lt_form.
  APPEND VALUE #( key = 'lang' value = SWITCH #( sy-langu WHEN 'd' THEN 'sr' ELSE 'en' ) ) TO lt_form.

  TRY.
      lv_response = zcl_http=>request( ip_host       = 'accounts.eu1.gigya.com'
                                       ip_endpoint   = '/accounts.setAccountInfo'
                                       ip_proxy_host = c_proxy_host
                                       ip_proxy_port = c_proxy_port
                                       ip_token      = lv_token
                                       it_form       = lt_form ).
    CATCH zcx_http INTO lr_http_error.
      et_return = lr_http_error->get_msg( ).
      RETURN.
  ENDTRY.

  CLEAR lr_response.
  lr_response = /ui2/cl_json=>generate( json = lv_response ).
  /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `UID` )->value( IMPORTING ev_data = ev_uid ).
  IF ev_uid IS INITIAL.
    /ui2/cl_data_access=>create( ir_data = lr_response iv_component = `ERRORDETAILS` )->value( IMPORTING ev_data = lv_msg ).
    ls_bapiret2 = zcl_string=>to_bapiret2( lv_msg ).
    APPEND ls_bapiret2 TO et_return.
  ENDIF.

ENDFUNCTION.