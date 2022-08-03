FUNCTION z_get_vendor_performance .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IP_MAGAZAKODU) TYPE  CHAR4
*"     VALUE(IP_TARIH) TYPE  DATUM
*"     VALUE(IP_CUSTOMFIELD1) TYPE  DSTRING OPTIONAL
*"     VALUE(IP_CUSTOMFIELD2) TYPE  DSTRING OPTIONAL
*"     VALUE(IP_CUSTOMFIELD3) TYPE  DSTRING OPTIONAL
*"  EXPORTING
*"     VALUE(ET_VENDOR_PERFORMANCE) TYPE  ZTT_VENDOR_PERF_RFC_RESP
*"----------------------------------------------------------------------

  TYPES: BEGIN OF input,
           magazakodu   TYPE string,
           tarih        TYPE string,
           customfield1 TYPE string,
           customfield2 TYPE string,
           customfield3 TYPE string,
         END OF input,
         BEGIN OF json,
           input TYPE input,
         END OF json.

  DATA: lv_body     TYPE string,
        lv_response TYPE string,
        lv_json     TYPE json.

  lv_json-input-magazakodu = ip_magazakodu.
  lv_json-input-tarih      = ip_tarih(4) && '-' && ip_tarih+4(2) && '-' && ip_tarih+6(2).
  lv_json-input-customfield1 = ip_customfield1.
  lv_json-input-customfield2 = ip_customfield2.
  lv_json-input-customfield3 = ip_customfield3.

  lv_body = /ui2/cl_json=>serialize( data          = lv_json
                                     compress      = abap_false
                                     pretty_name   = /ui2/cl_json=>pretty_mode-none
*                                     name_mappings = lt_map
                                     ).

  TRY.
      lv_response = zcl_http=>request(
          ip_protocol   = 'http'
          ip_host       = 'nn.nnn.n.nnn:nnnnn'
          ip_endpoint   = '/RESTAdapter/bw/performans/info'
          ip_username   = 'USERNAME'
          ip_password   = 'PASSWORD'
          ip_body       = lv_body ).
    CATCH zcx_http INTO DATA(lx_http).
  ENDTRY.

  TYPES: BEGIN OF pers_list,
           item TYPE STANDARD TABLE OF zst_vendor_perf_item WITH DEFAULT KEY,
         END OF pers_list,
         BEGIN OF resp_item.
           INCLUDE TYPE zst_vendor_perf_response.
           TYPES:   personelliste TYPE pers_list,
         END OF resp_item,
         BEGIN OF response,

           item TYPE STANDARD TABLE OF resp_item WITH DEFAULT KEY,
         END OF response.

  DATA ls_parsed_response TYPE response.

  /ui2/cl_json=>deserialize(
    EXPORTING
      json             = lv_response
    CHANGING
      data             = ls_parsed_response ).

  LOOP AT ls_parsed_response-item ASSIGNING FIELD-SYMBOL(<ls_item>).
    LOOP AT <ls_item>-personelliste-item ASSIGNING FIELD-SYMBOL(<ls_pers_item>).
      APPEND INITIAL LINE TO et_vendor_performance ASSIGNING FIELD-SYMBOL(<ls_perf>).
      MOVE-CORRESPONDING <ls_item> TO <ls_perf>.
      MOVE-CORRESPONDING <ls_pers_item> TO <ls_perf>-personelliste.
    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.