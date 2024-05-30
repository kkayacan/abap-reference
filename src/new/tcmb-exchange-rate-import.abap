*&---------------------------------------------------------------------*
*& Report  ZDTY_EXCHANGERATE_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_p001.
*----------------------------- Type-Pools -----------------------------*
TYPE-POOLS ixml.

TABLES : soos1.

*----------------------------- Instances ------------------------------*
DATA : http_client TYPE REF TO if_http_client .
DATA : gv_xml_node TYPE REF TO if_ixml_node   .

*----------------------------- Variables ------------------------------*
DATA : gv_xmlstr       TYPE string.
DATA : gv_xml_nodetext TYPE string.
DATA : uri             TYPE string.
DATA : xmlstr          TYPE string.
DATA : kurtarih(10) TYPE c.
DATA : gv_first_time.

*-------------------------- Internal Tables ---------------------------*
DATA : BEGIN OF gt_rate OCCURS 0,
         tarih(10)           TYPE c,
         kod(3)              TYPE c,
         currencycode(3)     TYPE c,
         unit(5)             TYPE c,
         isim(30)            TYPE c,
         currencyname(30)    TYPE c,
         forexbuying(15)     TYPE c,
         forexselling(15)    TYPE c,
         banknotebuying(15)  TYPE c,
         banknoteselling(15) TYPE c,
         crossrateusd(10)    TYPE c,
         crossrateother(10)  TYPE c,
         crossrateeuro(10)   TYPE c,
       END OF gt_rate.

*-----------------------------  Includes ------------------------------*
INCLUDE zfi_p001_i001.

*-------------------------- Selection Screen --------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001    .
*PARAMETERS : p_datum  TYPE sy-datum DEFAULT sy-datum                 .
PARAMETERS : p_datum  TYPE sy-datum DEFAULT sy-datum.
PARAMETERS : p_sender TYPE text100 DEFAULT 'muhasebe@gulyilmaz.com.tr' ."test ama l  ge ici mail adresi kondu.

PARAMETERS : p_mail AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK block1                                .
*---
*****************************  EVENTS  *********************************
*------------------------- INITIALIZATION -----------------------------*
INITIALIZATION.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.
*------------------------  START-OF-SELECTION -------------------------*
START-OF-SELECTION                 .
  PERFORM set_datum USING p_datum  .
  PERFORM get_tr_central_bank_rate .

*-------------------------  END-OF-SELECTION --------------------------*
END-OF-SELECTION           .
  PERFORM fill_curr_rate   .
  IF p_mail IS NOT INITIAL.
    PERFORM send_info_mail   .
  ENDIF.

*******************************  PERFORMS ******************************
*&---------------------------------------------------------------------*
*&      Form  get_tr_central_bank_rate
*&---------------------------------------------------------------------*
* TCMB Sitesine Ba lan l yor
*----------------------------------------------------------------------*
FORM get_tr_central_bank_rate .

* Example : http://www.tcmb.gov.tr/kurlar/201307/22072013.xml
  CONCATENATE 'http://www.tcmb.gov.tr/kurlar/'
  gv_datum+0(6) '/' gv_datum+6(2)
  gv_datum+4(2) gv_datum+0(4)
  '.xml' INTO uri.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = uri
      proxy_service      = gv_proxy_service
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    PERFORM fill_gt_error USING text-m02.
    EXIT.
  ENDIF.

  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2.

  IF sy-subrc <> 0.
    PERFORM fill_gt_error USING text-m02.
    EXIT.
  ENDIF.

*  receive response
  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  IF sy-subrc <> 0.
    DATA lv_message TYPE string.
    CALL METHOD http_client->get_last_error
      IMPORTING
        message = lv_message.    "type string
    PERFORM fill_gt_error USING text-m02.
    EXIT.
  ENDIF.

*  get response as character data
  gv_xmlstr = http_client->response->get_cdata( ).

  IF gv_xmlstr IS INITIAL.
    PERFORM fill_gt_error USING text-m05.
    EXIT.
  ENDIF.

  PERFORM parse_xml_data.

ENDFORM.                    " get_tr_central_bank_rate
*&---------------------------------------------------------------------*
*&      Form  parse_xml_data
*&---------------------------------------------------------------------*
*& XML Datas n n Al nmas 
*----------------------------------------------------------------------*
FORM parse_xml_data.
  CLASS cl_ixml DEFINITION LOAD.

  DATA: g_ixml        TYPE REF TO if_ixml.
  DATA: streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA: istream       TYPE REF TO if_ixml_istream.
  DATA: document      TYPE REF TO if_ixml_document.
  DATA: parser        TYPE REF TO if_ixml_parser.

  g_ixml = cl_ixml=>create( ).

* Now Create Stream Factory
  streamfactory = g_ixml->create_stream_factory( ).

* wrap the table containing the file into a stream
  istream = streamfactory->create_istream_string( string = gv_xmlstr ) .

* Create XML Document instance
  document = g_ixml->create_document( ).

* Create parser Object
  parser = g_ixml->create_parser( stream_factory = streamfactory
  istream = istream
  document = document ).

* Parse an XML document into a DOM tree
*parser->parse( ).

* Parsing Error Processing
  IF parser->parse( ) NE 0.
    IF parser->num_errors( ) NE 0.
      IF xmlstr IS INITIAL.
        PERFORM fill_gt_error USING text-m03.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

* Close the stream since it  s not needed anymore
  CALL METHOD istream->close( ).
  CLEAR istream.

  gv_first_time = 'X'.
  gv_xml_node = document.

  PERFORM create_datatable USING  gv_xml_node.

  IF lines( gt_rate ) EQ 0.
    PERFORM fill_gt_error USING text-m05.
    EXIT.
  ENDIF.

ENDFORM.                    " parse_xml_data
*&---------------------------------------------------------------------*
*&      Form  create_datatable
*&---------------------------------------------------------------------*
* Verinin   z mlenmesi
*----------------------------------------------------------------------*
FORM create_datatable USING VALUE(x_node) TYPE REF TO if_ixml_node.
  DATA: ptext       TYPE REF TO if_ixml_text.
  DATA: string      TYPE string.
  DATA: node2       TYPE REF TO if_ixml_node.
  DATA: attributes  TYPE REF TO if_ixml_named_node_map.
*----------
  CASE x_node->get_type( ).
    WHEN if_ixml_node=>co_node_element.
      string = x_node->get_name( ).
      gv_xml_nodetext = string.
      IF gv_xml_nodetext EQ 'Tarih_Date'.
        attributes = x_node->get_attributes( ).

        CALL METHOD attributes->get_named_item
          EXPORTING
            name = 'Tarih'
*           NAMESPACE = ''
          RECEIVING
            rval = node2.
        string = node2->get_value( ).
        MOVE string TO kurtarih.

      ELSEIF gv_xml_nodetext EQ 'Currency'.
        IF gv_first_time NE 'X'.
          gt_rate-tarih = kurtarih.
          APPEND gt_rate.
          CLEAR : gt_rate.
        ENDIF.
        gv_first_time = ' '.

        attributes = x_node->get_attributes( ).

        CALL METHOD attributes->get_named_item
          EXPORTING
            name = 'Kod'
          RECEIVING
            rval = node2.
        string = node2->get_value( ).
        MOVE string TO gt_rate-kod.

        CALL METHOD attributes->get_named_item
          EXPORTING
            name = 'CurrencyCode'
          RECEIVING
            rval = node2.
        string = node2->get_value( ).
        MOVE string TO gt_rate-currencycode.

      ENDIF.

    WHEN if_ixml_node=>co_node_text.
      ptext ?= x_node->query_interface( ixml_iid_text ).
      IF ptext->ws_only( ) IS INITIAL.
        string = x_node->get_value( ).

        CASE gv_xml_nodetext.
          WHEN 'Kod'.
            MOVE string TO gt_rate-kod.

          WHEN 'CurrencyCode'.
            MOVE string TO gt_rate-currencycode.

          WHEN 'Unit'.
            MOVE string TO gt_rate-unit.

          WHEN 'Isim'.
            MOVE string TO gt_rate-isim.

          WHEN 'CurrencyName'.
            MOVE string TO gt_rate-currencyname.

          WHEN 'ForexBuying'.
            MOVE string TO gt_rate-forexbuying.

          WHEN 'ForexSelling'.
            MOVE string TO gt_rate-forexselling.

          WHEN 'BanknoteBuying'.
            MOVE string TO gt_rate-banknotebuying.

          WHEN 'BanknoteSelling'.
            MOVE string TO gt_rate-banknoteselling.

          WHEN 'CrossRateUSD'.
            MOVE string TO gt_rate-crossrateusd.

          WHEN 'CrossRateOther'.
            MOVE string TO gt_rate-crossrateother.

          WHEN 'CrossRateEuro'.
            MOVE string TO gt_rate-crossrateeuro.

        ENDCASE.

      ENDIF.
  ENDCASE.

* Get the next child
  x_node = x_node->get_first_child( ).
* Recurse
  WHILE NOT x_node IS INITIAL.
    PERFORM create_datatable USING  x_node.
    x_node = x_node->get_next( ).
  ENDWHILE.

ENDFORM.                    "get_datatable
*&---------------------------------------------------------------------*
*&      Form  fill_curr_rate
*&---------------------------------------------------------------------*
* Kurlar n Aktar lmas 
*----------------------------------------------------------------------*
FORM fill_curr_rate .
  DATA: exch_rate LIKE bapi1093_0   OCCURS 1 WITH HEADER LINE,
        rettab    LIKE bapiret2     OCCURS 1 WITH HEADER LINE,
        ret2      LIKE bapiret2     OCCURS 1 WITH HEADER LINE,
        ret3      LIKE bapiret2     OCCURS 1 WITH HEADER LINE.
  DATA: ls_kur      TYPE zfi_t001.
  DATA: lt_kur      TYPE TABLE OF zfi_t001.
  DATA: lv_rate(10) TYPE c              .
  DATA: lv_using_c(4) TYPE c VALUE ',.. '.
  DATA: lv_error    TYPE text150        .
*--------
  CHECK gt_error[] IS INITIAL.
  SELECT * FROM zfi_t001 INTO TABLE lt_kur
  WHERE landl = 'TR'.
  LOOP AT lt_kur INTO ls_kur.
    CLEAR exch_rate.
    IF ls_kur-fcurr = 'BGN'.
      READ TABLE gt_rate WITH KEY currencycode = 'BGN'."BGL test ama l  BGN olarak de i tirildi eyilmaz.
      IF sy-subrc NE 0.
        READ TABLE gt_rate WITH KEY currencycode = ls_kur-fcurr.
      ENDIF.
    ELSE.
      READ TABLE gt_rate WITH KEY currencycode = ls_kur-fcurr.
    ENDIF.
    IF sy-subrc NE 0.
      CONCATENATE ls_kur-fcurr text-m06 INTO lv_error
      SEPARATED BY space .
      PERFORM fill_gt_error USING lv_error.
      CONTINUE.
    ENDIF.


    exch_rate-rate_type   = ls_kur-kurst.

    CASE     exch_rate-rate_type.
      WHEN 'M' . lv_rate = gt_rate-forexbuying   .
*      WHEN 'B' . LV_RATE = GT_RATE-FOREXSELLING  .
      WHEN 'S' . lv_rate = gt_rate-forexselling  .
      WHEN 'G' . lv_rate = gt_rate-banknotebuying.
      WHEN 'B' . lv_rate = gt_rate-banknoteselling.
    ENDCASE.

    IF ls_kur-hdfky EQ space.
      exch_rate-from_curr   = ls_kur-fcurr.
      exch_rate-to_currncy  = gv_waers_try.
      exch_rate-exch_rate   = lv_rate     .
      exch_rate-from_factor = gt_rate-unit.
      exch_rate-to_factor   = '1'         .
    ELSE.
      exch_rate-from_curr     = gv_waers_try.
      exch_rate-to_currncy    = ls_kur-fcurr.
      exch_rate-exch_rate_v   = lv_rate     .
      exch_rate-from_factor_v = gt_rate-unit.
      exch_rate-to_factor_v   = '1'         .
    ENDIF.

    exch_rate-valid_from    = p_datum + 1 .

    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
      EXPORTING
        exch_rate = exch_rate
        upd_allow = 'X'
      IMPORTING
        return    = rettab.

    IF rettab-type = 'E'.
      PERFORM bapi_rollback_work.
      CONCATENATE ls_kur-fcurr text-m08 rettab-message INTO lv_error
      SEPARATED BY space .
      PERFORM fill_gt_error USING lv_error.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = ret2.
    IF ret2-type = 'E'.
      CONCATENATE ls_kur-fcurr text-m08 ret2-message INTO lv_error
      SEPARATED BY space .
      PERFORM fill_gt_error USING lv_error.
      CONTINUE.
    ELSE.
      PERFORM fill_gt_info USING exch_rate-from_curr
            exch_rate-to_currncy
            lv_rate
            gt_rate-unit.
    ENDIF.

  ENDLOOP.

**-----  EURS Kur Oran n n sisteme eklenmesi
**------EURS ve USDS kurlar  kullan lmayacak eyilmaz.
*  LOOP AT LT_KUR INTO LS_KUR WHERE FCURR EQ 'EUR'.
*    READ TABLE GT_RATE WITH KEY CURRENCYCODE = LS_KUR-FCURR.
*    IF SY-SUBRC NE 0.
*      CONCATENATE LS_KUR-FCURR TEXT-M06 INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ENDIF.
*    EXCH_RATE-RATE_TYPE   = LS_KUR-KURST.
*    LS_KUR-FCURR = 'EURS'.
*
*    CASE     EXCH_RATE-RATE_TYPE.
*      WHEN 'M' . LV_RATE = GT_RATE-FOREXSELLING   .
*      WHEN 'N' . LV_RATE = GT_RATE-FOREXSELLING  .
*      WHEN 'K' . LV_RATE = GT_RATE-BANKNOTEBUYING.
*      WHEN 'L' . LV_RATE = GT_RATE-BANKNOTESELLING.
*    ENDCASE.
*
*    IF LS_KUR-HDFKY EQ SPACE.
*      EXCH_RATE-FROM_CURR   = 'EURS'.
*      EXCH_RATE-TO_CURRNCY  = GV_WAERS_TRY.
*      EXCH_RATE-EXCH_RATE   = LV_RATE     .
*      EXCH_RATE-FROM_FACTOR = GT_RATE-UNIT.
*      EXCH_RATE-TO_FACTOR   = '1'         .
*    ELSE.
*      EXCH_RATE-FROM_CURR     = GV_WAERS_TRY.
*      EXCH_RATE-TO_CURRNCY    = 'EURS'.
*      EXCH_RATE-EXCH_RATE_V   = LV_RATE     .
*      EXCH_RATE-FROM_FACTOR_V = GT_RATE-UNIT.
*      EXCH_RATE-TO_FACTOR_V   = '1'         .
*    ENDIF.
*
*    EXCH_RATE-VALID_FROM    = P_DATUM + 1 .
*    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
*      EXPORTING
*        EXCH_RATE = EXCH_RATE
*        UPD_ALLOW = 'X'
*      IMPORTING
*        RETURN    = RETTAB.
*
*    IF RETTAB-TYPE = 'E'.
*      PERFORM BAPI_ROLLBACK_WORK.
*      CONCATENATE LS_KUR-FCURR TEXT-M08 RETTAB-MESSAGE INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      IMPORTING
*        RETURN = RET2.
*    IF RET2-TYPE = 'E'.
*      CONCATENATE LS_KUR-FCURR TEXT-M08 RET2-MESSAGE INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ELSE.
*      PERFORM FILL_GT_INFO USING EXCH_RATE-FROM_CURR
*            EXCH_RATE-TO_CURRNCY
*            LV_RATE
*            GT_RATE-UNIT.
*    ENDIF.
*
*  ENDLOOP.
*
**-----  USDS Kur Oran n n sisteme eklenmesi
*  LOOP AT LT_KUR INTO LS_KUR WHERE FCURR EQ 'USD'.
*    READ TABLE GT_RATE WITH KEY CURRENCYCODE = LS_KUR-FCURR.
*    IF SY-SUBRC NE 0.
*      CONCATENATE LS_KUR-FCURR TEXT-M06 INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ENDIF.
*    EXCH_RATE-RATE_TYPE   = LS_KUR-KURST.
*    LS_KUR-FCURR = 'USDS'.
*
*    CASE     EXCH_RATE-RATE_TYPE.
*      WHEN 'M' . LV_RATE = GT_RATE-FOREXSELLING  .
*      WHEN 'N' . LV_RATE = GT_RATE-FOREXSELLING  .
*      WHEN 'K' . LV_RATE = GT_RATE-BANKNOTEBUYING.
*      WHEN 'L' . LV_RATE = GT_RATE-BANKNOTESELLING.
*    ENDCASE.
*
*    IF LS_KUR-HDFKY EQ SPACE.
*      EXCH_RATE-FROM_CURR   = 'USDS'.
*      EXCH_RATE-TO_CURRNCY  = GV_WAERS_TRY.
*      EXCH_RATE-EXCH_RATE   = LV_RATE     .
*      EXCH_RATE-FROM_FACTOR = GT_RATE-UNIT.
*      EXCH_RATE-TO_FACTOR   = '1'         .
*    ELSE.
*      EXCH_RATE-FROM_CURR     = GV_WAERS_TRY.
*      EXCH_RATE-TO_CURRNCY    = 'USDS'.
*      EXCH_RATE-EXCH_RATE_V   = LV_RATE     .
*      EXCH_RATE-FROM_FACTOR_V = GT_RATE-UNIT.
*      EXCH_RATE-TO_FACTOR_V   = '1'         .
*    ENDIF.
*
*    EXCH_RATE-VALID_FROM    = P_DATUM + 1 .
*    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
*      EXPORTING
*        EXCH_RATE = EXCH_RATE
*        UPD_ALLOW = 'X'
*      IMPORTING
*        RETURN    = RETTAB.
*
*    IF RETTAB-TYPE = 'E'.
*      PERFORM BAPI_ROLLBACK_WORK.
*      CONCATENATE LS_KUR-FCURR TEXT-M08 RETTAB-MESSAGE INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      IMPORTING
*        RETURN = RET2.
*    IF RET2-TYPE = 'E'.
*      CONCATENATE LS_KUR-FCURR TEXT-M08 RET2-MESSAGE INTO LV_ERROR
*      SEPARATED BY SPACE .
*      PERFORM FILL_GT_ERROR USING LV_ERROR.
*      CONTINUE.
*    ELSE.
*      PERFORM FILL_GT_INFO USING EXCH_RATE-FROM_CURR
*            EXCH_RATE-TO_CURRNCY
*            LV_RATE
*            GT_RATE-UNIT.
*    ENDIF.
*
*  ENDLOOP.

  IF sy-subrc NE 0.
    PERFORM fill_gt_error USING text-m07.
  ENDIF.

ENDFORM.                    " fill_curr_rate
*&---------------------------------------------------------------------*
*&      Form  send_info_mail
*&---------------------------------------------------------------------*
*  Bilgi Mailinin Haz rlanmas 
*----------------------------------------------------------------------*
FORM send_info_mail .
  DATA: off_tab       LIKE soli  OCCURS 100 WITH HEADER LINE,
        rec_tab       LIKE soos1 OCCURS 1   WITH HEADER LINE,
        head_tab      LIKE soli  OCCURS 0   WITH HEADER LINE,
        obj_hd_change LIKE sood1,
        obj_type      LIKE sood-objtp,
        obj_lang      LIKE thead-tdspras,
        form          TYPE ssfkeyname,        " form(30)+variant(8)
        originator    LIKE soos1-recextnam,
        sent_to_all   TYPE sonv-flag,
        l_subrc       LIKE sy-subrc,
        ls_reclist    TYPE    somlreci1,
        l_update      TYPE sy-subrc.

  DATA: l_entry TYPE soli,
        l_wa    TYPE soli.
  DATA : office_objid LIKE  soodk,
         mail_ok      TYPE  tdsfflag.
  DATA : mail_application_object_ids LIKE swotobjid OCCURS 0
        WITH HEADER LINE.
  DATA : msgid TYPE  symsgid,
         msgno TYPE  symsgno,
         msgv1 TYPE  symsgv,
         msgv2 TYPE  symsgv,
         msgv3 TYPE  symsgv,
         msgv4 TYPE  symsgv.
  DATA : lv_datum(10) TYPE c.
*------

* send data to SAPoffice
  CLEAR obj_hd_change.

  WRITE p_datum TO lv_datum DD/MM/YYYY.

  obj_hd_change-objla  = sy-langu.
  obj_hd_change-objpri = '1'     .
  obj_hd_change-objsns = 'O'     .
  obj_type             = 'RAW'   .

  IF gt_error[] IS INITIAL.

    EXIT.

    CONCATENATE sy-host
                lv_datum
                text-301 INTO off_tab
                SEPARATED BY space .
    APPEND off_tab.
    obj_hd_change-objdes = off_tab.

  ELSE.
* Hata Nedenleri
    CONCATENATE sy-host
                lv_datum
                text-300 INTO off_tab
                SEPARATED BY space .
    APPEND off_tab.
    obj_hd_change-objdes = off_tab.
    LOOP AT gt_error.
      off_tab = gt_error-mess.
      APPEND off_tab.
    ENDLOOP.
  ENDIF.

  APPEND INITIAL LINE TO off_tab.
  LOOP AT gt_info.
    IF sy-tabix = '1'.
      off_tab = text-m10.
      APPEND off_tab.
    ENDIF.
    off_tab = gt_info-mess.
    APPEND off_tab.
  ENDLOOP.

  CLEAR rec_tab.

  rec_tab-deliver    = ''  . " request delivery notification
  rec_tab-not_deli   = ''  . " request non-delivery notification
  rec_tab-recesc     = 'U'  .
  rec_tab-sndart     = 'INT'.
  rec_tab-sndbc      = space.
  rec_tab-sndcp      = space.
  rec_tab-recesc     = 'U'  .

  SELECT email FROM zfi_t002 INTO rec_tab-recextnam.
    APPEND rec_tab.
  ENDSELECT.

  originator = p_sender.
* call SO_OBJECT_SEND
  CALL FUNCTION 'SO_OBJECT_SEND'
    EXPORTING
      object_hd_change           = obj_hd_change
      object_type                = obj_type
      owner                      = space
      originator                 = originator
      originator_type            = 'U'
    IMPORTING
      object_id_new              = office_objid
      sent_to_all                = sent_to_all
    TABLES
      objcont                    = off_tab
      objhead                    = head_tab
*     OBJPARA                    =
*     OBJPARB                    =
      receivers                  = rec_tab
*     PACKING_LIST               =
*     ATT_CONT                   =
*     ATT_HEAD                   =
*     NOTE_TEXT                  =
*     LINK_LIST                  =
      application_object         = mail_application_object_ids
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      folder_not_exist           = 4
      folder_no_authorization    = 5
      forwarder_not_exist        = 6
      note_not_exist             = 7
      object_not_exist           = 8
      object_not_sent            = 9
      object_no_authorization    = 10
      object_type_not_exist      = 11
      operation_no_authorization = 12
      owner_not_exist            = 13
      parameter_error            = 14
      substitute_not_active      = 15
      substitute_not_defined     = 16
      system_failure             = 17
      too_much_receivers         = 18
      user_not_exist             = 19
      x_error                    = 20
      OTHERS                     = 21.
  l_subrc = sy-subrc.

  CALL FUNCTION 'SSF_SO_ERROR'
    EXPORTING
      sent_to_all = sent_to_all
      subrc       = l_subrc
    IMPORTING
      ok          = mail_ok
      msgid       = msgid
      msgno       = msgno
      msgv1       = msgv1
      msgv2       = msgv2
      msgv3       = msgv3
      msgv4       = msgv4
    TABLES
      receivers   = rec_tab.

* check for COMMIT
  CALL FUNCTION 'TH_IN_UPDATE_TASK'
    IMPORTING
      in_update_task = l_update.

  IF l_subrc = 0 AND l_update = 0.
    COMMIT WORK AND WAIT.
  ENDIF.
  PERFORM submit_rsconn01 .

  LOOP AT off_tab.
    WRITE : / off_tab.
  ENDLOOP.

ENDFORM.                    " send_info_mail
*&---------------------------------------------------------------------*
*&      Form  submit_rsconn01
*&---------------------------------------------------------------------*
*  Maillerin G nderilmesi
*----------------------------------------------------------------------*
FORM submit_rsconn01 .

  WAIT UP TO 1 SECONDS      .
  SUBMIT rsconn01 AND RETURN.

ENDFORM.                    " submit_rsconn01

*Text elements
*----------------------------------------------------------
* 300  tarihli kur aktar m nda a a  daki hatalarla kar  la  lm  t r:
* 301  tarihli kur aktar m  ba ar yla yap lm  t r
* 400 birim
* M02 TCMB Sitesine Ula  lam yor
* M03 TCMB Sitesinden Veriler Okunurken Hata  le Kar  la  ld 
* M05 Aktar lacak Veri Bulunamad 
* M06  Para Birimi i in de er bulunamad 
* M07 Uyarlama Tablosunda Aktar lacak Veri Bulunamad 
* M08  Para Birimi i in hata :
* M10 Aktar lan Kurlara  li kin Bilgiler:

*Selection texts
*----------------------------------------------------------
* P_DATUM         Kur tarihi