FORM send_vendor_email USING p_ebeln TYPE ebeln.

  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs,
        lv_subject      TYPE so_obj_des,
        lt_body         TYPE soli_tab,
        ls_body         TYPE soli,
        lv_html         TYPE string,
        ls_ekko         TYPE ekko,
        lt_ekpo         TYPE STANDARD TABLE OF ekpo,
        ls_ekpo         TYPE ekpo,
        lv_vendor_name  TYPE name1,
        lv_vendor_adrnr TYPE adrnr,
        lv_vendor_email TYPE ad_smtpadr,
        lv_store_name   TYPE name1,
        lv_store_adrnr  TYPE adrnr,
        ls_adrc_store   TYPE adrc,
        lv_region_code  TYPE loclb,
        lv_region_name  TYPE name1,
        lv_erdat        TYPE erdat,
        lv_eroed        TYPE eroed,
        ls_address      TYPE bapiaddr3,
        lt_return_bapi  TYPE STANDARD TABLE OF bapiret2.

  " 1. Veri Toplama
  " -------------------------------------------------------------------
  SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln = p_ebeln.
  CHECK sy-subrc = 0.

  " Tedarikçi E-posta
  SELECT SINGLE name1 adrnr FROM lfa1 INTO (lv_vendor_name, lv_vendor_adrnr)
    WHERE lifnr = ls_ekko-lifnr.

  IF lv_vendor_adrnr IS NOT INITIAL.
    SELECT SINGLE smtp_addr FROM adr6
      WHERE addrnumber = @lv_vendor_adrnr
        AND smtp_addr IS NOT INITIAL
      INTO @lv_vendor_email.
  ENDIF.

  " E-posta yoksa çık
  CHECK lv_vendor_email IS NOT INITIAL.

  " Kalemler
  SELECT * FROM ekpo INTO TABLE lt_ekpo WHERE ebeln = p_ebeln.
  CHECK lt_ekpo IS NOT INITIAL.

  " Mağaza Bilgileri (İlk kalemden)
  READ TABLE lt_ekpo INTO ls_ekpo INDEX 1.

  " Mağaza Adı ve Adres No
  SELECT SINGLE name1 adrnr FROM t001w INTO (lv_store_name, lv_store_adrnr)
    WHERE werks = ls_ekpo-werks.

  " Bölge Bilgisi (WRF3 -> T001W)
  " WRF3-LOCNR 10 karakterdir, WERKS'i pad etmeliyiz
  DATA: lv_locnr TYPE locnr.
  lv_locnr = ls_ekpo-werks.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_locnr
    IMPORTING
      output = lv_locnr.

  SELECT SINGLE loclb FROM wrf3 INTO lv_region_code WHERE locnr = lv_locnr.
  IF sy-subrc = 0.
    SELECT SINGLE name1 FROM t001w INTO lv_region_name WHERE werks = lv_region_code.
  ENDIF.

  " Mağaza Adres Detayı
  IF lv_store_adrnr IS NOT INITIAL.
    SELECT SINGLE * FROM adrc INTO ls_adrc_store WHERE addrnumber = lv_store_adrnr.
  ENDIF.

  " Yetkili Bilgisi (EKKO-ERNAM)
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = ls_ekko-ernam
    IMPORTING
      address  = ls_address
    TABLES
      return   = lt_return_bapi.

  " Mağaza Tarihleri (ZMAGAZA_BIS)
  " Tablo yoksa hata vermesin diye dinamik veya try-catch gerekebilir ama standart kodda direkt yazılır.
  TRY.
      SELECT SINGLE erdat eroed FROM zmagaza_bis
        INTO (lv_erdat, lv_eroed)
        WHERE werks = ls_ekpo-werks.
    CATCH cx_root.
      " Tablo yoksa boş kalır
  ENDTRY.

  " 2. HTML Oluşturma
  " -------------------------------------------------------------------
  " CSS ve Başlangıç
  lv_html = |<html><head><style>| &&
            |body \{ font-family: Arial, sans-serif; font-size: 10pt; \}| &&
            |table \{ border-collapse: collapse; width: 100%; max-width: 800px; \}| &&
            |th, td \{ border: 1px solid black; padding: 5px; text-align: left; \}| &&
            |.header \{ background-color: #f2f2f2; font-weight: bold; \}| &&
            |</style></head><body>|.

  lv_html = lv_html && |<table>|.

  " Sipariş Bilgisi Başlığı
  lv_html = lv_html && |<tr><td colspan="2" class="header">Sipariş Bilgisi</td></tr>|.

  " Firma
  DATA: lv_firma_text TYPE string.
  IF lv_region_name IS NOT INITIAL.
    lv_firma_text = |{ lv_region_name } ({ lv_region_code })|.
  ELSE.
    lv_firma_text = |{ ls_ekko-bukrs }|.
  ENDIF.
  lv_html = lv_html && |<tr><td width="30%">Firma</td><td>{ lv_firma_text }</td></tr>|.

  " Mağaza Kodu/Adı
  lv_html = lv_html && |<tr><td>Mağaza Kodu/Adı</td><td>{ lv_store_name } / { ls_ekpo-werks }</td></tr>|.

  " Mağaza Adresi
  DATA: lv_adres TYPE string.
  lv_adres = |{ ls_adrc_store-street } { ls_adrc_store-house_num1 } { ls_adrc_store-post_code1 } { ls_adrc_store-city1 }|.
  lv_html = lv_html && |<tr><td>Mağaza Adresi</td><td>{ lv_adres }</td></tr>|.

  " Yetkili Adı
  lv_html = lv_html && |<tr><td>Yetkili Adı</td><td>{ ls_address-fullname }</td></tr>|.

  " Yetkili Cep Telefonu
  lv_html = lv_html && |<tr><td>Yetkili Cep Telefonu</td><td>{ ls_address-tel1_numbr }</td></tr>|.

  " Sevk Adresi (Mağaza Adresi ile aynı varsayıldı)
  lv_html = lv_html && |<tr><td>Sevk Adresi</td><td>{ lv_adres }</td></tr>|.

  " Planlanan Teslim Tarihi (EKET'ten)
  DATA: lv_date_str TYPE char10,
        lv_eindt    TYPE eindt.

  " EKET tablosundan teslim tarihini al (İlk kalem ve ilk schedule line için)
  SELECT SINGLE eindt FROM eket
    INTO lv_eindt
    WHERE ebeln = ls_ekpo-ebeln
      AND ebelp = ls_ekpo-ebelp.

  IF lv_eindt IS INITIAL.
    " Eğer EKET'ten alınamazsa bugünün tarihini veya boş bırakabiliriz
    " lv_eindt = sy-datum.
  ENDIF.

  WRITE lv_eindt TO lv_date_str.
  lv_html = lv_html && |<tr><td>Planlanan Teslim Tarih</td><td>{ lv_date_str }</td></tr>|.

  " Mağaza Kuruluş Tarihi
  IF lv_erdat IS NOT INITIAL.
    WRITE lv_erdat TO lv_date_str.
    lv_html = lv_html && |<tr><td>Mağaza Kuruluş Tarihi</td><td>{ lv_date_str }</td></tr>|.
  ELSE.
    lv_html = lv_html && |<tr><td>Mağaza Kuruluş Tarihi</td><td></td></tr>|.
  ENDIF.

  " Mağaza Açılış Tarihi
  IF lv_eroed IS NOT INITIAL.
    WRITE lv_eroed TO lv_date_str.
    lv_html = lv_html && |<tr><td>Mağaza Açılış Tarihi</td><td>{ lv_date_str }</td></tr>|.
  ELSE.
    lv_html = lv_html && |<tr><td>Mağaza Açılış Tarihi</td><td></td></tr>|.
  ENDIF.

  " Sipariş Numarası
  lv_html = lv_html && |<tr><td>Sipariş Numarası</td><td>{ p_ebeln }</td></tr>|.

  lv_html = lv_html && |</table><br>|.

  " Sipariş Detayları Tablosu
  lv_html = lv_html && |<table>|.
  lv_html = lv_html && |<tr><td colspan="3" class="header">Sipariş Detayları</td></tr>|.
  lv_html = lv_html && |<tr class="header"><td>Malzeme Kodu</td><td>Malzeme Tanımı</td><td>Miktar</td></tr>|.

  LOOP AT lt_ekpo INTO ls_ekpo.
    lv_html = lv_html && |<tr>|.

    " Malzeme Kodu (Sıfırları at)
    DATA: lv_matnr_short TYPE string.
    lv_matnr_short = ls_ekpo-matnr.
    SHIFT lv_matnr_short LEFT DELETING LEADING '0'.

    lv_html = lv_html && |<td>{ lv_matnr_short }</td>|.
    lv_html = lv_html && |<td>{ ls_ekpo-txz01 }</td>|.

    DATA: lv_menge_str TYPE string.
    lv_menge_str = ls_ekpo-menge.
    CONDENSE lv_menge_str.
    lv_html = lv_html && |<td>{ lv_menge_str } { ls_ekpo-meins }</td>|.

    lv_html = lv_html && |</tr>|.
  ENDLOOP.

  lv_html = lv_html && |</table></body></html>|.

  " 3. Mail Gönderimi
  " -------------------------------------------------------------------
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).

      " HTML Body Ekle
      lt_body = cl_document_bcs=>string_to_soli( lv_html ).

      lv_subject = |Yeni Sipariş: { p_ebeln }|.

      lo_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_text    = lt_body
        i_subject = lv_subject ).

      lo_send_request->set_document( lo_document ).

      " Alıcı (Tedarikçi)
      lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_vendor_email ).
      lo_send_request->add_recipient( lo_recipient ).

      " Sender (Sistem veya Yetkili)
      DATA: lo_sender TYPE REF TO if_sender_bcs.

      TRY.
          lo_sender = cl_cam_address_bcs=>create_internet_address( 'bimsapdestek@bim.com.tr' ).
          lo_send_request->set_sender( lo_sender ).
        CATCH cx_bcs.
          " Sender oluşturulamazsa varsayılan ile devam et
      ENDTRY.

      " Gönder
      lo_send_request->send( i_with_error_screen = abap_false ).

      COMMIT WORK.

    CATCH cx_bcs.
      " Hata
  ENDTRY.

ENDFORM.