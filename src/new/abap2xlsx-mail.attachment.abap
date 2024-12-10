    DATA lv_email TYPE string.

    CALL FUNCTION 'POPUP_GET_STRING'
      EXPORTING
        label = 'E-Posta'
      IMPORTING
        value = lv_email.

    DATA: lo_excel     TYPE REF TO zcl_excel,
          lo_worksheet TYPE REF TO zcl_excel_worksheet,
          lo_column    TYPE REF TO zcl_excel_column,
          lo_row       TYPE REF TO zcl_excel_row,
          lv_line      TYPE i.

    CREATE OBJECT lo_excel.
    lo_worksheet = lo_excel->get_active_worksheet( ).

    LOOP AT gt_out INTO DATA(ls_out).

      ADD 1 TO lv_line.
      lo_worksheet->set_cell( ip_row = lv_line ip_column = 'A' ip_value = ls_out-matnr ).

    ENDLOOP.

    DATA: cl_writer  TYPE REF TO zif_excel_writer,
          xdata      TYPE xstring,
          lt_rawdata TYPE STANDARD TABLE OF solix,
          lv_size    TYPE i.

    CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
    xdata = cl_writer->write_file( lo_excel ).

    CALL METHOD cl_bcs_convert=>xstring_to_solix(
      EXPORTING
        iv_xstring = xdata
      RECEIVING
        et_solix   = lt_rawdata ).

    " E-posta Gönderimi
    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs.

    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).

        " E-posta Gövdesi
        lo_document = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = VALUE soli_tab( ( line = 'Sealed Bag listesi ektedir.' ) )
          i_subject = 'Sealed Bag' ).

        lo_document->add_attachment(
          i_attachment_type    = 'BIN'
          i_attachment_subject = 'Sealed Bag'
          i_att_content_hex    = lt_rawdata
          i_attachment_header  = value #( ( line = '&SO_FILENAME=SealedBag.xlsx' ) ) ).

        lo_send_request->set_document( lo_document ).

        " Alıcı Ekle
        lo_recipient = cl_cam_address_bcs=>create_internet_address( CONV adr6-smtp_addr( lv_email ) ).
        lo_send_request->add_recipient( lo_recipient ).

        " E-posta Gönder
        lo_send_request->send( i_with_error_screen = abap_true ).
        COMMIT WORK.
        MESSAGE 'E-posta gönderildi.' TYPE 'S'.

      CATCH cx_bcs.
        MESSAGE 'E-posta gönderiminde hata.' TYPE 'E'.
    ENDTRY.