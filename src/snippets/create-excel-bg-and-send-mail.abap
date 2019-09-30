FORM send_mail_new  TABLES   pt_excel TYPE tty_excel.

  TYPE-POOLS: ixml.

  TYPES: BEGIN OF xml_line,
           data(255) TYPE x,
         END OF xml_line.

  DATA: l_ixml          TYPE REF TO if_ixml,
        l_streamfactory TYPE REF TO if_ixml_stream_factory,
        l_ostream       TYPE REF TO if_ixml_ostream,
        l_renderer      TYPE REF TO if_ixml_renderer,
        l_document      TYPE REF TO if_ixml_document.

  DATA: ns_attribute         TYPE REF TO if_ixml_attribute,
        l_element_root       TYPE REF TO if_ixml_element,
        r_element_properties TYPE REF TO if_ixml_element,
        r_element            TYPE REF TO if_ixml_element,
        r_worksheet          TYPE REF TO if_ixml_element,
        r_table              TYPE REF TO if_ixml_element,
        r_column             TYPE REF TO if_ixml_element,
        r_row                TYPE REF TO if_ixml_element,
        r_cell               TYPE REF TO if_ixml_element,
        r_data               TYPE REF TO if_ixml_element,
        r_styles             TYPE REF TO if_ixml_element,
        r_style              TYPE REF TO if_ixml_element,
        r_style1             TYPE REF TO if_ixml_element,
        r_format             TYPE REF TO if_ixml_element,
        r_border             TYPE REF TO if_ixml_element,
        lv_value             TYPE string.

  DATA: lt_xml       TYPE TABLE OF xml_line,
        ls_xml       TYPE xml_line,
        lv_tab_lines TYPE sy-tabix,
        lv_xml_size  TYPE i,
        lv_rc        TYPE i,
        lv_msg_lines TYPE sy-tabix,
        lv_body      TYPE string,
        lv_mail_body TYPE string.

  DATA: lt_packing_list  TYPE TABLE OF sopcklsti1,
        lt_object_header TYPE TABLE OF solisti1,
        lt_contents_txt  TYPE TABLE OF solisti1,
        lt_contents_hex  TYPE TABLE OF solix,
        lt_receivers     TYPE TABLE OF somlreci1,
        ls_excel         TYPE ty_excel_attach.

  DATA: ls_document_data TYPE sodocchgi1,
        ls_packing_list  TYPE sopcklsti1,
        ls_object_header TYPE solisti1,
        ls_contents_txt  TYPE solisti1,
        ls_contents_hex  TYPE solix,
        ls_receivers     TYPE somlreci1.

  DEFINE body_append.
    CONCATENATE lv_body &1 INTO lv_body.
  END-OF-DEFINITION.

  DEFINE body_replace.
    REPLACE ALL OCCURRENCES OF &1 IN lv_body WITH &2.
  END-OF-DEFINITION.

  DEFINE subject_replace.
    REPLACE ALL OCCURRENCES OF &1 IN lv_subject WITH &2.
  END-OF-DEFINITION.

*-Document description-----------------------------------------------*
  ls_document_data-obj_name  = 'MAIL_TO_HEAD'.
  ls_document_data-obj_descr = 'Oluşturulan Muhatap ve Kart Bilgileri'.
*--------------------------------------------------------------------*

*-Mail body----------------------------------------------------------*
  CLEAR: lv_body.
  CONCATENATE 'Oluşturulan muhatap ve kart' 'bilgileri ektedir.'
 INTO lv_mail_body SEPARATED BY space.

  body_append:
    '<!-- SENT_FROM=[ZCRM_P004] -->',
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="tr" lang="tr">',
    '<head>',
    '  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">',
    '  <meta http-equiv="Content-Language" content="TR"/>',
    '  <style type="text/css">',
    '    body {font-family: Arial; font-size: 11pt;}',
    '  </style>',
    '</head>',
    '  <body bgcolor="#FFFFFF">',
    '    Sayın ilgili,<br><br>',
    '    <p>',
       lv_mail_body ,
    '  </p>',
    '    Saygılarımızla,',
    '  </body>',
    '</html>'.


*-Concert string to table (255 char wide)----------------------------*
  REFRESH lt_contents_txt.

  DO.
    IF strlen( lv_body ) GE 255.
      APPEND lv_body(255) TO lt_contents_txt.
    ELSE.
      APPEND lv_body TO lt_contents_txt.
      EXIT.
    ENDIF.
    SHIFT lv_body BY 255 PLACES LEFT.
  ENDDO.
*--------------------------------------------------------------------*

*-Document data------------------------------------------------------*
  CLEAR: lv_msg_lines.

  DESCRIBE TABLE lt_contents_txt LINES lv_msg_lines.

  READ TABLE lt_contents_txt
        INTO ls_contents_txt
       INDEX lv_msg_lines.

  ls_document_data-doc_size = ( lv_msg_lines - 1 ) * 255 +
                               strlen( ls_contents_txt ).
*--------------------------------------------------------------------*

*-Attachment---------------------------------------------------------*
* Creating a ixml Factory
  l_ixml = cl_ixml=>create( ).

* Creating the DOM Object Model
  l_document = l_ixml->create_document( ).

* Create Root Node 'Workbook'
  l_element_root  = l_document->create_simple_element( name   = 'Workbook'
                                                       parent = l_document ).
  l_element_root->set_attribute( name = 'xmlns'        value  = 'urn:schemas-microsoft-com:office:spreadsheet' ).

  ns_attribute = l_document->create_namespace_decl(    name   = 'ss'
                                                       prefix = 'xmlns'
                                                       uri    = 'urn:schemas-microsoft-com:office:spreadsheet' ).
  l_element_root->set_attribute_node( ns_attribute ).

  ns_attribute = l_document->create_namespace_decl(    name   = 'x'
                                                       prefix = 'xmlns'
                                                       uri    = 'urn:schemas-microsoft-com:office:excel' ).
  l_element_root->set_attribute_node( ns_attribute ).

* Create node for document properties.
  r_element_properties = l_document->create_simple_element( name = 'TEST_REPORT'  parent = l_element_root ).
  lv_value = sy-uname.
  l_document->create_simple_element( name = 'Author'  value = lv_value   parent = r_element_properties ).

* Styles
  r_styles = l_document->create_simple_element(   name   = 'Styles'     parent = l_element_root ).

* Style for Header
  r_style  = l_document->create_simple_element(   name   = 'Style'      parent = r_styles ).
  r_style->set_attribute_ns( name = 'ID'          prefix = 'ss'         value  = 'Header' ).

  r_format  = l_document->create_simple_element(  name   = 'Font'       parent = r_style ).
  r_format->set_attribute_ns( name = 'Bold'       prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Interior'   parent = r_style  ).
  r_format->set_attribute_ns( name = 'Color'      prefix = 'ss'         value  = '#92D050' ).
  r_format->set_attribute_ns( name = 'Pattern'    prefix = 'ss'         value  = 'Solid' ).

  r_format  = l_document->create_simple_element(  name   = 'Alignment'  parent = r_style  ).
  r_format->set_attribute_ns( name = 'Vertical'   prefix = 'ss'         value  = 'Center' ).
  r_format->set_attribute_ns( name = 'WrapText'   prefix = 'ss'         value  = '1' ).

  r_border  = l_document->create_simple_element(  name   = 'Borders'    parent = r_style ).
  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Bottom' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Left' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Top' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Right' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

* Style for Data
  r_style1  = l_document->create_simple_element(  name   = 'Style'      parent = r_styles  ).
  r_style1->set_attribute_ns( name = 'ID'         prefix = 'ss'         value  = 'Data' ).

  r_border  = l_document->create_simple_element(  name   = 'Borders'    parent = r_style1 ).
  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Bottom' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Left' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Top' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

  r_format  = l_document->create_simple_element(  name   = 'Border'     parent = r_border  ).
  r_format->set_attribute_ns( name = 'Position'   prefix = 'ss'         value  = 'Right' ).
  r_format->set_attribute_ns( name = 'LineStyle'  prefix = 'ss'         value  = 'Continuous' ).
  r_format->set_attribute_ns( name = 'Weight'     prefix = 'ss'         value  = '1' ).

* Worksheet
  r_worksheet = l_document->create_simple_element( name = 'Worksheet'  parent = l_element_root ).
  r_worksheet->set_attribute_ns( name = 'Name'  prefix = 'ss'  value = 'Sheet1' ).

* Table
  r_table = l_document->create_simple_element(    name   = 'Table'      parent = r_worksheet ).
  r_table->set_attribute_ns( name = 'FullColumns' prefix = 'x'          value  = '1' ).
  r_table->set_attribute_ns( name = 'FullRows'    prefix = 'x'          value  = '1' ).

* Column Formatting
  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

  r_column = l_document->create_simple_element(   name   = 'Column'     parent = r_table ).
  r_column->set_attribute_ns( name = 'Width'      prefix = 'ss'         value  = '100' ).

* Blank Row
  r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).

* Column Headers Row
  r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).
  r_row->set_attribute_ns( name = 'AutoFitHeight'  prefix = 'ss'  value = '1' ).

  DEFINE create_excel_header.
    r_cell = l_document->create_simple_element( name   = 'Cell'  parent = r_row    ).
    r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'    value  = 'Header' ).
    r_data = l_document->create_simple_element( name   = 'Data'  parent = r_cell   value  = &1 ).
    r_data->set_attribute_ns( name = 'Type'     prefix = 'ss'    value  = 'String' ).
  END-OF-DEFINITION.

  DEFINE create_excel_body.
    lv_value = &1.
    r_cell = l_document->create_simple_element( name   = 'Cell'  parent = r_row ).
    r_cell->set_attribute_ns( name = 'StyleID'  prefix = 'ss'    value  = 'Data' ).
    r_data = l_document->create_simple_element( name   = 'Data'  parent = r_cell value = lv_value ). " Data
    r_data->set_attribute_ns( name = 'Type'     prefix = 'ss'    value  = 'String' ).                " Cell format
  END-OF-DEFINITION.

  create_excel_header: 'MUHATAP NUMARASI'.
  create_excel_header: 'KART NUMARASI'.


* Data Table
  LOOP AT pt_excel INTO ls_excel.

    r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).

    create_excel_body: ls_excel-partner_no.
    create_excel_body: ls_excel-card_no.

  ENDLOOP.

* Creating a Stream Factory
  l_streamfactory = l_ixml->create_stream_factory( ).

* Connect Internal XML Table to Stream Factory
  l_ostream = l_streamfactory->create_ostream_itable( table = lt_xml ).

* Rendering the Document
  l_renderer = l_ixml->create_renderer( ostream  = l_ostream  document = l_document ).
  lv_rc = l_renderer->render( ).

* Saving the XML Document
  lv_xml_size = l_ostream->get_num_written_raw( ).
*--------------------------------------------------------------------*

*-Creation of the document attachment--------------------------------*
  LOOP AT lt_xml INTO ls_xml.
    CLEAR: ls_contents_hex.
    ls_contents_hex-line = ls_xml-data.
    APPEND ls_contents_hex TO lt_contents_hex.
    CLEAR: ls_contents_hex.
  ENDLOOP.
*--------------------------------------------------------------------*

*-Object header------------------------------------------------------*
  DESCRIBE TABLE lt_contents_hex LINES lv_tab_lines.
  ls_object_header = ''.
  APPEND ls_object_header TO lt_object_header.
*--------------------------------------------------------------------*

*-Packing list for mail body-----------------------------------------*
  ls_packing_list-transf_bin = ' '.
  ls_packing_list-head_start = 1.
  ls_packing_list-head_num   = 0.
  ls_packing_list-body_start = 1.
  ls_packing_list-body_num   = lv_msg_lines.
  ls_packing_list-doc_type   = 'HTML'.
  APPEND ls_packing_list TO lt_packing_list.
  CLEAR: ls_packing_list.
*--------------------------------------------------------------------*

*-Packing list for attachment----------------------------------------*
  ls_packing_list-transf_bin = 'X'.
  ls_packing_list-head_start = 1.
  ls_packing_list-head_num   = 0.
  ls_packing_list-body_start = 1.
  ls_packing_list-body_num   = lv_tab_lines.
  ls_packing_list-obj_descr  = 'Oluşturulan Muhatap ve Kart Bilgileri'.
  ls_packing_list-doc_type   = 'XLS'.
  ls_packing_list-doc_size   = lv_tab_lines * 255.
  APPEND ls_packing_list TO lt_packing_list.
  CLEAR: ls_packing_list.
*--------------------------------------------------------------------*

**-Receiver data------------------------------------------------------*

  ls_receivers-receiver = p_email.
  ls_receivers-rec_type = 'U'.
  ls_receivers-express  = 'X'.
  APPEND ls_receivers TO lt_receivers.
  CLEAR: ls_receivers.

*--------------------------------------------------------------------*

*-Send mail----------------------------------------------------------*
  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_document_data
      put_in_outbox              = 'X'
      sender_address             = 'adese@adese.com.tr'
      sender_address_type        = 'INT'
      commit_work                = 'X'
    TABLES
      packing_list               = lt_packing_list
      object_header              = lt_object_header
      contents_txt               = lt_contents_txt
      contents_hex               = lt_contents_hex
      receivers                  = lt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.
    cl_os_transaction_end_notifier=>raise_commit_requested( ).
    CALL FUNCTION 'DB_COMMIT'.
    cl_os_transaction_end_notifier=>raise_commit_finished( ).
  ENDIF.
*---------------
ENDFORM.