METHOD display_image.

    TYPES pic_line(1022) TYPE x.
    DATA  pic_tab TYPE TABLE OF pic_line.

    DATA l_alignment TYPE i.

    DATA lv_image TYPE xstring.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input  = image_base64
      IMPORTING
        output = lv_image
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_image
      TABLES
        binary_tab = pic_tab.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type    = 'IMAGE'
        subtype = 'GIF'
      TABLES
        data    = pic_tab
      CHANGING
        url     = url.

    CALL SCREEN 1001 STARTING AT 5 5.

  ENDMETHOD.

  MODULE status_1001 OUTPUT.
  DATA l_alignment TYPE i.

  CREATE OBJECT: container1 EXPORTING container_name = 'PICTURE1',
                 pict1 EXPORTING parent = container1.

  l_alignment = cl_gui_control=>align_at_left   +
                cl_gui_control=>align_at_right  +
                cl_gui_control=>align_at_top    +
                cl_gui_control=>align_at_bottom.

  CALL METHOD pict1->set_alignment
    EXPORTING
      alignment = l_alignment.

  CALL METHOD pict1->set_3d_border
    EXPORTING
      border = 1.

  evt_tab_line-eventid = cl_gui_picture=>eventid_picture_click.
  evt_tab_line-appl_event = ' '.       " System Event!
  APPEND evt_tab_line TO evt_tab.

  CALL METHOD pict1->set_registered_events
    EXPORTING
      events = evt_tab.

  CALL METHOD pict1->load_picture_from_url
    EXPORTING
      url    = url
    EXCEPTIONS
      OTHERS = 4.

  pict1->set_display_mode(
    EXPORTING
      display_mode = cl_gui_picture=>display_mode_fit
    EXCEPTIONS
      error = 1 ).

  SET PF-STATUS 'SCREEN_1001'.
ENDMODULE.