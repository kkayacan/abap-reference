TYPES: BEGIN OF gst_invo,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
       END OF gst_invo.

DATA: gt_invo TYPE TABLE OF gst_invo,
      go_grid TYPE REF TO cl_gui_alv_grid,
      go_cont TYPE REF TO cl_gui_custom_container.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm e_saved e_not_processed,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.
  METHOD on_user_command.
    BREAK-POINT.
  ENDMETHOD.
  METHOD on_after_user_command.
    BREAK-POINT.
  ENDMETHOD.
  METHOD on_toolbar.
    APPEND VALUE #( function = 'ADD' icon = icon_insert_row quickinfo = TEXT-b01 ) TO e_object->mt_toolbar.
    APPEND VALUE #( function = 'DEL' icon = icon_delete_row quickinfo = TEXT-b02 ) TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD on_f4.

    DATA lt_sel          TYPE STANDARD TABLE OF ddshretval.

    CASE e_fieldname.
      WHEN 'VBELN'.

        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = 'VBRP'
            fieldname         = 'VBELN'
            searchhelp        = 'H_VBRK'
          TABLES
            return_tab        = lt_sel
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.

        READ TABLE lt_sel ASSIGNING FIELD-SYMBOL(<ls_sel>) INDEX 1.
        IF sy-subrc = 0.
          READ TABLE gt_invo ASSIGNING FIELD-SYMBOL(<ls_invo>) INDEX es_row_no-row_id.
          IF sy-subrc = 0.
            <ls_invo>-vbeln = <ls_sel>-fieldval.
          ENDIF.
        ENDIF.

      WHEN 'POSNR'.

        READ TABLE gt_invo ASSIGNING <ls_invo> INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          SET PARAMETER ID 'VF' FIELD <ls_invo>-vbeln.
        ENDIF.

        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = 'VBRP'
            fieldname         = 'POSNR'
            searchhelp        = 'ZF4_POSNR_VBRP'
            callback_program  = 'ZQM_R_SUBROUTINE'
            callback_form     = 'POSNR_HELP_CB'
          TABLES
            return_tab        = lt_sel
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.

        READ TABLE lt_sel ASSIGNING <ls_sel> INDEX 1.
        IF sy-subrc = 0.
          READ TABLE gt_invo ASSIGNING <ls_invo> INDEX es_row_no-row_id.
          IF sy-subrc = 0.
            <ls_invo>-posnr = <ls_sel>-fieldval.
          ENDIF.
        ENDIF.

    ENDCASE.

    er_event_data->m_event_handled = abap_true.

  ENDMETHOD.
ENDCLASS.