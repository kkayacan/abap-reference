REPORT zaknp_billdoc_view.

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      start_of_selection,
      pbo,
      pai,
      display_grid,
      change_fcat
        IMPORTING it_fcat        TYPE lvc_t_fcat
        RETURNING VALUE(et_fcat) TYPE lvc_t_fcat,
      save_changes,
      create_goods_movement,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
  PRIVATE SECTION.
    CLASS-DATA: o_view TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

TABLES: erpsls_billdoc, zacron_ei_t_300, vbrk.

SELECTION-SCREEN BEGIN OF BLOCK docnum WITH FRAME TITLE text-vbe.
SELECT-OPTIONS: svbeln FOR erpsls_billdoc-vbeln.
SELECTION-SCREEN END OF BLOCK docnum.

SELECTION-SCREEN BEGIN OF BLOCK docdata WITH FRAME TITLE text-doc.
SELECT-OPTIONS: sfkart FOR erpsls_billdoc-fkart DEFAULT 'ZIHR',
                skunag FOR erpsls_billdoc-kunag,
                skunrg FOR erpsls_billdoc-kunrg,
                sfkdat FOR erpsls_billdoc-fkdat.
SELECTION-SCREEN END OF BLOCK docdata.

SELECTION-SCREEN BEGIN OF BLOCK orgdata WITH FRAME TITLE text-org.
SELECT-OPTIONS: svkorg FOR erpsls_billdoc-vkorg MEMORY ID vko OBLIGATORY DEFAULT '1000',
                svtweg FOR erpsls_billdoc-vtweg MEMORY ID vtw OBLIGATORY DEFAULT '30'.
SELECTION-SCREEN END OF BLOCK orgdata.

SELECTION-SCREEN BEGIN OF BLOCK sperre WITH FRAME TITLE text-inc.
PARAMETER  popen  AS CHECKBOX DEFAULT 'X'.
PARAMETER  pnotopen   AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END   OF BLOCK sperre.

SELECTION-SCREEN BEGIN OF BLOCK addfld WITH FRAME TITLE text-add.
SELECT-OPTIONS: sgibno FOR zacron_ei_t_300-invid,
                sbynno FOR vbrk-zzbeyanno.
SELECTION-SCREEN END OF BLOCK addfld.

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  CALL SCREEN 0001.

MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.

CLASS lcl_controller IMPLEMENTATION.

  METHOD start_of_selection.

    zcl_akn_billdoc_update=>retrieve_list( VALUE #( svbeln   = svbeln[]
                                                    sfkart   = sfkart[]
                                                    skunag   = skunag[]
                                                    skunrg   = skunrg[]
                                                    sfkdat   = sfkdat[]
                                                    svkorg   = svkorg[]
                                                    svtweg   = svtweg[]
                                                    popen    = popen
                                                    pnotopen = pnotopen
                                                    sgibno   = sgibno[]
                                                    sbynno   = sbynno[] ) ).

  ENDMETHOD.

  METHOD pbo.
    DATA(title) = sy-title.
    SET TITLEBAR 'TITLE' WITH title.
    SET PF-STATUS 'STATUS'.
    display_grid( ).
  ENDMETHOD.

  METHOD pai.
    o_view->check_changed_data( ).
    CASE sy-ucomm.
      WHEN 'BACK'. SET SCREEN 0.
      WHEN 'EXIT'. LEAVE PROGRAM.
      WHEN 'CANC'. SET SCREEN 0.
      WHEN 'SAVE'. save_changes( ).
      WHEN 'GDSM'. create_goods_movement( ).
    ENDCASE.
  ENDMETHOD.

  METHOD display_grid.

    DATA lt_fcat TYPE lvc_t_fcat.

    IF o_view IS NOT BOUND.

      CREATE OBJECT o_view
        EXPORTING
          i_lifetime        = cl_gui_control=>lifetime_dynpro
          i_parent          = cl_gui_custom_container=>screen0
*         i_appl_events     = abap_true "Triggers PAI/PBO on alv events
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      lt_fcat = zcl_gui=>build_lvc_fcat( zcl_akn_billdoc_update=>t_list ).
      lt_fcat = change_fcat( lt_fcat ).

      SET HANDLER on_double_click          FOR o_view.
      SET HANDLER on_data_changed_finished FOR o_view.

      o_view->register_edit_event(
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter
        EXCEPTIONS
          OTHERS     = 99 ).
      o_view->register_edit_event(
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          OTHERS     = 99 ).

      o_view->set_table_for_first_display(
        EXPORTING
        is_layout                     = VALUE #( zebra      = abap_true
                                                 cwidth_opt = abap_true
                                                 stylefname = 'STYLE'
*                                                 no_rowmark = abap_true
                                                 no_toolbar = abap_true
*                                                   sel_mode   = 'A' "Column and row selection, pushbuttons
                                                 sel_mode   = 'B' "Simple selection
*                                                   sel_mode   = 'C' "Multiple selection
*                                                   sel_mode   = 'D' "Cell selection
*                                                   excp_fname = 'LIGHT'
*                                                   excp_rolln = 'STAT_CHK'
                                                 info_fname = ''  )
        CHANGING
          it_outtab                     = zcl_akn_billdoc_update=>t_list
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

    ELSE.

      o_view->refresh_table_display(
        EXPORTING
          is_stable      = VALUE #( row = abap_true col = abap_true )
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).

    ENDIF.

  ENDMETHOD.

  METHOD change_fcat.
    DEFINE change_text.
      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m  = <ls_fcat>-scrtext_l = <ls_fcat>-reptext = &1.
    END-OF-DEFINITION.
    et_fcat = it_fcat.
    LOOP AT et_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CASE <ls_fcat>-fieldname.
        WHEN 'VBELV'.     change_text 'Teslimat no'(c01).
        WHEN 'WADAT_IST'. change_text 'Mal çıkış tarihi'(c02).
        WHEN 'ZZVEDOPTARIH'.
          <ls_fcat>-edit = abap_true.
          <ls_fcat>-ref_table = 'VBRK'.
          <ls_fcat>-ref_field = 'ZZVEDOPTARIH'.
        WHEN 'ZZBEYANNO'.    <ls_fcat>-edit = abap_true.
        WHEN 'ZZBEYANTARIH'.
          <ls_fcat>-edit = abap_true.
          <ls_fcat>-ref_table = 'VBRK'.
          <ls_fcat>-ref_field = 'ZZBEYANTARIH'.
        WHEN 'ZZCARDTYPE'.
          <ls_fcat>-edit = abap_true.
          <ls_fcat>-ref_table = 'VBRK'.
          <ls_fcat>-ref_field = 'ZZCARDTYPE'.
        WHEN 'VBTYP'.
          <ls_fcat>-ref_table = 'VBRK'.
          <ls_fcat>-ref_field = 'VBTYP'.
        WHEN 'RFBSK'.
          <ls_fcat>-ref_table = 'VBRK'.
          <ls_fcat>-ref_field = 'RFBSK'.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_changes.
    DATA(lt_return) = zcl_akn_billdoc_update=>save_changes( ).
  ENDMETHOD.

  METHOD create_goods_movement.

    o_view->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(lt_roid) ).

    DATA(lt_return) = zcl_akn_billdoc_update=>post_document( lt_roid ).

    IF line_exists( lt_return[ type = 'E' ] ).
      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = lt_return.
    ENDIF.

  ENDMETHOD.

  METHOD on_double_click.

    CASE e_column.
      WHEN 'LIGHT'.
        DATA(lt_return) = zcl_akn_billdoc_update=>get_messages( es_row_no-row_id ).
        IF lt_return IS NOT INITIAL.
          CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
            EXPORTING
              it_message = lt_return.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD on_data_changed_finished.
    LOOP AT et_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).
      zcl_akn_billdoc_update=>add_changed( <ls_good>-row_id ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
