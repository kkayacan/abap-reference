REPORT zmm_fuel_consumption.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

TYPES: BEGIN OF gst_list,
         light TYPE c LENGTH 1.
        INCLUDE TYPE zfuelconsumption.
TYPES:   report_type_x TYPE dd07t-ddtext,
         ports         TYPE c LENGTH 6,
         msg           TYPE bapiret2_tab,
         END OF gst_list.

TABLES: sscrfields, zfuelconsumption.

DATA: gt_list  TYPE TABLE OF gst_list,
      go_grid  TYPE REF TO cl_gui_alv_grid,
      gt_fuel  TYPE HASHED TABLE OF zfueltypes WITH UNIQUE KEY fuel_type,
      gv_ports TYPE gst_list-ports,
      gv_stop  TYPE abap_bool.

PARAMETERS: p_file RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND r01.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
PARAMETERS: p_fname TYPE ibipparms-path.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN SKIP 1.

PARAMETERS p_db RADIOBUTTON GROUP r01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME.
SELECT-OPTIONS: s_vessel FOR zfuelconsumption-vessel_name,
                s_date   FOR zfuelconsumption-calculation_date,
                s_type   FOR zfuelconsumption-report_type,
                s_lport  FOR zfuelconsumption-last_port,
                s_nport  FOR zfuelconsumption-next_port,
                s_ports  FOR gv_ports,
                s_mblnr  FOR zfuelconsumption-mblnr.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_posted RADIOBUTTON GROUP r02 DEFAULT 'X',
            p_np     RADIOBUTTON GROUP r02,
            p_all    RADIOBUTTON GROUP r02.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  IF sy-sysid = 'TU0'.
    MESSAGE 'UNDER CONSTRUCTION' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'Z_FUEL'
           ID 'ACTVT' FIELD '01'.
  IF sy-subrc = 0.
    sscrfields-functxt_01 = text-s01.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF p_file = abap_true.
    LOOP AT SCREEN.
      IF screen-name = 'P_FNAME'.
        screen-required = '2'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM file_open_dialog CHANGING p_fname.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lport-low.
  PERFORM get_port USING 'S_LPORT-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lport-high.
  PERFORM get_port USING 'S_LPORT-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_nport-low.
  PERFORM get_port USING 'S_NPORT-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_nport-high.
  PERFORM get_port USING 'S_NPORT-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ports-low.
  PERFORM get_ports USING 'S_PORTS-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ports-high.
  PERFORM get_ports USING 'S_PORTS-HIGH'.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZFUELTYPES'
        EXCEPTIONS
          OTHERS    = 99.
    WHEN 'ONLI'.
      IF p_file = abap_true AND p_fname IS INITIAL.
        MESSAGE e055(00).
*   Tüm zorunlu alanları doldurun
      ENDIF.
  ENDCASE.

START-OF-SELECTION.
  CASE abap_true.
    WHEN p_file.
      PERFORM read_file USING p_fname.
      PERFORM validate_and_save.
    WHEN p_db.
      PERFORM retrieve_data.
  ENDCASE.

END-OF-SELECTION.
  IF gv_stop = abap_false.
    CALL SCREEN 0001.
  ENDIF.

FORM file_open_dialog CHANGING cp_fname.

  DATA: lt_filetable TYPE filetable,
        lv_rc        TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = 'xlsx'
      file_filter             = '(*.xlsx)|*.xlsx|'
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

  TRY.
      cp_fname = lt_filetable[ 1 ]-filename.
    CATCH cx_root.
  ENDTRY.

ENDFORM.

FORM get_port USING p_fname.

  DATA: BEGIN  OF ls_help,
          port      LIKE zports-port,
          port_name LIKE zports-port_name,
        END  OF ls_help.

  STATICS lt_help LIKE TABLE OF ls_help.

  IF lt_help IS INITIAL.
    SELECT port port_name FROM zports INTO TABLE lt_help ORDER BY port.
  ENDIF.

  PERFORM call_value_request TABLES  lt_help
              USING  'PORT' 'PORT_NAME'  sy-dynnr  p_fname.

ENDFORM.

FORM get_ports USING p_fname.

  TYPES: BEGIN OF lst_ports,
           port TYPE zports-port,
         END OF lst_ports.

  DATA: BEGIN  OF ls_help,
          ports LIKE vtrdi-route,
        END  OF ls_help,
        lt_ports TYPE TABLE OF lst_ports.

  STATICS lt_help LIKE TABLE OF ls_help.

  IF lt_help IS INITIAL.
    SELECT port FROM zports INTO TABLE lt_ports.

    LOOP AT lt_ports ASSIGNING FIELD-SYMBOL(<ls_left>).
      LOOP AT lt_ports ASSIGNING FIELD-SYMBOL(<ls_right>) WHERE port <> <ls_left>-port.
        ls_help-ports = <ls_left>-port && <ls_right>-port.
        APPEND ls_help TO lt_help.
      ENDLOOP.
    ENDLOOP.

    SORT lt_help.
  ENDIF.

  PERFORM call_value_request TABLES  lt_help
              USING  'PORTS' ''  sy-dynnr  p_fname.

ENDFORM.

FORM call_value_request  TABLES pt_value_tab
                           USING    pv_retfield
                                    pv_pvalkey
                                    pv_dynpnr
                                    pv_dynprofield.

  DATA:
    lv_dynpnr      TYPE sydynnr,
    lv_dynprofield TYPE dynfnam.

  lv_dynpnr      = pv_dynpnr.
  lv_dynprofield = pv_dynprofield.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = pv_retfield
      pvalkey         = pv_pvalkey
      dynpprog        = sy-repid
      dynpnr          = lv_dynpnr
      dynprofield     = lv_dynprofield
      value_org       = 'S'
    TABLES
      value_tab       = pt_value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

FORM read_file USING ip_fname.

  TYPES: BEGIN OF lst_report_type,
           ddtext     TYPE dd07t-ddtext,
           domvalue_l TYPE dd07t-domvalue_l,
         END OF lst_report_type.

  DATA: lo_excel  TYPE REF TO zcl_excel,
        lo_reader TYPE REF TO zif_excel_reader.

  DATA: lo_worksheet      TYPE REF TO zcl_excel_worksheet,
        lv_highest_column TYPE zexcel_cell_column,
        lv_highest_row    TYPE int4,
        lv_column         TYPE zexcel_cell_column VALUE 1,
        lv_col_str        TYPE zexcel_cell_column_alpha,
        lv_row            TYPE int4 VALUE 1,
        lv_value          TYPE zexcel_cell_value.

  DATA: ls_fuel         TYPE gst_list,
        lt_report_types TYPE HASHED TABLE OF lst_report_type WITH UNIQUE KEY ddtext.

  CREATE OBJECT lo_reader TYPE zcl_excel_reader_2007.
  lo_excel = lo_reader->load_file( ip_fname ).

  lo_worksheet = lo_excel->get_active_worksheet( ).
  lv_highest_column = lo_worksheet->get_highest_column( ).
  lv_highest_row    = lo_worksheet->get_highest_row( ).

  SELECT ddtext, domvalue_l FROM  dd07t INTO TABLE @lt_report_types
         WHERE  domname     = 'ZREPORT_TYPE'
         AND    ddlanguage  = 'T'
         AND    as4local    = 'A'.

  WHILE lv_row <= lv_highest_row.
    IF lv_row = 2 OR lv_row = 5.
      WHILE lv_column <= lv_highest_column.
        lv_col_str = zcl_excel_common=>convert_column2alpha( lv_column ).
        lo_worksheet->get_cell(
          EXPORTING
            ip_column = lv_col_str
            ip_row    = lv_row
          IMPORTING
            ep_value = lv_value ).

        CASE lv_row.
          WHEN 2.
            CASE lv_column.
              WHEN 1. ls_fuel-calculation_date = zcl_excel_common=>excel_string_to_date( lv_value ).
              WHEN 2. ls_fuel-time = zcl_excel_common=>excel_string_to_time( lv_value ).
              WHEN 3. ls_fuel-vessel_name      = lv_value.
              WHEN 4. ls_fuel-voyage_number    = lv_value.
              WHEN 5.
                ls_fuel-report_type_x = lv_value.
                TRY .
                    ls_fuel-report_type = lt_report_types[ ddtext = lv_value ]-domvalue_l.
                  CATCH cx_root.
                ENDTRY.
              WHEN 6.  ls_fuel-fuel_type_1     = lv_value.
              WHEN 7.  ls_fuel-ft1_consumption = lv_value.
              WHEN 8.  ls_fuel-ft1_remain      = lv_value.
              WHEN 9.  ls_fuel-fuel_type_2     = lv_value.
              WHEN 10. ls_fuel-ft2_consumption = lv_value.
              WHEN 11. ls_fuel-ft2_remain      = lv_value.
              WHEN 12. ls_fuel-fuel_type_3     = lv_value.
              WHEN 13. ls_fuel-ft3_consumption = lv_value.
              WHEN 14. ls_fuel-ft3_remain      = lv_value.
              WHEN 15. ls_fuel-last_port_name  = lv_value.
              WHEN 16. ls_fuel-last_port       = lv_value.
              WHEN 17. ls_fuel-next_port_name  = lv_value.
              WHEN 18. ls_fuel-next_port       = lv_value.
              WHEN 19. ls_fuel-route           = lv_value.
            ENDCASE.
          WHEN 5.
            CASE lv_column.
              WHEN 1.  ls_fuel-speed         = lv_value.
              WHEN 2.  ls_fuel-sailing_time  = lv_value.
              WHEN 4.  ls_fuel-zzone         = lv_value.
              WHEN 5.  ls_fuel-dist          = lv_value.
              WHEN 6.  ls_fuel-zmode         = lv_value.
              WHEN 7.  ls_fuel-eng_percent   = lv_value.
              WHEN 8.  ls_fuel-cgo           = lv_value.
              WHEN 9.  ls_fuel-ball          = lv_value.
              WHEN 10. ls_fuel-reefr         = lv_value.
              WHEN 11. ls_fuel-wind_direct   = lv_value.
              WHEN 12. ls_fuel-wind_beaufort = lv_value.
              WHEN 13. ls_fuel-sea_direct    = lv_value.
              WHEN 14. ls_fuel-sea_beaufort  = lv_value.
              WHEN 15. ls_fuel-ztrim         = lv_value.
              WHEN 16. ls_fuel-gm            = lv_value.
              WHEN 17. ls_fuel-bm            = lv_value.
              WHEN 18. ls_fuel-zlat          = lv_value.
              WHEN 19. ls_fuel-zlong         = lv_value.
            ENDCASE.
        ENDCASE.
        lv_column = lv_column + 1.
      ENDWHILE.
    ENDIF.
    lv_column = 1.
    lv_row = lv_row + 1.
  ENDWHILE.

  ls_fuel-ports = ls_fuel-last_port && ls_fuel-next_port.
  APPEND ls_fuel TO gt_list.

ENDFORM.

FORM validate_and_save.

  DATA: ls_fuel      TYPE zfuelconsumption,
        lv_fuel_type TYPE zfueltypes-fuel_type,
        lv_datex     TYPE c LENGTH 10.

  gv_stop = abap_false.

  LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
    SELECT SINGLE cname, cdate, ctime, mblnr, mjahr
      FROM  zfuelconsumption INTO CORRESPONDING FIELDS OF @<ls_list>
           WHERE  vessel_name       = @<ls_list>-vessel_name
           AND    calculation_date  = @<ls_list>-calculation_date
           AND    report_type       = @<ls_list>-report_type.
    CASE sy-subrc.
      WHEN 0.
*        IF <ls_list>-mblnr IS NOT INITIAL.
*          <ls_list>-light = '3'.
        WRITE <ls_list>-calculation_date TO lv_datex DD/MM/YYYY.
        MESSAGE s005(zmb) WITH <ls_list>-vessel_name lv_datex <ls_list>-report_type_x DISPLAY LIKE 'E'.
*   &, &, & için kayıt var
        gv_stop = abap_true.
        RETURN.
*        ENDIF.
      WHEN OTHERS.

        SELECT SINGLE COUNT( * ) FROM  zvessels
               WHERE  vessel_name  = <ls_list>-vessel_name.
        IF sy-subrc <> 0.
          APPEND VALUE #( type = 'E' id = 'ZMB' number = '001'
                          message_v1 = <ls_list>-vessel_name ) TO <ls_list>-msg.
          <ls_list>-light = '1'.
        ENDIF.

        IF <ls_list>-report_type IS INITIAL.
          APPEND VALUE #( type = 'E' id = 'ZMB' number = '002'
                          message_v1 = <ls_list>-report_type_x ) TO <ls_list>-msg.
          <ls_list>-light = '1'.
        ENDIF.

        DO 3 TIMES VARYING lv_fuel_type FROM <ls_list>-fuel_type_1 NEXT <ls_list>-fuel_type_2.
          IF lv_fuel_type IS NOT INITIAL.
            SELECT SINGLE COUNT( * ) FROM  zfueltypes
                   WHERE  fuel_type  = lv_fuel_type.
            IF sy-subrc <> 0.
              APPEND VALUE #( type = 'E' id = 'ZMB' number = '003'
                              message_v1 = lv_fuel_type ) TO <ls_list>-msg.
              <ls_list>-light = '1'.
            ENDIF.
          ENDIF.
        ENDDO.

        IF <ls_list>-light IS INITIAL.
          <ls_list>-cname = sy-uname.
          <ls_list>-cdate = sy-datum.
          <ls_list>-ctime = sy-uzeit.
          MOVE-CORRESPONDING <ls_list> TO ls_fuel.
          INSERT zfuelconsumption FROM ls_fuel.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

FORM retrieve_data.

  CASE abap_true.
    WHEN p_posted.
      APPEND VALUE #( sign = 'E' option = 'EQ' low = space ) TO s_mblnr.
    WHEN p_np.
      APPEND VALUE #( sign = 'E' option = 'NE' low = space ) TO s_mblnr.
  ENDCASE.

  SELECT fuel~vessel_name, fuel~calculation_date, fuel~report_type, fuel~time,
         fuel~voyage_number, fuel~fuel_type_1, fuel~ft1_consumption, fuel~ft1_remain,
         fuel~fuel_type_2, fuel~ft2_consumption, fuel~ft2_remain, fuel~fuel_type_3,
         fuel~ft3_consumption, fuel~ft3_remain, fuel~last_port_name, fuel~last_port,
         fuel~next_port_name, fuel~next_port, fuel~route, fuel~speed, fuel~sailing_time,
         fuel~zzone, fuel~dist, fuel~zmode, fuel~eng_percent, fuel~cgo, fuel~ball, fuel~reefr,
         fuel~wind_direct, fuel~wind_beaufort, fuel~sea_direct, fuel~sea_beaufort,
         fuel~ztrim, fuel~gm, fuel~bm, fuel~zlat, fuel~zlong, fuel~cname, fuel~cdate,
         fuel~ctime, fuel~mblnr, fuel~mjahr, report~ddtext AS report_type_x,
         CASE fuel~mblnr WHEN @space THEN @space ELSE '3' END AS light
    FROM  zfuelconsumption AS fuel
    LEFT OUTER JOIN dd07t AS report ON report~domname    = 'ZREPORT_TYPE'
                                   AND report~ddlanguage = @sy-langu
                                   AND report~as4vers    = 'A'
                                   AND report~domvalue_l = fuel~report_type
    INTO CORRESPONDING FIELDS OF TABLE @gt_list
         WHERE  fuel~vessel_name      IN @s_vessel
         AND    fuel~calculation_date IN @s_date
         AND    fuel~report_type      IN @s_type
         AND    fuel~last_port        IN @s_lport
         AND    fuel~next_port        IN @s_nport
         AND    fuel~mblnr            IN @s_mblnr.

  LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
    <ls_list>-ports = <ls_list>-last_port && <ls_list>-next_port.
  ENDLOOP.

  DELETE gt_list WHERE ports NOT IN s_ports.

ENDFORM.

MODULE pbo OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.
  DATA(title) = sy-title.
  SET TITLEBAR 'TITLE' WITH title.
  AUTHORITY-CHECK OBJECT 'Z_FUEL'
           ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    APPEND 'DELE' TO fcode.
  ENDIF.
  SET PF-STATUS 'STATUS' EXCLUDING fcode.
  PERFORM display_grid.
ENDMODULE.

MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'. SET SCREEN 0.
    WHEN 'EXIT'. LEAVE PROGRAM.
    WHEN 'CANC'. SET SCREEN 0.
    WHEN 'SAVE'. PERFORM on_save.
    WHEN 'DELE'. PERFORM on_delete.
  ENDCASE.
ENDMODULE.

FORM display_grid.

  DATA lt_fcat TYPE lvc_t_fcat.

  IF go_grid IS NOT BOUND.

    CREATE OBJECT go_grid
      EXPORTING
        i_lifetime        = cl_gui_control=>lifetime_dynpro
        i_parent          = cl_gui_container=>screen0
*       i_appl_events     = abap_true "Triggers PAI/PBO on alv events
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM build_fcat USING gt_list CHANGING lt_fcat.
    PERFORM change_fcat CHANGING lt_fcat.

    SET HANDLER lcl_events=>on_double_click  FOR go_grid.
    SET HANDLER lcl_events=>on_hotspot_click FOR go_grid.

    go_grid->set_table_for_first_display(
      EXPORTING
        is_layout = VALUE #( cwidth_opt = abap_true
                             sel_mode   = COND #( WHEN p_file = abap_true THEN space
                                                  WHEN p_db   = abap_true THEN 'A' )
                             excp_fname = 'LIGHT'
                             excp_rolln = 'AD01STAT'
                             info_fname = ''  )
      CHANGING
        it_outtab                     = gt_list
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).

  ELSE.

    go_grid->refresh_table_display(
      EXPORTING
        is_stable      = VALUE #( row = abap_true col = abap_true )
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  ENDIF.

ENDFORM.

FORM build_fcat USING it_list TYPE ANY TABLE
             CHANGING ct_fcat TYPE lvc_t_fcat.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lo_salv)
    CHANGING
      t_table      = it_list ).

  ct_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = lo_salv->get_columns( )
        r_aggregations = lo_salv->get_aggregations( ) ).

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    <ls_fcat>-no_sign = abap_false.
  ENDLOOP.
ENDFORM.

FORM change_fcat CHANGING ct_fcat TYPE lvc_t_fcat.
  DEFINE change_text.
    <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m  = <ls_fcat>-scrtext_l = <ls_fcat>-reptext = &1.
  END-OF-DEFINITION.
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    CASE <ls_fcat>-fieldname.
      WHEN 'MANDT'.         <ls_fcat>-tech    = abap_true.
      WHEN 'REPORT_TYPE'.   <ls_fcat>-no_out  = abap_true.
      WHEN 'MBLNR'.         <ls_fcat>-hotspot = abap_true.
      WHEN 'REPORT_TYPE_X'. change_text 'Rapor tipi'(c01).
      WHEN 'PORTS'.         change_text 'Limanlar'(c02).
    ENDCASE.
  ENDLOOP.
  PERFORM move_column_after USING 'REPORT_TYPE_X' 'REPORT_TYPE' CHANGING ct_fcat.
  PERFORM move_column_after USING 'PORTS'         'NEXT_PORT'   CHANGING ct_fcat.
ENDFORM.

FORM move_column_after USING ip_col ip_target CHANGING ct_fcat TYPE lvc_t_fcat.

  DATA(lv_col_pos) = 0.
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    ADD 1 TO lv_col_pos.
    <ls_fcat>-col_pos = lv_col_pos.
  ENDLOOP.

  TRY.
      lv_col_pos = ct_fcat[ fieldname = ip_target ]-col_pos.
    CATCH cx_root.
      RETURN.
  ENDTRY.

  LOOP AT ct_fcat ASSIGNING <ls_fcat> WHERE col_pos > lv_col_pos.
    ADD 1 TO <ls_fcat>-col_pos.
  ENDLOOP.

  TRY.
      ASSIGN ct_fcat[ fieldname = ip_col ]-col_pos TO FIELD-SYMBOL(<lv_col_pos>).
      <lv_col_pos> = lv_col_pos + 1.
    CATCH cx_root.
      RETURN.
  ENDTRY.

  SORT ct_fcat BY col_pos.

ENDFORM.

FORM on_save.

  DATA ls_header TYPE bapi2017_gm_head_01.
  DATA lt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create.
  DATA lv_fuel_type   TYPE zfuelconsumption-fuel_type_1.
  DATA lv_consumption TYPE zfuelconsumption-ft1_consumption.
  DATA ls_fuel TYPE zfueltypes.
  DATA lv_quantity TYPE c LENGTH 20.

  go_grid->get_selected_rows(
    IMPORTING
      et_row_no = DATA(lt_roid) ).

  LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<ls_list>) WHERE light = space.

    DATA(lv_tabix) = sy-tabix.
    IF p_db = abap_true.
      CHECK line_exists( lt_roid[ row_id = lv_tabix ] ).
    ENDIF.

    CLEAR: ls_header, lt_item.

    ls_header-pstng_date     = ls_header-doc_date = <ls_list>-calculation_date.
    ls_header-bill_of_lading = <ls_list>-vessel_name.
    ls_header-header_txt     = <ls_list>-report_type_x.

    DO 3 TIMES VARYING lv_fuel_type   FROM <ls_list>-fuel_type_1     NEXT <ls_list>-fuel_type_2
               VARYING lv_consumption FROM <ls_list>-ft1_consumption NEXT <ls_list>-ft2_consumption.
      CHECK lv_fuel_type   IS NOT INITIAL AND
            lv_consumption IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-material = lv_fuel_type.
*      ASSIGN gt_fuel[ fuel_type = lv_fuel_type ] TO FIELD-SYMBOL(<ls_fuel>).
      SELECT SINGLE * FROM  zfueltypes INTO ls_fuel
             WHERE  fuel_type  = lv_fuel_type.
      <ls_item>-plant      = ls_fuel-werks.
      <ls_item>-stge_loc   = <ls_list>-vessel_name.
      IF lv_consumption >= 0.
        <ls_item>-move_type  = ls_fuel-cfb_bwart.
      ELSE.
        <ls_item>-move_type  = ls_fuel-bwart.
      ENDIF.
      <ls_item>-val_type   = <ls_list>-vessel_name.
      <ls_item>-entry_qnt  = abs( lv_consumption ).
      <ls_item>-entry_uom  = 'TO'.
      <ls_item>-costcenter = ls_fuel-kostl.
      <ls_item>-orderid    = <ls_list>-voyage_number.
      <ls_item>-profit_ctr = ls_fuel-prctr.
      <ls_item>-gl_account = ls_fuel-saknr.
      DATA(lv_menge) = CONV mseg-menge( lv_consumption ).
      WRITE lv_menge TO lv_quantity UNIT <ls_item>-entry_uom.
      CONDENSE lv_quantity.
      DATA(lv_length) = CONV i( 10 - strlen( lv_fuel_type ) ).
      <ls_item>-item_text  = <ls_list>-vessel_name  && ` ` && lv_fuel_type.
      DATA(lv_offset) = strlen( <ls_item>-item_text ) + lv_length + 1.
      <ls_item>-item_text+lv_offset = <ls_list>-ports.
      lv_length = 7 - strlen( lv_quantity ).
      <ls_item>-item_text = <ls_item>-item_text && ` ` && lv_quantity.
      lv_offset = strlen( <ls_item>-item_text ) + lv_length + 1.
      <ls_item>-item_text+lv_offset = 'TON'.
    ENDDO.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = VALUE bapi2017_gm_code( gm_code = '03' )
      IMPORTING
        materialdocument = <ls_list>-mblnr
        matdocumentyear  = <ls_list>-mjahr
      TABLES
        goodsmvt_item    = lt_item
        return           = <ls_list>-msg.

    LOOP AT <ls_list>-msg ASSIGNING FIELD-SYMBOL(<ls_msg>) WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    CASE sy-subrc.
      WHEN 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        <ls_list>-light = '1'.
      WHEN OTHERS.
        UPDATE zfuelconsumption
          SET mblnr = <ls_list>-mblnr
              mjahr = <ls_list>-mjahr
          WHERE vessel_name      = <ls_list>-vessel_name
          AND   calculation_date = <ls_list>-calculation_date
          AND   report_type      = <ls_list>-report_type.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        <ls_list>-light = '3'.
    ENDCASE.

  ENDLOOP.

ENDFORM.

FORM on_delete.

  go_grid->get_selected_rows(
    IMPORTING
      et_row_no = DATA(lt_roid) ).

  LOOP AT lt_roid ASSIGNING FIELD-SYMBOL(<ls_roid>).

    ASSIGN gt_list[ <ls_roid>-row_id ] TO FIELD-SYMBOL(<ls_list>).

    SELECT SINGLE COUNT( * ) FROM  mseg
           WHERE  mblnr  = <ls_list>-mblnr
           AND    mjahr  = <ls_list>-mjahr
           AND    smbln  = space .
    CASE sy-subrc.
      WHEN 0.
        APPEND VALUE #( type = 'E' id = 'ZMB' number = '004'
                        message_v1 = <ls_list>-mblnr ) TO <ls_list>-msg.
        <ls_list>-light = '1'.
      WHEN OTHERS.
        DELETE FROM zfuelconsumption WHERE vessel_name      = <ls_list>-vessel_name
                                     AND   calculation_date = <ls_list>-calculation_date
                                     AND   report_type      = <ls_list>-report_type.
        IF sy-subrc = 0.
          <ls_list>-mblnr = 'DELETE'.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  COMMIT WORK.
  DELETE gt_list WHERE mblnr = 'DELETE'.

ENDFORM.

CLASS lcl_events IMPLEMENTATION.

  METHOD on_double_click.
    IF e_column = 'LIGHT'.
      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = gt_list[ es_row_no-row_id ]-msg.
    ENDIF.
  ENDMETHOD.

  METHOD on_hotspot_click.

    ASSIGN gt_list[ es_row_no-row_id ] TO FIELD-SYMBOL(<ls_list>).
    IF <ls_list>-mblnr IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        i_action            = 'A04'
        i_refdoc            = 'R02'
        i_notree            = 'X'
        i_no_auth_check     = ' '
        i_deadend           = 'X'
        i_skip_first_screen = 'X'
        i_okcode            = 'OK_GO'
        i_mblnr             = <ls_list>-mblnr
        i_mjahr             = <ls_list>-mjahr.

  ENDMETHOD.

ENDCLASS.