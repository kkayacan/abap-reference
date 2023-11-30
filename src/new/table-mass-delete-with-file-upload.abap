REPORT ykk_mass_delete.

INCLUDE zabap2xlsx_simple.

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      start_of_selection,
      end_of_selection,
      file_open_dialog RETURNING VALUE(rv_file) TYPE string.
ENDCLASS.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      read_file
        IMPORTING ip_file TYPE csequence,
      read_excel
        IMPORTING ip_file TYPE csequence,
      read_text
        IMPORTING ip_file TYPE csequence,
      read_csv
        IMPORTING ip_file TYPE csequence,
      delete_lines.
  PROTECTED SECTION.
    TYPES: BEGIN OF st_field,
             fieldname TYPE name_feld,
             inttype   TYPE dfies-inttype,
             leng      TYPE dfies-leng,
             decimals  TYPE dfies-decimals,
             convexit  TYPE dfies-convexit,
           END OF st_field,
           tt_fields TYPE STANDARD TABLE OF st_field WITH DEFAULT KEY.
    CLASS-DATA: t_fields TYPE tt_fields,
                r_tab    TYPE REF TO data.
    CLASS-METHODS:
      get_field_attributes
        IMPORTING ip_table         TYPE csequence
                  VALUE(it_fields) TYPE tt_fields
        RETURNING VALUE(rt_fields) TYPE tt_fields,
      create_dynamic_table
        IMPORTING it_fields     TYPE tt_fields
        RETURNING VALUE(rr_tab) TYPE REF TO data,
      build_condition
        IMPORTING it_fields      TYPE tt_fields
                  is_line        TYPE any
        RETURNING VALUE(rv_cond) TYPE string.

ENDCLASS.

PARAMETERS p_file TYPE rlgrap-filename OBLIGATORY.
PARAMETERS p_table TYPE tabname16 DEFAULT 'SBOOK' NO-DISPLAY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = lcl_controller=>file_open_dialog( ).

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  lcl_controller=>end_of_selection( ).

CLASS lcl_controller IMPLEMENTATION.

  METHOD file_open_dialog.
    DATA: lt_file_tab TYPE filetable,
          lv_rc       TYPE i,
          lv_desktop  TYPE string.

    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING
        desktop_directory    = lv_desktop
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    cl_gui_cfw=>update_view( ).

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title         = ''
        file_filter          = 'Excel (*.xlsx)|*.XLSX|Tab Delimited Text (*.txt)|*.TXT|Comma Separated (*.csv)|*.CSV'
        initial_directory    = lv_desktop
      CHANGING
        file_table           = lt_file_tab
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    IF sy-subrc = 0 AND lt_file_tab IS NOT INITIAL.
      rv_file = lt_file_tab[ 1 ].
    ENDIF.

  ENDMETHOD.

  METHOD start_of_selection.
    "table check
    lcl_model=>read_file( p_file ).
  ENDMETHOD.

  METHOD end_of_selection.
    lcl_model=>delete_lines( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_model IMPLEMENTATION.
  METHOD read_file.

    DATA lv_extension TYPE c LENGTH 4.

    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = ip_file
      IMPORTING
        extension = lv_extension.

    CASE lv_extension.
      WHEN 'XLSX'. read_excel( ip_file ).
      WHEN 'TXT'.  read_text( ip_file ).
      WHEN 'CSV'.  read_csv( ip_file ).
    ENDCASE.

  ENDMETHOD.

  METHOD read_excel.

    DATA: excel  TYPE REF TO zcl_excel,
          reader TYPE REF TO zif_excel_reader.

    DATA: worksheet      TYPE REF TO zcl_excel_worksheet,
          highest_column TYPE zexcel_cell_column,
          highest_row    TYPE int4,
          column         TYPE zexcel_cell_column VALUE 1,
          col_str        TYPE zexcel_cell_column_alpha,
          row            TYPE int4               VALUE 1,
          value          TYPE zexcel_cell_value.

    DATA: ls_field LIKE LINE OF t_fields,
          lv_fm    TYPE rs38l_fnam.

    FIELD-SYMBOLS <lt_list> TYPE STANDARD TABLE.

    CREATE OBJECT reader TYPE zcl_excel_reader_2007.
    excel = reader->load_file( p_file ).

    worksheet = excel->get_active_worksheet( ).
    highest_column = worksheet->get_highest_column( ).
    highest_row    = worksheet->get_highest_row( ).

    WHILE row <= highest_row.
      WHILE column <= highest_column.
        col_str = zcl_excel_common=>convert_column2alpha( column ).
        worksheet->get_cell(
          EXPORTING
            ip_column = col_str
            ip_row    = row
          IMPORTING
            ep_value = value ).
        IF row = 1.
          ls_field-fieldname = value.
          APPEND ls_field TO t_fields.
          IF column = highest_column.
            t_fields = get_field_attributes( ip_table  = p_table
                                             it_fields = t_fields ).
            IF t_fields IS INITIAL.
               RETURN.
            ENDIF.
            r_tab = create_dynamic_table( t_fields ).
            ASSIGN r_tab->* TO <lt_list>.
          ENDIF.
        ELSE.
          IF column = 1.
            APPEND INITIAL LINE TO <lt_list> ASSIGNING FIELD-SYMBOL(<ls_list>).
          ENDIF.
          ASSIGN COMPONENT column OF STRUCTURE <ls_list> TO FIELD-SYMBOL(<lv_field>).
          READ TABLE t_fields INTO ls_field INDEX column.
          IF ls_field-inttype = 'D'.
            <lv_field> = zcl_excel_common=>excel_string_to_date( value ).
          ELSEIF ls_field-convexit IS NOT INITIAL.
            lv_fm = 'CONVERSION_EXIT_' && ls_field-convexit && '_INPUT'.
            CALL FUNCTION lv_fm
              EXPORTING
                input  = value
              IMPORTING
                output = <lv_field>.
          ELSE.
            <lv_field> = value.
          ENDIF.
        ENDIF.
        column = column + 1.
      ENDWHILE.
      column = 1.
      row = row + 1.
    ENDWHILE.

  ENDMETHOD.

  METHOD read_text.

  ENDMETHOD.

  METHOD read_csv.

  ENDMETHOD.

  METHOD delete_lines.

    DATA: condition TYPE string,
          count     TYPE i.

    FIELD-SYMBOLS: <lt_list> TYPE STANDARD TABLE,
                   <ls_line> TYPE any.

    ASSIGN r_tab->* TO <lt_list>.

    LOOP AT <lt_list> ASSIGNING <ls_line>.
      condition = build_condition( it_fields = t_fields is_line = <ls_line> ).
      DELETE FROM (p_table)
      WHERE (condition).
      ADD sy-dbcnt TO count.
    ENDLOOP.

    COMMIT WORK.

    WRITE: / count, 'records deleted'.

  ENDMETHOD.

  METHOD get_field_attributes.

    DATA lt_dfies    TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = CONV ddobjname( ip_table )
      TABLES
        dfies_tab      = lt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    LOOP AT it_fields INTO DATA(ls_field).
      READ TABLE lt_dfies INTO DATA(ls_dfies) WITH KEY fieldname = ls_field-fieldname.
      IF sy-subrc <> 0.
        WRITE: / icon_led_red AS ICON, 'Unknown column'.
        CLEAR rt_fields.
        RETURN.
      ENDIF.
      ls_field-inttype  = ls_dfies-inttype.
      ls_field-leng     = ls_dfies-leng.
      ls_field-decimals = ls_dfies-decimals.
      ls_field-convexit = ls_dfies-convexit.
      APPEND ls_field TO rt_fields.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_dynamic_table.

    DATA: lt_comp        TYPE cl_abap_structdescr=>component_table,
          lo_structdescr TYPE REF TO cl_abap_structdescr.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      <ls_comp>-name = <ls_field>-fieldname.
      CASE <ls_field>-inttype.
        WHEN 'I'. <ls_comp>-type = cl_abap_elemdescr=>get_i( ).
        WHEN 'b'. <ls_comp>-type = cl_abap_elemdescr=>get_int1( ).
        WHEN 's'. <ls_comp>-type = cl_abap_elemdescr=>get_int2( ).
        WHEN 'D'. <ls_comp>-type = cl_abap_elemdescr=>get_d( ).
        WHEN 'T'. <ls_comp>-type = cl_abap_elemdescr=>get_t( ).
        WHEN 'C'. <ls_comp>-type = cl_abap_elemdescr=>get_c( CONV i( <ls_field>-leng ) ).
        WHEN 'N'. <ls_comp>-type = cl_abap_elemdescr=>get_n( CONV i( <ls_field>-leng ) ).
        WHEN 'P'. <ls_comp>-type = cl_abap_elemdescr=>get_p( p_length = CONV i( <ls_field>-leng ) p_decimals = CONV i( <ls_field>-decimals ) ).
      ENDCASE.
    ENDLOOP.

    lo_structdescr = cl_abap_structdescr=>create( lt_comp ).
    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( lo_structdescr ).
    CREATE DATA rr_tab TYPE HANDLE lo_tabledescr.

  ENDMETHOD.

  METHOD build_condition.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE is_line TO FIELD-SYMBOL(<lv_val>).
      IF sy-subrc = 0.
        IF rv_cond IS NOT INITIAL.
          rv_cond = rv_cond && ` AND`.
        ENDIF.
        rv_cond = rv_cond  && ` ` && <ls_field>-fieldname && ` = '` && <lv_val> && `'`.
      ENDIF.
    ENDLOOP.

    CONDENSE rv_cond.

  ENDMETHOD.

ENDCLASS.