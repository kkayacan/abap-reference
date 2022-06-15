FUNCTION zfio_get_chart_manifest.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VIEW_ID) TYPE  RSZWOBJID
*"  EXPORTING
*"     VALUE(ES_CHART) TYPE  ZTURNOVER_MANIFEST
*"----------------------------------------------------------------------

  DATA: lt_cell TYPE rrws_t_cell,
        lt_axis TYPE rrws_thx_axis_data,
        lt_txt  TYPE rrws_t_text_symbols,
        lr_json TYPE REF TO data.

  DATA: lt_comp  TYPE cl_abap_structdescr=>component_table.

  CALL FUNCTION 'RRW3_GET_QUERY_VIEW_DATA' DESTINATION 'MBP_TRUSTED'
    EXPORTING
      i_view_id               = iv_view_id
    IMPORTING
      e_cell_data             = lt_cell
      e_axis_data             = lt_axis
      e_txt_symbols           = lt_txt
    EXCEPTIONS
      no_applicable_data      = 1
      invalid_variable_values = 2
      no_authority            = 3
      abort                   = 4
      invalid_input           = 5
      invalid_view            = 6
      OTHERS                  = 7.

  lt_comp = VALUE #( ( name = 'ID'   type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'TYPE' type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(sap_app) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'N'           type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'U'           type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'TREND'       type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'VALUE_COLOR' type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(header_data_json) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'JSON' type = header_data_json ) ).
  DATA(header_data) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'NUMBER' type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'UNIT'   type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'TREND'  type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'STATE'  type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(main_indicator) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'TYPE'           type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'DATA'           type = header_data )
                     ( name = 'TITLE'          type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'MAIN_INDICATOR' type = main_indicator ) ).
  DATA(header) = cl_abap_structdescr=>create( lt_comp ).

  DATA(xfeld) = CAST cl_abap_datadescr( cl_abap_elemdescr=>describe_by_name( 'XFELD' ) ).
  lt_comp = VALUE #( ( name = 'VISIBLE'   type = xfeld )
                     ( name = 'POSITION'  type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'ALIGNMENT' type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(legend) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'VISIBLE'    type = xfeld )
                     ( name = 'SHOW_TOTAL' type = xfeld ) ).
  DATA(data_label) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'VISIBLE'    type = xfeld ) ).
  DATA(axis_text) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'DATA_LABEL'         type = data_label )
                     ( name = 'CATEGORY_AXIS_TEXT' type = axis_text )
                     ( name = 'VALUE_AXIS_TEXT'    type = axis_text ) ).
  DATA(plot_area) = cl_abap_structdescr=>create( lt_comp ).

  READ TABLE lt_axis INTO DATA(ls_line) WITH KEY axis = '001'.
  LOOP AT ls_line-set ASSIGNING FIELD-SYMBOL(<line>) WHERE chavl_ext IS NOT INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
    LOOP AT ls_line-set ASSIGNING <line>.
      <line>-chavl_ext = <line>-chavl = <line>-caption.
    ENDLOOP.
  ENDIF.
  DELETE ls_line-set WHERE chavl_ext IS INITIAL.
  CLEAR lt_comp.
  APPEND VALUE #( name = 'MONTH' type = cl_abap_elemdescr=>get_string( ) ) TO lt_comp.
  DATA lv_fieldname TYPE name_feld.
  LOOP AT ls_line-set ASSIGNING <line> WHERE chavl_ext IS NOT INITIAL.
    PERFORM create_field_name USING <line>-chavl CHANGING lv_fieldname.
    APPEND VALUE #( name = lv_fieldname type = cl_abap_elemdescr=>get_string( ) ) TO lt_comp.
  ENDLOOP.
  DATA(list) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_comp ) ).

  lt_comp = VALUE #( ( name = 'LIST' type = list ) ).
  DATA(content_data_json) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'JSON' type = content_data_json )
                     ( name = 'PATH' type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(content_data) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'LABEL' type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'VALUE' type = cl_abap_elemdescr=>get_string( ) ) ).
  DATA(pair) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_comp ) ).

  lt_comp = VALUE #( ( name = 'CHART_TYPE'     type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'LEGEND'         type = legend )
                     ( name = 'PLOT_AREA'      type = plot_area )
                     ( name = 'TITLE'          type = axis_text )
                     ( name = 'MEASURE_AXIS'   type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'DIMENSION_AXIS' type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'DATA'           type = content_data )
                     ( name = 'DIMENSIONS'     type = pair )
                     ( name = 'MEASURES'       type = pair ) ).
  DATA(content) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'TYPE'    type = cl_abap_elemdescr=>get_string( ) )
                     ( name = 'HEADER'  type = header )
                     ( name = 'CONTENT' type = content ) ).
  DATA(sap_card) = cl_abap_structdescr=>create( lt_comp ).

  lt_comp = VALUE #( ( name = 'SAP_APP'  type = sap_app )
                     ( name = 'SAP_CARD' type = sap_card ) ).
  DATA(json) = cl_abap_structdescr=>create( lt_comp ).

  CREATE DATA lr_json TYPE HANDLE json.
  ASSIGN lr_json->* TO FIELD-SYMBOL(<json>).

  ASSIGN COMPONENT 'SAP_APP' OF STRUCTURE <json> TO FIELD-SYMBOL(<sap_app>).
  ASSIGN COMPONENT 'ID' OF STRUCTURE <sap_app> TO FIELD-SYMBOL(<val>).
  <val> = iv_view_id.
  ASSIGN COMPONENT 'TYPE' OF STRUCTURE <sap_app> TO <val>.
  <val> = 'card'.
  ASSIGN COMPONENT 'SAP_CARD' OF STRUCTURE <json> TO FIELD-SYMBOL(<sap_card>).
  ASSIGN COMPONENT 'TYPE' OF STRUCTURE <sap_card> TO <val>.
  <val> = 'Analytical'.
  ASSIGN COMPONENT 'HEADER' OF STRUCTURE <sap_card> TO FIELD-SYMBOL(<header>).
*  ASSIGN COMPONENT 'TYPE' OF STRUCTURE <header> TO <val>.
*  <val> = 'Numeric'.
*  ASSIGN COMPONENT 'DATA' OF STRUCTURE <header> TO FIELD-SYMBOL(<header_data>).
*  ASSIGN COMPONENT 'JSON' OF STRUCTURE <header_data> TO FIELD-SYMBOL(<header_data_json>).
*  ASSIGN COMPONENT 'N' OF STRUCTURE <header_data_json> TO <val>.
*  <val> = '43.2'.
*  ASSIGN COMPONENT 'U' OF STRUCTURE <header_data_json> TO <val>.
*  <val> = '%'.
*  ASSIGN COMPONENT 'TREND' OF STRUCTURE <header_data_json> TO <val>.
*  <val> = 'Down'.
*  ASSIGN COMPONENT 'VALUE_COLOR' OF STRUCTURE <header_data_json> TO <val>.
*  <val> = 'Good'.
  READ TABLE lt_txt INTO DATA(ls_txt) WITH KEY sym_name = 'REPTXTLG'.
  ASSIGN COMPONENT 'TITLE' OF STRUCTURE <header> TO <val>.
  <val> = ls_txt-sym_value.
*  ASSIGN COMPONENT 'MAIN_INDICATOR' OF STRUCTURE <header> TO FIELD-SYMBOL(<main_indicator>).
*  ASSIGN COMPONENT 'NUMBER' OF STRUCTURE <main_indicator> TO <val>.
*  <val> = '{n}'.
*  ASSIGN COMPONENT 'UNIT' OF STRUCTURE <main_indicator> TO <val>.
*  <val> = '{u}'.
*  ASSIGN COMPONENT 'TREND' OF STRUCTURE <main_indicator> TO <val>.
*  <val> = '{trend}'.
*  ASSIGN COMPONENT 'STATE' OF STRUCTURE <main_indicator> TO <val>.
*  <val> = '{valueColor}'.
  ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <sap_card> TO FIELD-SYMBOL(<content>).
  ASSIGN COMPONENT 'CHART_TYPE' OF STRUCTURE <content> TO <val>.
  <val> = 'Line'.
  ASSIGN COMPONENT 'LEGEND' OF STRUCTURE <content> TO FIELD-SYMBOL(<legend>).
  ASSIGN COMPONENT 'VISIBLE' OF STRUCTURE <legend> TO <val>.
  <val> = 'X'.
  ASSIGN COMPONENT 'POSITION' OF STRUCTURE <legend> TO <val>.
  <val> = 'Top'.
  ASSIGN COMPONENT 'ALIGNMENT' OF STRUCTURE <legend> TO <val>.
  <val> = 'Center'.
  ASSIGN COMPONENT 'MEASURE_AXIS' OF STRUCTURE <content> TO <val>.
  <val> = 'valueAxis'.
  ASSIGN COMPONENT 'DIMENSION_AXIS' OF STRUCTURE <content> TO <val>.
  <val> = 'categoryAxis'.
*  ASSIGN COMPONENT 'PLOT_AREA' OF STRUCTURE <content> TO FIELD-SYMBOL(<plot_area>).
*  ASSIGN COMPONENT 'DATA_LABEL' OF STRUCTURE <plot_area> TO FIELD-SYMBOL(<data_label>).
*  ASSIGN COMPONENT 'VISIBLE' OF STRUCTURE <data_label> TO <val>.
*  <val> = 'X'.
*  ASSIGN COMPONENT 'CATEGORY_AXIS_TEXT' OF STRUCTURE <plot_area> TO FIELD-SYMBOL(<category_axis_text>).
*  ASSIGN COMPONENT 'VISIBLE' OF STRUCTURE <category_axis_text> TO <val>.
*  <val> = 'X'.
*  ASSIGN COMPONENT 'VALUE_AXIS_TEXT' OF STRUCTURE <plot_area> TO FIELD-SYMBOL(<value_axis_text>).
*  ASSIGN COMPONENT 'VISIBLE' OF STRUCTURE <value_axis_text> TO <val>.
*  <val> = 'X'.
  ASSIGN COMPONENT 'DATA' OF STRUCTURE <content> TO FIELD-SYMBOL(<data>).
  ASSIGN COMPONENT 'JSON' OF STRUCTURE <data> TO FIELD-SYMBOL(<data_json>).
  FIELD-SYMBOLS <list> TYPE STANDARD TABLE.
  ASSIGN COMPONENT 'LIST' OF STRUCTURE <data_json> TO <list>.

  DATA lt_months TYPE STANDARD TABLE OF t247.
  CALL FUNCTION 'MONTH_NAMES_GET'
    TABLES
      month_names           = lt_months
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.

  DATA: cell_ordinal TYPE rrtcellordinal,
        set_start    TYPE rrtcellordinal,
        lv_month     TYPE n LENGTH 2.
  READ TABLE lt_axis INTO DATA(ls_val) WITH KEY axis = '000'.
  DELETE ls_val-set WHERE chavl_ext IS INITIAL.
  DATA(month_count) = lines( ls_val-set ).
  LOOP AT ls_val-set ASSIGNING FIELD-SYMBOL(<value>) WHERE chavl_ext IS NOT INITIAL.
    cell_ordinal = set_start.
    APPEND INITIAL LINE TO <list> ASSIGNING FIELD-SYMBOL(<item>).
    ASSIGN COMPONENT 'MONTH' OF STRUCTURE <item> TO <val>.
    IF strlen( <value>-chavl ) = 2.
      lv_month = <value>-chavl(2).
    ELSE.
      lv_month = <value>-chavl+4(2).
      DATA(add_year) = abap_true.
    ENDIF.
    READ TABLE lt_months ASSIGNING FIELD-SYMBOL(<ls_month>) WITH KEY mnr = lv_month.
    IF sy-subrc = 0.
      IF add_year = abap_true.
        <val> = <ls_month>-ltx && ` ` && <value>-chavl(4).
      ELSE.
        <val> = <ls_month>-ltx.
      ENDIF.
    ENDIF.
    LOOP AT ls_line-set ASSIGNING <line> WHERE chavl_ext IS NOT INITIAL.
*      DATA(field) = 'C' && <line>-tuple_ordinal.
      PERFORM create_field_name USING <line>-chavl CHANGING lv_fieldname.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <item> TO <val>.
      READ TABLE lt_cell ASSIGNING FIELD-SYMBOL(<cell>) WITH KEY cell_ordinal = cell_ordinal.
      IF sy-subrc = 0.
        <val> = <cell>-value.
      ENDIF.
      ADD month_count TO cell_ordinal.
    ENDLOOP.
    ADD 1 TO set_start.
  ENDLOOP.

  ASSIGN COMPONENT 'PATH' OF STRUCTURE <data> TO <val>.
  <val> = '/list'.

  FIELD-SYMBOLS <dimensions> TYPE STANDARD TABLE.
  ASSIGN COMPONENT 'DIMENSIONS' OF STRUCTURE <content> TO <dimensions>.
  APPEND INITIAL LINE TO <dimensions> ASSIGNING FIELD-SYMBOL(<dimension>).
  ASSIGN COMPONENT 'LABEL' OF STRUCTURE <dimension> TO <val>.
  <val> = 'Ay'.
  ASSIGN COMPONENT 'VALUE' OF STRUCTURE <dimension> TO <val>.
  <val> = '{month}'.

  DATA lv_pretty_name TYPE string.
  FIELD-SYMBOLS <measures> TYPE STANDARD TABLE.
  ASSIGN COMPONENT 'MEASURES' OF STRUCTURE <content> TO <measures>.
  LOOP AT ls_line-set ASSIGNING <line> WHERE chavl_ext IS NOT INITIAL.
    APPEND INITIAL LINE TO <measures> ASSIGNING FIELD-SYMBOL(<measure>).
    ASSIGN COMPONENT 'LABEL' OF STRUCTURE <measure> TO <val>.
    <val> = <line>-chavl.
    ASSIGN COMPONENT 'VALUE' OF STRUCTURE <measure> TO <val>.
    PERFORM create_field_name USING <line>-chavl CHANGING lv_fieldname.
    PERFORM pretty_name USING lv_fieldname CHANGING lv_pretty_name.
    <val> = '{' && lv_pretty_name && '}'.
  ENDLOOP.

  DATA(lt_map) = VALUE /ui2/cl_json=>name_mappings( ( abap = 'SAP_APP'  json = 'sap.app' )
                                                    ( abap = 'SAP_CARD' json = 'sap.card' ) ).
  es_chart-manifest = /ui2/cl_json=>serialize( data          = <json>
                                               compress      = abap_false
                                               pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                               name_mappings = lt_map ).

  es_chart-view_id = iv_view_id.

ENDFUNCTION.

FORM create_field_name USING iv_text CHANGING ev_field.

  ev_field = iv_text.

  REPLACE ALL OCCURRENCES OF ` ` IN ev_field WITH '_'.
  REPLACE ALL OCCURRENCES OF '/' IN ev_field WITH '_'.
  REPLACE ALL OCCURRENCES OF '#' IN ev_field WITH 'H'.
  REPLACE ALL OCCURRENCES OF 'ç' IN ev_field WITH 'c'.
  REPLACE ALL OCCURRENCES OF 'Ç' IN ev_field WITH 'C'.
  REPLACE ALL OCCURRENCES OF 'ğ' IN ev_field WITH 'g'.
  REPLACE ALL OCCURRENCES OF 'Ğ' IN ev_field WITH 'G'.
  REPLACE ALL OCCURRENCES OF 'ı' IN ev_field WITH 'i'.
  REPLACE ALL OCCURRENCES OF 'İ' IN ev_field WITH 'I'.
  REPLACE ALL OCCURRENCES OF 'ö' IN ev_field WITH 'o'.
  REPLACE ALL OCCURRENCES OF 'Ö' IN ev_field WITH 'O'.
  REPLACE ALL OCCURRENCES OF 'ş' IN ev_field WITH 's'.
  REPLACE ALL OCCURRENCES OF 'Ş' IN ev_field WITH 'S'.
  REPLACE ALL OCCURRENCES OF 'ü' IN ev_field WITH 'u'.
  REPLACE ALL OCCURRENCES OF 'Ü' IN ev_field WITH 'U'.
  SHIFT ev_field RIGHT DELETING TRAILING '_'.
  CONDENSE ev_field.
  IF ev_field(1) CA '1234567890'.
    ev_field = 'N' && ev_field.
  ENDIF.

ENDFORM.

FORM pretty_name USING in TYPE csequence CHANGING out TYPE string.

  DATA: tokens TYPE TABLE OF char128.
*        cache  LIKE LINE OF mt_name_mappings,

  FIELD-SYMBOLS: <token> LIKE LINE OF tokens.
*                 <cache> LIKE LINE OF mt_name_mappings.

*  READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
*  IF sy-subrc IS INITIAL.
*    out = <cache>-json.
*  ELSE.
  out = in.

  REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

  TRANSLATE out TO LOWER CASE.
  TRANSLATE out USING `/_:_~_`.
  SPLIT out AT `_` INTO TABLE tokens.
  LOOP AT tokens ASSIGNING <token> FROM 2.
    TRANSLATE <token>(1) TO UPPER CASE.
  ENDLOOP.

  CONCATENATE LINES OF tokens INTO out.
  REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

*    cache-abap  = in.
*    cache-json = out.
*    INSERT cache INTO TABLE mt_name_mappings.
*    INSERT cache INTO TABLE mt_name_mappings_ex.
*  ENDIF.

ENDFORM.

*  TYPES: BEGIN OF sap_app,
*          id   TYPE string,
*          type TYPE string,
*         END OF sap_app,
*         BEGIN OF header_data_json,
*           n TYPE string,
*           u TYPE string,
*           trend TYPE string,
*           value_color TYPE string,
*         END OF header_data_json,
*         BEGIN OF header_data,
*           json TYPE header_data_json,
*         END OF header_data,
*         BEGIN OF main_indicator,
*           number TYPE string,
*           unit TYPE string,
*           trend TYPE string,
*           state TYPE string,
*         END OF main_indicator,
*         BEGIN OF header,
*           type TYPE string,
*           data TYPE header_data,
*           title TYPE string,
*           main_indicator TYPE main_indicator,
*         END OF header,
*         BEGIN OF legend,
*           visible TYPE xfeld,
*           position TYPE string,
*           alignment TYPE string,
*         END OF legend,
*         BEGIN OF data_label,
*           visible TYPE xfeld,
*           show_total TYPE xfeld,
*         END OF data_label,
*         BEGIN OF category_axis_text,
*           visible TYPE xfeld,
*         END OF category_axis_text,
*         BEGIN OF value_axis_text,
*           visible TYPE xfeld,
*         END OF value_axis_text,
*         BEGIN OF plot_area,
*           data_label TYPE data_label,
*           category_axis_text TYPE category_axis_text,
*           value_axis_text TYPE value_axis_text,
*         END OF plot_area,
*         BEGIN OF title,
*           visible TYPE xfeld,
*         END OF title,
*         BEGIN OF list,
*           week TYPE string,
*           revenue TYPE string,
*           cost TYPE string,
*           cost1 TYPE string,
*           cost2 TYPE string,
*           cost3 TYPE string,
*           target TYPE string,
*           budget TYPE string,
*         END OF list,
*         list_t TYPE STANDARD TABLE OF list WITH DEFAULT KEY,
*         BEGIN OF content_data_json,
*           list TYPE list_t,
*         END OF content_data_json,
*         BEGIN OF content_data,
*           json TYPE content_data_json,
*           path TYPE string,
*         END OF content_data,
*         BEGIN OF dimensions,
*           label TYPE string,
*           value TYPE string,
*         END OF dimensions,
*         dimensions_t TYPE STANDARD TABLE OF dimensions WITH DEFAULT KEY,
*         BEGIN OF content,
*           chart_type TYPE string,
*           legend TYPE legend,
*           plot_area TYPE plot_area,
*           title TYPE title,
*           measure_axis TYPE string,
*           dimension_axis TYPE string,
*           data TYPE content_data,
*           dimensions TYPE dimensions_t,
*           measures TYPE dimensions_t,
*         END OF content,
*         BEGIN OF sap_card,
*           type TYPE string,
*           header TYPE header,
*           content TYPE content,
*         END OF sap_card,
*         BEGIN OF json,
*           sap_app TYPE sap_app,
*           sap_card TYPE sap_card,
*         END OF json.
*
*  DATA json TYPE json.
*
*  json-sap_app-id = iv_view_id.
*  json-sap_app-type = 'card'.
*  json-sap_card-type = 'Analytical'.
*  json-sap_card-header-type = 'Numeric'.
*  json-sap_card-header-data-json-n = '43.2'.
*  json-sap_card-header-data-json-u = '%'.
*  json-sap_card-header-data-json-trend = 'Down'.
*  json-sap_card-header-data-json-value_color = 'Good'.
*  json-sap_card-header-title = 'Failure Breakdown - Q1, 2019'.
*  json-sap_card-header-main_indicator-number = '{n}'.
*  json-sap_card-header-main_indicator-unit = '{u}'.
*  json-sap_card-header-main_indicator-trend = '{trend}'.
*  json-sap_card-header-main_indicator-state = '{valueColor}'.
*  json-sap_card-content-chart_type = 'Line'.
*  json-sap_card-content-legend-visible = 'X'.
*  json-sap_card-content-legend-position = 'Bottom'.
*  json-sap_card-content-legend-alignment = 'Left'.
*  json-sap_card-content-plot_area-data_label-visible = ''.
*  json-sap_card-content-plot_area-data_label-show_total = ''.
*  json-sap_card-content-plot_area-category_axis_text-visible = ''.
*  json-sap_card-content-plot_area-value_axis_text-visible = ''.
*  json-sap_card-content-title-visible = ''.
*  json-sap_card-content-measure_axis = 'valueAxis'.
*  json-sap_card-content-dimension_axis = 'categoryAxis'.
*  json-sap_card-content-data-json-list =
*  VALUE #( ( week = 'Weather'   revenue = '431000.22' cost = '230000.00' cost1 = '24800.63' cost2 = '205199.37' cost3 = '199999.37' target = '500000.00' budget = '210000.00' )
*           ( week = 'Mechanics' revenue = '494000.30' cost = '238000.00' cost1 = '99200.39' cost2 = '138799.61' cost3 = '200199.37' target = '500000.00' budget = '224000.00' )
*           ( week = 'Software'  revenue = '491000.17' cost = '221000.00' cost1 = '70200.54' cost2 = '150799.46' cost3 = '80799.46'  target = '500000.00' budget = '238000.00' ) ).
*  json-sap_card-content-data-path = '/list'.
*  json-sap_card-content-dimensions = VALUE #( ( label = 'Weeks' value = '{week}' ) ).
*  json-sap_card-content-measures = VALUE #( ( label = 'Revenue' value = '{revenue}' )
*                                            ( label = 'Cost' value = '{cost}' )
*                                            ( label = 'Cost' value = '{cost2}' ) ).
*
*  DATA(lt_map) = VALUE /ui2/cl_json=>name_mappings( ( abap = 'SAP_APP'  json = 'sap.app' )
*                                                    ( abap = 'SAP_CARD' json = 'sap.card' ) ).
*  es_chart-manifest = /ui2/cl_json=>serialize( data          = json
*                                               compress      = abap_false
*                                               pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
*                                               name_mappings = lt_map ).
*
*  es_chart-view_id = iv_view_id.