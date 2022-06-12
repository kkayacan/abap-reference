FUNCTION ZFIO_GET_CHART_MANIFEST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VIEW_ID) TYPE  RSZWOBJID
*"  EXPORTING
*"     VALUE(ES_CHART) TYPE  ZTURNOVER_MANIFEST
*"----------------------------------------------------------------------

  TYPES: BEGIN OF sap_app,
          type TYPE string,
         END OF sap_app,
         BEGIN OF header_data_json,
           n TYPE string,
           u TYPE string,
           trend TYPE string,
           value_color TYPE string,
         END OF header_data_json,
         BEGIN OF header_data,
           json TYPE header_data_json,
         END OF header_data,
         BEGIN OF main_indicator,
           number TYPE string,
           unit TYPE string,
           trend TYPE string,
           state TYPE string,
         END OF main_indicator,
         BEGIN OF header,
           type TYPE string,
           data TYPE header_data,
           title TYPE string,
           main_indicator TYPE main_indicator,
         END OF header,
         BEGIN OF legend,
           visible TYPE xfeld,
           position TYPE string,
           alignment TYPE string,
         END OF legend,
         BEGIN OF data_label,
           visible TYPE xfeld,
           show_total TYPE xfeld,
         END OF data_label,
         BEGIN OF category_axis_text,
           visible TYPE xfeld,
         END OF category_axis_text,
         BEGIN OF value_axis_text,
           visible TYPE xfeld,
         END OF value_axis_text,
         BEGIN OF plot_area,
           data_label TYPE data_label,
           category_axis_text TYPE category_axis_text,
           value_axis_text TYPE value_axis_text,
         END OF plot_area,
         BEGIN OF title,
           visible TYPE xfeld,
         END OF title,
         BEGIN OF list,
           week TYPE string,
           revenue TYPE string,
           cost TYPE string,
           cost1 TYPE string,
           cost2 TYPE string,
           cost3 TYPE string,
           target TYPE string,
           budget TYPE string,
         END OF list,
         list_t TYPE STANDARD TABLE OF list WITH DEFAULT KEY,
         BEGIN OF content_data_json,
           list TYPE list_t,
         END OF content_data_json,
         BEGIN OF content_data,
           json TYPE content_data_json,
           path TYPE string,
         END OF content_data,
         BEGIN OF dimensions,
           label TYPE string,
           value TYPE string,
         END OF dimensions,
         dimensions_t TYPE STANDARD TABLE OF dimensions WITH DEFAULT KEY,
         BEGIN OF content,
           chart_type TYPE string,
           legend TYPE legend,
           plot_area TYPE plot_area,
           title TYPE title,
           measure_axis TYPE string,
           dimension_axis TYPE string,
           data TYPE content_data,
           dimensions TYPE dimensions_t,
           measures TYPE dimensions_t,
         END OF content,
         BEGIN OF sap_card,
           type TYPE string,
           header TYPE header,
           content TYPE content,
         END OF sap_card,
         BEGIN OF json,
           sap_app TYPE sap_app,
           sap_card TYPE sap_card,
         END OF json.

  DATA json TYPE json.

  json-sap_app-type = 'card'.
  json-sap_card-type = 'Analytical'.
  json-sap_card-header-type = 'Numeric'.
  json-sap_card-header-data-json-n = '43.2'.
  json-sap_card-header-data-json-u = '%'.
  json-sap_card-header-data-json-trend = 'Down'.
  json-sap_card-header-data-json-value_color = 'Good'.
  json-sap_card-header-title = 'Failure Breakdown - Q1, 2019'.
  json-sap_card-header-main_indicator-number = '{n}'.
  json-sap_card-header-main_indicator-unit = '{u}'.
  json-sap_card-header-main_indicator-trend = '{trend}'.
  json-sap_card-header-main_indicator-state = '{valueColor}'.
  json-sap_card-content-chart_type = 'Line'.
  json-sap_card-content-legend-visible = 'X'.
  json-sap_card-content-legend-position = 'Bottom'.
  json-sap_card-content-legend-alignment = 'Left'.
  json-sap_card-content-plot_area-data_label-visible = ''.
  json-sap_card-content-plot_area-data_label-show_total = ''.
  json-sap_card-content-plot_area-category_axis_text-visible = ''.
  json-sap_card-content-plot_area-value_axis_text-visible = ''.
  json-sap_card-content-title-visible = ''.
  json-sap_card-content-measure_axis = 'valueAxis'.
  json-sap_card-content-dimension_axis = 'categoryAxis'.
  json-sap_card-content-data-json-list =
  VALUE #( ( week = 'Weather'   revenue = '431000.22' cost = '230000.00' cost1 = '24800.63' cost2 = '205199.37' cost3 = '199999.37' target = '500000.00' budget = '210000.00' )
           ( week = 'Mechanics' revenue = '494000.30' cost = '238000.00' cost1 = '99200.39' cost2 = '138799.61' cost3 = '200199.37' target = '500000.00' budget = '224000.00' )
           ( week = 'Software'  revenue = '491000.17' cost = '221000.00' cost1 = '70200.54' cost2 = '150799.46' cost3 = '80799.46'  target = '500000.00' budget = '238000.00' ) ).
  json-sap_card-content-data-path = '/list'.
  json-sap_card-content-dimensions = VALUE #( ( label = 'Weeks' value = '{Week}' ) ).
  json-sap_card-content-measures = VALUE #( ( label = 'Revenue' value = '{Revenue}' )
                                            ( label = 'Cost' value = '{Cost}' )
                                            ( label = 'Cost' value = '{Cost2}' ) ).

  DATA(lt_map) = VALUE /ui2/cl_json=>name_mappings( ( abap = 'SAP_APP'  json = 'sap.app' )
                                                    ( abap = 'SAP_CARD' json = 'sap.card' ) ).
  es_chart-manifest = /ui2/cl_json=>serialize( data          = json
                                               compress      = abap_false
                                               pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                               name_mappings = lt_map ).

ENDFUNCTION.