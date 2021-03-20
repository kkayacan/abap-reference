FUNCTION z_mm_read_assets .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BE_CO_CODE) TYPE  MMPUR_D_BE_COCODE
*"     VALUE(IV_ANLN1) TYPE  ANLN1 OPTIONAL
*"     VALUE(ITR_ANLKL) TYPE  RANGE_ANLKL_IN_T OPTIONAL
*"     VALUE(ITR_AKTIV) TYPE  DATE_T_RANGE OPTIONAL
*"  EXPORTING
*"     VALUE(ET_ASSET) TYPE  ZTMM_ASSET
*"----------------------------------------------------------------------

  DATA(lv_destination) = zcl_mm_cproc_pr=>retrieve_destination( iv_be_co_code ).

  DATA lt_options     TYPE STANDARD TABLE OF rfc_db_opt.
  DATA lt_fields      TYPE STANDARD TABLE OF rfc_db_fld.
  DATA lt_data        TYPE STANDARD TABLE OF tab512.

  DATA lt_sel          TYPE rsdri_t_range.
  DATA lv_where_clause TYPE string.

  IF iv_anln1 IS NOT INITIAL AND iv_anln1 <> 'null'.
    APPEND VALUE #( chanm  = 'ANLN1'
                    sign   = 'I'
                    compop = 'EQ'
                    low    = iv_anln1 ) TO lt_sel.
  ENDIF.
  LOOP AT itr_anlkl ASSIGNING FIELD-SYMBOL(<ls_anlkl>).
    APPEND VALUE #( chanm  = 'ANLKL'
                    sign   = <ls_anlkl>-sign
                    compop = <ls_anlkl>-option
                    low    = <ls_anlkl>-low
                    high   = <ls_anlkl>-high ) TO lt_sel.
  ENDLOOP.
  LOOP AT itr_aktiv ASSIGNING FIELD-SYMBOL(<ls_aktiv>).
    APPEND VALUE #( chanm  = 'AKTIV'
                    sign   = <ls_aktiv>-sign
                    compop = <ls_aktiv>-option
                    low    = <ls_aktiv>-low
                    high   = <ls_aktiv>-high ) TO lt_sel.
  ENDLOOP.

  CALL FUNCTION 'RSAN_UT_FILTER_BUILD_SQL_WHERE'
    EXPORTING
      i_t_sel        = lt_sel
    IMPORTING
      e_where_clause = lv_where_clause.

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = CONV char30k( lv_where_clause )
      outputlen           = 72
    TABLES
      out_lines           = lt_options
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

  lt_fields = VALUE #( ( fieldname = 'ANLN1' )
                       ( fieldname = 'ANLN2' )
                       ( fieldname = 'ANLKL' )
                       ( fieldname = 'TXT50' )
                       ( fieldname = 'AKTIV' ) ).

  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION lv_destination
    EXPORTING
      query_table          = 'MMPUR_V_ASSET'
      delimiter            = '|'
    TABLES
      options              = lt_options
      fields               = lt_fields
      data                 = lt_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    APPEND INITIAL LINE TO et_asset ASSIGNING FIELD-SYMBOL(<ls_asset>).
    SPLIT <ls_data>-wa AT '|' INTO <ls_asset>-anln1 <ls_asset>-anln2 <ls_asset>-anlkl <ls_asset>-txt50  <ls_asset>-aktiv.
  ENDLOOP.

ENDFUNCTION.