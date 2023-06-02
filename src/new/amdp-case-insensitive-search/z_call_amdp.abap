FUNCTION zposae_store_value_help.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SEARCH) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(ET_STORES) TYPE  ZPOSAE_RETAILSTORES_T
*"----------------------------------------------------------------------

  IF iv_search IS INITIAL.
    SELECT werks AS retailstoreid, name1 AS retailstorename FROM zposdw_t001w_limited
      INTO CORRESPONDING FIELDS OF TABLE @et_stores
      ##db_feature_mode[amdp_table_function].

    LOOP AT et_stores ASSIGNING FIELD-SYMBOL(<ls_store>).
      <ls_store>-retailstoreid = |{ <ls_store>-retailstoreid ALPHA = IN }|.
    ENDLOOP.
  ELSE.
    zoaa_cl_common_amdp=>search_store(
      EXPORTING
        iv_search = iv_search
      IMPORTING
        et_stores = DATA(lt_result) ).
    ET_STORES = CORRESPONDING #( lt_result MAPPING retailstorename = name1 ) .
  ENDIF.

ENDFUNCTION.