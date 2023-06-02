CLASS zoaa_cl_common_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
   INTERFACES if_amdp_marker_hdb.

   CLASS-METHODS :
      search_store IMPORTING VALUE(iv_search) TYPE string
                   EXPORTING VALUE(et_stores) TYPE ZPOSDW_TT_030.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zoaa_cl_common_amdp IMPLEMENTATION.

MEthod search_store by database PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
                    OPTIONS READ-ONLY.
  et_stores = SELECT werks, CASE WHEN ( LENGTH(LTRIM(werks,' 0123456789')) = 0 and LTRIM(werks,' ') <> '' )
THEN LPAD( werks,10,'0' ) ELSE werks END as retailstoreid, name1 FROM  "$ABAP.SCHEMA( /POSDW/SAP_ECC )".t001w
WHERE ( UPPER(werks) LIKE CONCAT(CONCAT('%', UPPER(:iv_search)), '%') )
                  OR ( UPPER(name1) LIKE CONCAT(CONCAT('%', UPPER(:iv_search)), '%') );
ENDMETHOD.
ENDCLASS.