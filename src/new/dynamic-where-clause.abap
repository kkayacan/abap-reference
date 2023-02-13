* NOT TESTED !!!

DATA : s_store TYPE RANGE OF zrt_satici_prim-magaza.
DATA : s_date  TYPE RANGE OF sy-datum.

APPEND VALUE #( sign   = 'I'
                option = 'EQ'
                low    = lv_magaza ) TO s_store.

APPEND VALUE #( sign   = 'I'
                option = 'EQ'
                low    = lv_tarih ) TO s_date.

DATA(lv_where) = cl_shdb_seltab=>combine_seltabs(
it_named_seltabs = VALUE #(
( name = 'RETAILSTOREID'    dref = REF #( s_store[] ) )
( name = 'BUSINESSDAYDATE'  dref = REF #( s_date[] ) )
)
) .