SELECT * FROM ztf_posaggattrib( p_store = @lv_retailstoreid,  p_date = @lv_prevday )
INTO TABLE @DATA(lt_prevday)
##db_feature_mode[amdp_table_function].

SELECT * FROM ztf_posaggattrib( p_store = @lv_retailstoreid,  p_date = @iv_date )
INTO TABLE @DATA(lt_today)
##db_feature_mode[amdp_table_function].