DATA: lt_item  TYPE bapimeconf_t_item,
lt_conf  TYPE bapimeconf_t_detail,
lt_confx TYPE bapimeconf_t_detailx.

lt_item = VALUE #( FOR wa IN confirmation ( item_no = wa-item_no ) ).
lt_conf = VALUE #( FOR wa IN confirmation ( item_no        = wa-item_no
                                      conf_category  = 'AB'
                                      deliv_date_typ = 'D'
                                      deliv_date     = wa-deliv_date
                                      quantity       = wa-quantity ) ).
lt_confx = VALUE #( FOR wa IN confirmation ( item_no        = wa-item_no
                                       conf_category  = 'X'
                                       deliv_date     = 'X'
                                       quantity       = 'X' ) ).