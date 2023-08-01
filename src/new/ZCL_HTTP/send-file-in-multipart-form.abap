lv_response = zcl_http=>request(    ip_host      = host
                                    ip_endpoint  = lv_endpoint
                                    ip_token     = token
                                    ip_multipart = abap_true
                                    ip_file      = lv_file
                                    ip_file_name = 'request.zip' ).