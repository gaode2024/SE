FUNCTION zde_con_pfm_fm_portfolio_mod.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_ATTRIBUTES) TYPE  /RPM/TT_PORTFOLIO_API OPTIONAL
*"     VALUE(IV_LOG_FLAG) TYPE  BOOLE_D OPTIONAL
*"     VALUE(IV_ACTION) TYPE  CHAR100 OPTIONAL
*"  EXPORTING
*"     VALUE(EV_RC) TYPE  I
*"  TABLES
*"      ET_MSG TYPE  /RPM/TT_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA:
    lr_portfolio          TYPE REF TO /rpm/cl_portfolio_api,
    lr_bucket             TYPE REF TO /rpm/cl_bucket_api,
    lr_msg_handler        TYPE REF TO /rpm/cl_message_handle,
    lr_error              TYPE REF TO cx_root,
    ls_key_with_language  TYPE /rpm/ts_portfolio_key_in,
    lt_keys_with_language TYPE /rpm/tt_portfolio_key_in,
    lt_headers_attributes TYPE /rpm/tt_portfolio_api,
    ls_header_attributes  TYPE /rpm/ts_portfolio_api,
    lt_select             TYPE /rpm/tt_selection_parameters,
    ls_select             TYPE sesf_selection_parameter,
    lt_buckets_attributes TYPE /rpm/tt_bucket_api,
    lt_bucket_api         TYPE /rpm/tt_bucket_api,
    ls_bucket_api         TYPE /rpm/ts_bucket_api,
    lv_rc                 TYPE i,
    lv_rejected           TYPE boolean,
    ls_attr               TYPE /rpm/ts_portfolio_api,
    ls_msg                TYPE /rpm/ts_messages,
    lt_msg                TYPE /rpm/tt_messages,
    ls_log_handle         TYPE balloghndl,
    lv_guid_x16           TYPE sysuuid_x16,
    lv_timestamp          TYPE tzntstmps,
    lv_num                TYPE numc5 VALUE 1,
    lv_num100             TYPE numc5,
    lt_logs               TYPE TABLE OF zcon_pfmt_frtlog,
    lv_language           TYPE laiso.

  CHECK it_attributes IS NOT INITIAL.
  lv_language = cl_rpm_language_buffer=>convert_langu_to_laiso( sy-langu ).
  CREATE OBJECT lr_msg_handler.
  lr_portfolio ?= /rpm/cl_portfolio_api=>get_instance( sy-langu ).
* call query to get buckets
  lr_bucket ?= /rpm/cl_bucket_api=>get_instance( sy-langu ).
  IF iv_log_flag EQ abap_true.
* Batch number
    lv_guid_x16 = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
* Batch timestamp
    GET TIME STAMP FIELD lv_timestamp.
  ENDIF.
  LOOP AT it_attributes ASSIGNING FIELD-SYMBOL(<ls_attributes>).
    CLEAR:lt_keys_with_language, lt_headers_attributes,ls_attr.
    ls_key_with_language-portfolio_guid = <ls_attributes>-portfolio_guid.
    ls_key_with_language-language       = lv_language.
    APPEND ls_key_with_language TO lt_keys_with_language.
*     Object details
    CALL METHOD lr_portfolio->/rpm/if_provider_access~retrieve
      EXPORTING
        in_bo_node_name   = lr_portfolio->ov_node_detail
        in_keys           = lt_keys_with_language
        in_edit_mode      = '2'
        in_message_buffer = lr_msg_handler
      IMPORTING
        out_data          = lt_headers_attributes.

    READ TABLE lt_headers_attributes INTO ls_header_attributes INDEX 1.
    IF sy-subrc = 0.
      ls_attr = ls_header_attributes.
    ELSE.
      CONTINUE.
    ENDIF.

    IF <ls_attributes>-zzforecast_period IS NOT INITIAL.
      ls_attr-zzforecast_period = <ls_attributes>-zzforecast_period.
      ls_attr-zzforecast_period_desc = <ls_attributes>-zzforecast_period_desc.
    ENDIF.
    IF <ls_attributes>-zzforecast_status IS NOT INITIAL.
      ls_attr-zzforecast_status = <ls_attributes>-zzforecast_status.
    ENDIF.
    CALL FUNCTION '/RPM/PORTFOLIO_MODIFY'
      EXPORTING
        is_attributes  = ls_attr
        iv_change_mode = /rpm/cl_co=>sc_change_mode_update
      IMPORTING
        ev_rc          = lv_rc
      TABLES
        et_msg         = lt_msg.
    IF lv_rc EQ 0 .
      CALL FUNCTION '/RPM/SAVE_CHANGES'
        IMPORTING
          ev_rc  = lv_rc
          et_msg = lt_msg.

* Free the objects
      CALL METHOD cl_inm_ppm_services=>do_cleanup( ).
    ENDIF.
    IF iv_log_flag EQ abap_true.
      PERFORM f_get_log_handle CHANGING ls_log_handle.
      PERFORM f_process_message USING lt_msg ls_log_handle.
    ENDIF.

*/Convert selection to esa selection
    ls_select-attribute_name = 'PORTFOLIO_GUID'.
    ls_select-sign           = 'I'.
    ls_select-option         = 'EQ'.
    ls_select-low            = ls_attr-portfolio_guid.
    APPEND ls_select TO lt_select.

    ls_select-attribute_name = 'ALL_ITEMS'.
    ls_select-sign           = 'I'.
    ls_select-option         = 'EQ'.
    ls_select-low            = 'X'.
    APPEND ls_select TO lt_select.

    ls_select-attribute_name = 'LANGUAGE'.
    ls_select-sign           = 'I'.
    ls_select-option         = 'EQ'.
    ls_select-low            = lv_language.
    APPEND ls_select TO lt_select.

    ls_select-attribute_name = 'SCOPE'.
    ls_select-sign           = 'I'.
    ls_select-option         = 'EQ'.
    ls_select-low            = '01'.
    APPEND ls_select TO lt_select.
* buckets data
    CLEAR lt_bucket_api.
    CALL METHOD lr_bucket->/rpm/if_provider_query~query
      EXPORTING
        in_bo_node_name         = /rpm/cl_bucket_api=>ov_node_detail
        in_query_name           = /rpm/cl_bucket_api=>ov_query_getlist
        in_selection_parameters = lt_select
        in_message_buffer       = lr_msg_handler
      IMPORTING
        out_data                = lt_bucket_api.
    DELETE lt_bucket_api WHERE external_id+0(2) <> 'PL'.
    LOOP AT lt_bucket_api ASSIGNING FIELD-SYMBOL(<ls_bucket_api>).
      IF <ls_attributes>-zzforecast_period IS NOT INITIAL.
        <ls_bucket_api>-zzforecast_period = <ls_attributes>-zzforecast_period.
        <ls_bucket_api>-zzforecast_period_desc = <ls_attributes>-zzforecast_period_desc.
      ENDIF.
      IF <ls_attributes>-zzforecast_status IS NOT INITIAL.
        <ls_bucket_api>-zzforecast_status = <ls_attributes>-zzforecast_status.
      ENDIF.
      APPEND <ls_bucket_api> TO lt_buckets_attributes.
      lv_num100 = lv_num100 + 1.
      IF lv_num100 EQ 100.
        CALL FUNCTION '/RPM/BUCKET_MODIFY'
          EXPORTING
            iv_language    = lv_language
            iv_change_mode = /rpm/cl_co=>sc_change_mode_update
*           iv_short_texts = iv_short_texts
*           iv_dx_report   = abap_true
          IMPORTING
            ev_rc          = lv_rc
          TABLES
            it_attributes  = lt_buckets_attributes
            et_msg         = lt_msg.
        IF lv_rc EQ 0 .
          CALL FUNCTION '/RPM/SAVE_CHANGES'
            IMPORTING
              ev_rc  = lv_rc
              et_msg = lt_msg.
* Free the objects
          CALL METHOD cl_inm_ppm_services=>do_cleanup( ).
        ENDIF.
        IF iv_log_flag EQ abap_true.
          PERFORM f_process_message USING lt_msg ls_log_handle.
        ENDIF.
        CLEAR:lv_num100, lt_buckets_attributes.
*        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_num100 GT 0.
      CALL FUNCTION '/RPM/BUCKET_MODIFY'
        EXPORTING
          iv_language    = lv_language
          iv_change_mode = /rpm/cl_co=>sc_change_mode_update
*         iv_short_texts = iv_short_texts
*         iv_dx_report   = abap_true
        IMPORTING
          ev_rc          = lv_rc
        TABLES
          it_attributes  = lt_buckets_attributes
          et_msg         = lt_msg.
      IF lv_rc EQ 0 .
        CALL FUNCTION '/RPM/SAVE_CHANGES'
          IMPORTING
            ev_rc  = lv_rc
            et_msg = lt_msg.
* Free the objects
        CALL METHOD cl_inm_ppm_services=>do_cleanup( ).
      ENDIF.
      IF iv_log_flag EQ abap_true.
        PERFORM f_process_message USING lt_msg ls_log_handle.
      ENDIF.
      CLEAR:lv_num100, lt_buckets_attributes.
    ENDIF.
    IF iv_log_flag EQ abap_true.
      APPEND INITIAL LINE TO lt_logs ASSIGNING FIELD-SYMBOL(<ls_log>).
      <ls_log>-guid = lv_guid_x16.
      <ls_log>-item_num = lv_num.
      <ls_log>-created_by = sy-uname.
      <ls_log>-created_on = lv_timestamp.
      <ls_log>-objectguid = ls_attr-guid.
      <ls_log>-objectid = ls_attr-external_id.
      <ls_log>-objectype = /rpm/cl_co=>sc_ot_portfolio.
      <ls_log>-log_handle = ls_log_handle.
      <ls_log>-action = iv_action.
      IF lv_rc EQ 0 .
        <ls_log>-icon = gc_icon_s.
      ELSE.
        <ls_log>-icon = gc_icon_e.
      ENDIF.
      <ls_log>-zzforecast_period = ls_attr-zzforecast_period.
      <ls_log>-zzforecast_status = ls_attr-zzforecast_status.
      lv_num = lv_num + 1.
      PERFORM f_add_log USING <ls_log>.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
