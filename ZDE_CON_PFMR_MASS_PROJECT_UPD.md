*&---------------------------------------------------------------------*
*& Report ZDE_CON_PFMR_MASS_PROJECT_UPD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zde_con_pfmr_mass_project_upd.

TABLES:sscrfields.

TYPES:
  BEGIN OF gty_s_upload,
     portfolio_id TYPE rpm_tv_extid
    ,proj_type  TYPE  zcpm_name60
    ,proj_id  TYPE  char40
    ,proj_desc  TYPE  zcpm_name60
    ,proj_sdate TYPE  zcpm_name60
    ,proj_edate TYPE  zcpm_name60
    ,zzfkstl TYPE  zcpm_name60
    ,proj_manager TYPE  zcpm_name60
    ,proj_controller TYPE  zcpm_name60
    ,pm_deputy TYPE  zcpm_name60
    ,pc_deputy TYPE  zcpm_name60
    ,syslead TYPE  zcpm_name60
    ,zzdev_status TYPE  zcpm_name60
    ,zzboard_name TYPE  zcpm_name60
    ,zzboard_date TYPE  zcpm_name60
    ,zzcustomer_vis TYPE  zcpm_name60
    ,zzcontract_typ TYPE  zcpm_name60
    ,zzext_cooper TYPE  zcpm_name60
    ,zzcooper_detail TYPE  zcpm_name60
    ,zzplan_field TYPE  zcpm_name60
    ,zzsub_planfield  TYPE  zcpm_name60
    ,zztech_center  TYPE  zcpm_name60
    ,zzsub_system TYPE  zcpm_name60
    ,
  END OF gty_s_upload.
TYPES:
  BEGIN OF gty_bucket_struc,
    item_type       TYPE rpm_tv_extid,
    zzplan_field    TYPE rpm_tv_extid,
    zzsub_planfield TYPE rpm_tv_extid,
    zztech_center   TYPE rpm_tv_extid,
    zzsub_system    TYPE rpm_tv_extid,
    guid_l2         TYPE rpm_tv_guid,
    guid_l3         TYPE rpm_tv_guid,
    guid_l4         TYPE rpm_tv_guid,
    guid_l5         TYPE rpm_tv_guid,
    guid_l6         TYPE rpm_tv_guid,
  END OF gty_bucket_struc .
TYPES:
  BEGIN OF gty_s_batch,
    guid       TYPE zcon_pfmt_prjlog-guid,
    created_by TYPE zcon_pfmt_prjlog-created_by,
    created_on TYPE zcon_pfmt_prjlog-created_on,
  END OF gty_s_batch.
TYPES:
  gtt_bucket_struc TYPE STANDARD TABLE OF gty_bucket_struc.
TYPES:
  BEGIN OF gty_output_data.
    INCLUDE TYPE /rpm/ts_item_d_api.
TYPES: project_key TYPE /bobf/conf_key,
    members     TYPE zcon_cpmtt_mp_role_member,
  END OF gty_output_data .
TYPES:
  gtt_output_data TYPE STANDARD TABLE OF gty_output_data
    WITH DEFAULT KEY .

DATA:
  gv_portfolio_guid TYPE /rpm/tv_guid,
  gs_proj_hdr       TYPE /cpd/s_mp_hdr_k,
  gt_bucket_api     TYPE /rpm/tt_bucket_api,
  gt_output_data    TYPE gtt_output_data,
  gt_bucket_struc   TYPE gtt_bucket_struc,
  gt_batchs         TYPE TABLE OF gty_s_batch,
  gt_fieldcat       TYPE lvc_t_fcat,
  gt_logs           TYPE STANDARD TABLE OF zcon_pfmt_prjlog,
  gt_upload         TYPE STANDARD TABLE OF gty_s_upload.

FIELD-SYMBOLS :
   <gt_data> TYPE STANDARD TABLE .

CONSTANTS: gc_ncol   TYPE i VALUE 22,
           gc_mode_c TYPE zpfm_mode VALUE 'Create'.

SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN: FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-b01.

  PARAMETERS:
      r_link RADIOBUTTON GROUP rgp DEFAULT 'X'  USER-COMMAND ucomm,
      r_upload RADIOBUTTON GROUP rgp MODIF ID m1
      .
  PARAMETERS:
    p_file TYPE rlgrap-filename MODIF ID m1,
    p_guid TYPE char32 AS LISTBOX VISIBLE LENGTH 24 MODIF ID m2.
  SELECT-OPTIONS:
          s_mpid FOR gs_proj_hdr-mp_id MODIF ID m2,
          s_mptype FOR gs_proj_hdr-mp_type NO INTERVALS MODIF ID m2.
  SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN END OF BLOCK bl01.

INITIALIZATION.
  PERFORM f_init_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_select_filename CHANGING p_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_set_screen.

AT SELECTION-SCREEN.
  PERFORM f_exe_button.

START-OF-SELECTION.
  PERFORM f_main_process.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form f_main_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_main_process .
  gv_portfolio_guid = p_guid.
* upload data
  IF r_upload EQ abap_true.
    PERFORM f_read_file.
    PERFORM f_process_file.
    PERFORM f_read_filedata.
  ELSEIF r_link EQ abap_true.
*  Read Data
    PERFORM f_read_cpmdata.
  ENDIF.
* check data
  PERFORM f_edit_data.
* project creation
  PERFORM f_create_item.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exe_button
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exe_button .
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM f_down_template.
    WHEN 'FC02'.
      PERFORM f_display_log.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save_path
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_FILE
*&---------------------------------------------------------------------*
FORM f_save_path  CHANGING p_lv_file.
  DATA: lv_window_title TYPE string,
        lv_file_filter  TYPE string,
        lv_file_name    TYPE string.

  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_type     TYPE string,
        lv_fullpath TYPE string.

  CLEAR p_lv_file.

  CONCATENATE 'Mass_bucket_creation_' sy-datum INTO lv_file_name. "File name

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension         = 'xlsx'
      default_file_name         = lv_file_name
    CHANGING
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.

  IF sy-subrc <> 0.

    MESSAGE TEXT-m02 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lv_fullpath IS NOT INITIAL.
    p_lv_file = lv_fullpath.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_down_template
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_down_template .
  DATA:
    lv_file  TYPE rlgrap-filename,
    lv_objid TYPE w3objid.

  PERFORM f_save_path
     CHANGING
       lv_file. "Save file path

  IF lv_file IS NOT INITIAL.

    lv_objid = 'ZCON_CPM_MASS_MPC'.       "file object
    PERFORM f_download_excel
      USING
        lv_objid
        lv_file.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_download_excel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_OBJID
*&      --> LV_FILE
*&---------------------------------------------------------------------*
FORM f_download_excel
USING
    uv_objid TYPE wwwdatatab-objid
    uv_file  TYPE rlgrap-filename.

  DATA: ls_objdata     TYPE wwwdatatab,
        lv_destination TYPE rlgrap-filename,
        lv_rc          TYPE sy-subrc,
        lv_message     TYPE bapi_msg.

  SELECT SINGLE relid objid
    FROM wwwdata
    INTO CORRESPONDING FIELDS OF ls_objdata
   WHERE srtf2 = 0
     AND relid   = 'MI'
     AND objid   = uv_objid.
  IF sy-subrc NE 0 OR ls_objdata-objid EQ space.
    lv_message =  TEXT-m01.    "Template &1 not exit,Please upload it using SWM0!
    REPLACE '&1' WITH uv_objid INTO lv_message.
    MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  lv_destination   = uv_file.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT' "Download function
    EXPORTING
      key         = ls_objdata
      destination = lv_destination
    IMPORTING
      rc          = lv_rc.

  IF lv_rc NE 0.
    lv_message = TEXT-m02.    " Template download failed!
    MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_read_file .
  DATA : lv_filename      TYPE string,
         lt_records       TYPE solix_tab,
         lv_headerxstring TYPE xstring,
         lv_filelength    TYPE i.

  lv_filename = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_filelength
      header                  = lv_headerxstring
    TABLES
      data_tab                = lt_records
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  "convert binary data to xstring
  "if you are using cl_fdt_xl_spreadsheet in odata then skips this step
  "as excel file will already be in xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filelength
    IMPORTING
      buffer       = lv_headerxstring
    TABLES
      binary_tab   = lt_records
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    "Implement suitable error handling here
  ENDIF.

  DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
        document_name = lv_filename
        xdocument     = lv_headerxstring ).
    CATCH cx_fdt_excel_core.
      "Implement suitable error handling here
  ENDTRY .

  "Get List of Worksheets
  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheets) ).

  IF NOT lt_worksheets IS INITIAL.
    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
      lv_woksheetname ).
    "now you have excel work sheet data in dyanmic internal table
    ASSIGN lo_data_ref->* TO <gt_data>.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_file .
  DATA : lv_numberofcolumns TYPE i,
         ls_upload          TYPE gty_s_upload.
  FIELD-SYMBOLS :
    <ls_data>  TYPE any,
    <lv_data>  TYPE any,
    <lv_field> TYPE any.

  "you could find out number of columns dynamically from table <gt_data>
  lv_numberofcolumns = gc_ncol .

  LOOP AT <gt_data> ASSIGNING <ls_data> FROM 3 .

    "processing columns
    DO lv_numberofcolumns TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data> TO <lv_data> .
      IF sy-subrc = 0 .
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_upload TO <lv_field>.
        IF sy-subrc = 0 .
          <lv_field> = <lv_data> .
        ENDIF.
      ENDIF.
    ENDDO .
    IF ls_upload-proj_type IS INITIAL AND ls_upload-proj_id IS INITIAL.
      EXIT.
    ENDIF.
    APPEND ls_upload TO gt_upload.
    CLEAR ls_upload.

    NEW-LINE .
  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_edit_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_edit_data .
  IF gv_portfolio_guid IS NOT INITIAL.
    CALL METHOD zde_cl_con_pfm_bucket_api_enh=>get_bucket_list
      EXPORTING
        iv_portfolio_guid = gv_portfolio_guid
      IMPORTING
        et_bucket_api     = gt_bucket_api.


    SELECT DISTINCT item_type,zzplan_field,zzsub_planfield,zztech_center,zzsub_system
          FROM @gt_output_data AS main
          INTO CORRESPONDING FIELDS OF TABLE @gt_bucket_struc.

    IF sy-subrc = 0 AND  gt_bucket_api IS NOT  INITIAL.
      SORT gt_bucket_api BY external_id.
      LOOP AT gt_bucket_struc ASSIGNING FIELD-SYMBOL(<ls_bucket_struc>).
        READ TABLE gt_bucket_api ASSIGNING FIELD-SYMBOL(<ls_bucket_api>)
        WITH KEY external_id = <ls_bucket_struc>-item_type BINARY SEARCH.
        IF sy-subrc <> 0.CONTINUE.ENDIF.
        <ls_bucket_struc>-guid_l2 = <ls_bucket_api>-bucket_guid.
        READ TABLE gt_bucket_api ASSIGNING <ls_bucket_api>
        WITH KEY external_id = <ls_bucket_struc>-zzplan_field parent_guid = <ls_bucket_struc>-guid_l2  BINARY SEARCH.
        IF sy-subrc <> 0.CONTINUE.ENDIF.
        <ls_bucket_struc>-guid_l3 = <ls_bucket_api>-bucket_guid.
        READ TABLE gt_bucket_api ASSIGNING <ls_bucket_api>
        WITH KEY external_id = <ls_bucket_struc>-zzsub_planfield parent_guid = <ls_bucket_struc>-guid_l3  BINARY SEARCH.
        IF sy-subrc <> 0.CONTINUE.ENDIF.
        <ls_bucket_struc>-guid_l4 = <ls_bucket_api>-bucket_guid.
        READ TABLE gt_bucket_api ASSIGNING <ls_bucket_api>
        WITH KEY external_id = <ls_bucket_struc>-zztech_center parent_guid = <ls_bucket_struc>-guid_l4  BINARY SEARCH.
        IF sy-subrc <> 0.CONTINUE.ENDIF.
        <ls_bucket_struc>-guid_l5 = <ls_bucket_api>-bucket_guid.
        READ TABLE gt_bucket_api ASSIGNING <ls_bucket_api>
        WITH KEY external_id = <ls_bucket_struc>-zzsub_system parent_guid = <ls_bucket_struc>-guid_l5  BINARY SEARCH.
        IF sy-subrc <> 0.CONTINUE.ENDIF.
        <ls_bucket_struc>-guid_l6 = <ls_bucket_api>-bucket_guid.
      ENDLOOP.
      DELETE gt_bucket_struc WHERE guid_l6 IS INITIAL.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_read_cpmdata .

  SELECT DISTINCT
   hdr~db_key AS project_key,
   hdr~mp_id AS external_id,
   hdr~proj_mgr_bupa_id AS zzpm_bupa_id,
   hdrt~text AS proj_description,
   hdr~start_date AS planned_start,
   hdr~end_date AS planned_finish,
   hdr~mp_stage AS status,
   hdr~proj_currency AS currency,
   hdr~zzext_cooper,
   hdr~zzcooper_detail,
   hdr~zzdev_status,
   hdr~zzboard_name,
   hdr~zzboard_date,
   hdr~zzcustomer_vis,
   hdr~zzcontract_typ,
   hdr~zzplan_field,
   hdr~zzsub_planfield,
   hdr~zztech_center,
   hdr~zzsub_system,
   org_cs~org_unit_key AS zzfkstl,
   hdr~mp_type AS item_type

 FROM /cpd/d_mp_hdr AS hdr
 LEFT OUTER JOIN /cpd/d_mp_hdr_s AS hdrt ON hdr~db_key = hdrt~parent_key
 LEFT JOIN /cpd/pwsc_orgpid AS org_cs   ON hdr~org_id = org_cs~org_unit_id AND org_cs~org_unit_typ = 'CS'
  INTO CORRESPONDING FIELDS OF TABLE @gt_output_data
  WHERE hdr~mp_id IN @s_mpid
   AND hdr~mp_type IN @s_mptype
   AND hdr~mp_id NOT IN ( SELECT external_id FROM /rpm/item_d AS portfolio_item )
  .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_item.
  TYPES:
  ty_change_mode TYPE c LENGTH 1 .
  TYPES:
    BEGIN OF ty_modifications,
      bo_node_name         TYPE string,
      key                  TYPE REF TO data,
      change_mode          TYPE ty_change_mode,
      data                 TYPE REF TO data,
      changed_fields       TYPE scol_string_t,
      association          TYPE string,
      source_bo_node       TYPE string,
      source_key           TYPE REF TO data,
      source_key_is_handle TYPE sesf_boolean,
    END OF ty_modifications .
  TYPES:
      tt_modifications TYPE STANDARD TABLE OF ty_modifications
  WITH DEFAULT KEY .
  DATA:
    lr_item                TYPE REF TO /rpm/cl_item_d_api,
    lr_link_api            TYPE REF TO /rpm/cl_object_link_api,
    lr_msg_handler         TYPE REF TO /rpm/cl_message_handle,
    ls_item_with_language  TYPE /rpm/ts_item_key_in,
    ls_short_text_key      TYPE /rpm/ts_comment_key,
    lt_modif               TYPE tt_modifications,
    ls_modif               TYPE ty_modifications,
    lv_authorized          TYPE boole_d,
    ls_context             TYPE /rpm/ts_object_hier,
    lv_msg                 TYPE string,
    lr_key                 TYPE REF TO data,
    lr_data                TYPE REF TO data,
    lr_short_text_key      TYPE REF TO data,
    lr_short_texts         TYPE REF TO data,
    lv_item_key_type       TYPE fieldname,
    lv_item_data_type      TYPE fieldname,
    lv_short_text_key_type TYPE fieldname,
    lv_rc                  TYPE i,
    ls_guid                TYPE /rpm/ts_guid,
    ls_attr                TYPE /rpm/ts_item_d_api,
    ls_item_attr           TYPE /rpm/ts_item_d_api,
    ls_msg                 TYPE /rpm/ts_messages,
    lt_msg                 TYPE /rpm/tt_messages,
    lv_language            TYPE laiso.

  FIELD-SYMBOLS:
    <fs_key>            TYPE any,
    <fs_data>           TYPE any,
    <fv_data>           TYPE any,
    <fs_short_text_key> TYPE any,
    <fv_short_texts>    TYPE any.
  CONSTANTS:
    lc_currency    TYPE waers_curc VALUE 'EUR',
    lc_unit        TYPE /rpm/tv_unit VALUE 'TAG',
    lc_period_type TYPE /rpm/tv_period_type VALUE '12',
    lc_org_id      TYPE /cpd/pws_ws_org_unit_id VALUE 'ZCAR',
    lc_icon_s      TYPE icon_d VALUE '@5B@',
    lc_icon_e      TYPE icon_d VALUE '@5C@'.

  lv_item_key_type        = '/RPM/TS_ITEM_KEY_IN'.
  lv_item_data_type       = /rpm/cl_item_d_api=>ov_item_api_type.
  lv_short_text_key_type  = /rpm/cl_object_api=>ov_comment_key_type.

  lv_language = cl_rpm_language_buffer=>convert_langu_to_laiso( sy-langu ).
* Check if portfolio and bucket are created.

  lr_item ?= /rpm/cl_item_d_api=>get_instance( ).
  lr_link_api ?= /rpm/cl_object_link_api=>get_instance( ).
  CREATE OBJECT lr_msg_handler.
  ls_context-portfolio_guid = gv_portfolio_guid.
  ls_context-parent_type = /rpm/cl_co=>sc_ot_bucket.
  ls_context-object_type = /rpm/cl_co=>sc_ot_item.
  LOOP AT gt_output_data ASSIGNING FIELD-SYMBOL(<ls_output_data>).
    READ TABLE gt_bucket_struc ASSIGNING FIELD-SYMBOL(<ls_bucket_struc>)
    WITH KEY
    item_type = <ls_output_data>-item_type
    zzplan_field = <ls_output_data>-zzplan_field
    zzsub_planfield = <ls_output_data>-zzsub_planfield
    zztech_center = <ls_output_data>-zztech_center
    zzsub_system = <ls_output_data>-zzsub_system.
    IF sy-subrc = 0.
      <ls_output_data>-parent_guid = <ls_bucket_struc>-guid_l6.
    ELSE.
      CONTINUE.
    ENDIF.
    ls_context-parent_guid = <ls_output_data>-parent_guid.
* Check 'Create' authorization on parent object
    CALL METHOD lr_item->check_authorization
      EXPORTING
        is_context    = ls_context
        iv_mode       = 'C'
      RECEIVING
        ev_authorized = lv_authorized.
* Not authorized for creation : log message and set ev_rc
    IF lv_authorized IS INITIAL.
      MESSAGE e040(/rpm/item) INTO lv_msg.
      RETURN.
    ENDIF.
* Set item attributes
    REFRESH lt_modif.
    CLEAR ls_modif.
    CLEAR: lr_key, lr_data.

    CREATE DATA lr_key TYPE (lv_item_key_type).
    ASSIGN lr_key->* TO <fs_key>.

* Build modification table
    MOVE-CORRESPONDING <ls_output_data> TO ls_attr.
    ls_attr-cap_periodtype = ls_attr-periodtype  = lc_period_type.
    ls_attr-unit  = lc_unit.
    ls_attr-portfolio_guid = gv_portfolio_guid.
    ls_attr-zzorg_id = lc_org_id.

    ls_item_with_language-language        = lv_language.
    ls_item_with_language-portfl_guid     = ls_attr-portfolio_guid.
    ls_item_with_language-bucket_guid     = ls_attr-parent_guid.
    ls_item_with_language-call_mode       = 'C'.

    <fs_key>     = ls_item_with_language.
    ls_modif-key = lr_key.
    ls_modif-bo_node_name = /rpm/cl_item_d_api=>ov_node_item_detail.
    ls_modif-change_mode  = 'C'.

    CREATE DATA lr_data TYPE (lv_item_data_type).
    ASSIGN lr_data->* TO <fs_data>.
    <fs_data> = ls_attr.
    ls_modif-data = lr_data.
    APPEND ls_modif TO lt_modif.
* Call modify in Create mode
    CALL METHOD lr_item->/rpm/if_provider_access~modify
      EXPORTING
        in_message_buffer = lr_msg_handler
      CHANGING
        in_modifications  = lt_modif.

    CLEAR ls_attr-guid.
    READ TABLE lt_modif INTO ls_modif WITH KEY change_mode = 'C'.
    IF sy-subrc = 0.
      ASSIGN ls_modif-data->* TO <fs_data>.
      ASSIGN COMPONENT 'GUID' OF STRUCTURE <fs_data> TO <fv_data>.
      IF sy-subrc = 0.
        ls_attr-guid = <fv_data>.
      ENDIF.
    ENDIF.

    lv_rc = lr_item->ov_rc.
    IF lv_rc NE 0 .
      RETURN.
    ENDIF.

    REFRESH lt_modif.
    CLEAR   ls_modif.
    CLEAR:  lr_short_texts.

* Short text Processing - short text key
    CREATE DATA lr_short_text_key TYPE (lv_short_text_key_type).
    ASSIGN lr_short_text_key->* TO <fs_short_text_key>.
    ls_short_text_key-guid = ls_attr-guid."lv_guid.
    ls_short_text_key-language = lv_language.
    <fs_short_text_key> =  ls_short_text_key.

    ls_modif-bo_node_name = /rpm/cl_item_d_api=>ov_node_short_text.
    ls_modif-change_mode  = 'U'.
    ls_modif-key = lr_short_text_key.

* short text data
    CREATE DATA lr_short_texts TYPE string.
    ASSIGN lr_short_texts->* TO <fv_short_texts>.
    <fv_short_texts>  = ls_attr-external_id.
    ls_modif-data = lr_short_texts.
    APPEND ls_modif TO lt_modif.
* Call modify in Update mode
    CALL METHOD lr_item->/rpm/if_provider_access~modify
      EXPORTING
        in_message_buffer = lr_msg_handler
      CHANGING
        in_modifications  = lt_modif.

    lv_rc = lr_item->ov_rc.

    CALL METHOD lr_msg_handler->get_all_messages
      RECEIVING
        et_messages = lt_msg.

    IF lv_rc NE 0 .
      RETURN.
    ENDIF.

* update RPM_OBJ_LINK database table
    CALL METHOD lr_link_api->create_cpm_object_link
      EXPORTING
        iv_item_guid = ls_attr-guid
        iv_mp_guid   = <ls_output_data>-project_key
      IMPORTING
        ev_rc        = lv_rc
        et_messages  = lt_msg.
    IF lv_rc NE 0 .
      RETURN.
    ENDIF.
    CALL FUNCTION '/RPM/SAVE_CHANGES'
      IMPORTING
        ev_rc  = lv_rc
        et_msg = lt_msg.

    IF lv_rc NE 0 .
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_init_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_screen .
  DATA:
    lv_functxt    TYPE smp_dyntxt,
    lv_subrc      TYPE sy-subrc,
    lv_id         TYPE vrm_id,
    lt_vrm_values TYPE vrm_values,
    ls_vrm_values TYPE vrm_value.
  SELECT PortfolioGuid , portfolio~Portfolioid , portfolio~portfoliotype
    FROM zcon_pfm_ddl_i_portfolio AS portfolio
  INTO TABLE @DATA(lt_data).
  IF sy-subrc = 0.
    LOOP AT lt_data INTO DATA(ls_data).
      ls_vrm_values-key = ls_data-PortfolioGuid.
      CONCATENATE ls_data-Portfolioid  ls_data-portfoliotype INTO ls_vrm_values-text SEPARATED BY space.
      APPEND ls_vrm_values TO lt_vrm_values.
    ENDLOOP.
    MOVE-CORRESPONDING lt_data TO lt_vrm_values.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'P_GUID'
        values = lt_vrm_values.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.
  "Template download
  lv_functxt-icon_id   = icon_export.
  lv_functxt-icon_text =  TEXT-001.
  sscrfields-functxt_01 = lv_functxt.
  "Display import log
  lv_functxt-icon_id   = icon_display.
  lv_functxt-icon_text =  TEXT-002.
  sscrfields-functxt_02 = lv_functxt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_filename
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM f_select_filename  CHANGING cv_file.
  DATA:
    ls_filetable TYPE file_table,
    lt_filetable TYPE filetable.

  DATA:
    lv_rc           TYPE i,
    lv_window_title TYPE string,
    lv_file_filter  TYPE string.

  lv_file_filter  = 'Excel(*.xlsx)|*.xlsx|Excel(*.xls)|*.xls'.
  ##SUBRC_OK
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = lv_file_filter
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_filetable INTO ls_filetable INDEX 1.
  IF sy-subrc EQ 0.
    cv_file = ls_filetable-filename.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_screen .
  LOOP AT SCREEN.
    IF r_upload EQ abap_true.
      IF screen-group1 EQ 'M2'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF r_link EQ abap_true.
      IF screen-group1 EQ 'M1'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_log .
  DATA:ls_layout TYPE lvc_s_layo,
       lv_title  TYPE lvc_title.
* Display basic information for all batchs
  SELECT DISTINCT guid,created_by,created_on FROM zcon_pfmt_prjlog
    INTO CORRESPONDING FIELDS OF TABLE @gt_batchs
    WHERE action = @gc_mode_c
  ORDER BY created_on DESCENDING.
  PERFORM f_build_fieldcat USING 'B'.
* ALV Output
  ls_layout-cwidth_opt = abap_true.
  lv_title = TEXT-003.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-cprog
      is_layout_lvc           = ls_layout
      i_grid_title            = lv_title
      it_fieldcat_lvc         = gt_fieldcat
      i_callback_user_command = 'F_DISPLAY_LOG_DATA'  "This is the subroutine name which is going to be called when user makes double click on basic list
    TABLES
      t_outtab                = gt_batchs
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
FORM f_display_log_data USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  DATA:ls_layout TYPE lvc_s_layo,
       lv_title  TYPE lvc_title.
  CASE r_ucomm.
    WHEN '&IC1'.
*     Display the details of a batch
      READ TABLE gt_batchs INTO DATA(ls_batch) INDEX rs_selfield-tabindex.
      IF  sy-subrc = 0.

* Display basic information for all batchs
        SELECT DISTINCT * FROM zcon_pfmt_bktlog
          INTO CORRESPONDING FIELDS OF TABLE @gt_logs
          WHERE guid = @ls_batch-guid
        ORDER BY item_num.
        PERFORM f_build_fieldcat USING 'D'.
* ALV Output
        ls_layout-cwidth_opt = abap_true.
        lv_title = TEXT-003.
        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
          EXPORTING
            i_callback_program      = sy-cprog
            is_layout_lvc           = ls_layout
            i_grid_title            = lv_title
            it_fieldcat_lvc         = gt_fieldcat
            i_callback_user_command = 'F_DISPLAY_LOG_DETAIL'  "This is the subroutine name which is going to be called when user makes double click on basic list
          TABLES
            t_outtab                = gt_logs
          EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
FORM f_display_log_detail USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  DATA:
    l_s_display_profile TYPE bal_s_prof,
    l_s_log_filter      TYPE bal_s_lfil,
    l_s_obj             TYPE bal_s_obj,
    l_s_extn            TYPE bal_s_extn,
    l_t_log_header      TYPE balhdr_t.
  CASE r_ucomm.
    WHEN '&IC1'.
*     Display the details of a batch
      READ TABLE gt_logs INTO DATA(ls_log) INDEX rs_selfield-tabindex.
      IF  sy-subrc = 0.
* create filter to search for this log on db
        CLEAR l_s_log_filter-log_handle.
*        CLEAR l_s_log_filter-object.
*        CLEAR l_s_obj.
*        l_s_obj-sign = 'I'.
*        l_s_obj-option = 'EQ'.
*        l_s_obj-low    = 'ZCON_PFM_SYN'.
*        APPEND l_s_obj TO l_s_log_filter-object.
        CLEAR l_s_extn.
        l_s_extn-sign = 'I'.
        l_s_extn-option = 'EQ'.
        l_s_extn-low    = ls_log-log_handle.
        APPEND l_s_extn TO l_s_log_filter-log_handle.
* search for this log
        CALL FUNCTION 'BAL_DB_SEARCH'
          EXPORTING
            i_s_log_filter = l_s_log_filter
          IMPORTING
            e_t_log_header = l_t_log_header
          EXCEPTIONS
            OTHERS         = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

* load these messages into memory
        CALL FUNCTION 'BAL_DB_LOAD'
          EXPORTING
            i_t_log_header         = l_t_log_header
            i_do_not_load_messages = abap_false  "read_from_db_hdr
          EXCEPTIONS
            OTHERS                 = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

* show this log:

* get display profile
        CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
          IMPORTING
            e_s_display_profile = l_s_display_profile
          EXCEPTIONS
            OTHERS              = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

* use grid for display if wanted
*        l_s_display_profile-use_grid = p_grid.

* set report to allow saving of variants
        l_s_display_profile-disvariant-report = sy-repid.
* when you use also other ALV lists in your report,
* please specify a handle to distinguish between the display
* variants of these different lists, e.g:
        l_s_display_profile-disvariant-handle = 'LOG'.

* call display function module
* We do not specify any filter (like I_S_LOG_FILTER, ...,
* I_T_MSG_HANDLE) since we want to display all logs available
        CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
          EXPORTING
            i_s_display_profile = l_s_display_profile
            i_s_log_filter      = l_s_log_filter
          EXCEPTIONS
            OTHERS              = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_set_field
  USING
    u_v_text      TYPE lvc_txtcol
    u_v_fieldname TYPE lvc_fname
    u_v_rfname    TYPE lvc_rfname
    u_v_rtname    TYPE lvc_rtname
    u_v_length    TYPE lvc_outlen
  CHANGING
    c_it_fieldcat TYPE lvc_t_fcat.

  DATA:
    ls_fieldcat TYPE lvc_s_fcat.

  ls_fieldcat-coltext   = u_v_text.
  ls_fieldcat-scrtext_l = u_v_text.
  ls_fieldcat-scrtext_m = u_v_text.
  ls_fieldcat-scrtext_s = u_v_text.
  ls_fieldcat-fieldname = u_v_fieldname.
  ls_fieldcat-outputlen = u_v_length.
  ls_fieldcat-ref_field = u_v_rfname.
  ls_fieldcat-ref_table = u_v_rtname.

  APPEND ls_fieldcat TO c_it_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_build_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_build_fieldcat USING p_flg.
  CLEAR gt_fieldcat.
  IF p_flg EQ 'B'.
    PERFORM f_set_field USING TEXT-t17 TEXT-f17    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t14 TEXT-f14    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t15 TEXT-f15    space space 30 CHANGING gt_fieldcat.
  ELSEIF p_flg EQ 'D'.
    PERFORM f_set_field USING TEXT-t01 TEXT-f01    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t02 TEXT-f02    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t03 TEXT-f03    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t04 TEXT-f04    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t05 TEXT-f05    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t06 TEXT-f06    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t07 TEXT-f07    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t08 TEXT-f08    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t09 TEXT-f09    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t10 TEXT-f10    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t11 TEXT-f11    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t12 TEXT-f12    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t13 TEXT-f13    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t14 TEXT-f14    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t15 TEXT-f15    space space 30 CHANGING gt_fieldcat.
    PERFORM f_set_field USING TEXT-t16 TEXT-f16    space space 30 CHANGING gt_fieldcat.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_filedata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_read_filedata .
  DATA: lo_guid         TYPE REF TO if_system_uuid,
        lv_guid_x16     TYPE sysuuid_x16,
        lv_timestamp    TYPE tzntstmps,
        lv_num          TYPE numc5 VALUE 1,
        ls_proj_details TYPE /cpd/pws_wss_mptype,
        lt_dfies        TYPE TABLE OF dfies,
        ls_dfies        LIKE LINE OF lt_dfies,
        lt_dfies_hdr    TYPE TABLE OF dfies,
        ls_dfies_hdr    LIKE LINE OF lt_dfies_hdr,
        ls_role_member  TYPE zcon_cpms_mp_role_member,
        lt_attr         TYPE /cpd/t_pws_ws_rep_attributes,
        ls_attr         TYPE /cpd/s_pws_ws_rep_attributes,
        ls_rep_attr     TYPE /cpd/s_mp_rep_attr,
        lv_field_attr   TYPE string,
        ls_upload       TYPE gty_s_upload,
        ls_output       TYPE gty_output_data.
  CONSTANTS:
    lc_separator               TYPE char1 VALUE ',',
    lc_role_z001               TYPE /cpd/pws_ws_role_id VALUE 'Z001',
    lc_role_z002               TYPE /cpd/pws_ws_role_id VALUE 'Z002',
    lc_role_z003               TYPE /cpd/pws_ws_role_id VALUE 'Z003',
    lc_role_z004               TYPE /cpd/pws_ws_role_id VALUE 'Z004',
    lc_role_z005               TYPE /cpd/pws_ws_role_id VALUE 'Z005',
    lc_flg_e                   TYPE zcpmflag VALUE 'E',
    lc_flg_s                   TYPE zcpmflag VALUE 'S',
    lc_fieldname_org_id        TYPE fieldname VALUE 'ORG_ID',
    lc_fieldname_start_date    TYPE fieldname VALUE 'START_DATE',
    lc_fieldname_end_date      TYPE fieldname VALUE 'END_DATE',
    lc_fieldname_proj_mgr      TYPE fieldname VALUE 'PROJ_MGR_BUPA_ID',
    lc_fieldname_proj_ctr      TYPE fieldname VALUE 'PROJ_MGR_BUPA_ID',
    lc_fieldname_pm_Deputy     TYPE fieldname VALUE 'PROJ_MGR_BUPA_ID',
    lc_fieldname_pc_Deputy     TYPE fieldname VALUE 'PROJ_MGR_BUPA_ID',
    lc_fieldname_ext_cooper    TYPE fieldname VALUE 'ZZEXT_COOPER',
    lc_fieldname_cooper_detail TYPE fieldname VALUE 'ZZCOOPER_DETAIL',
    lc_fieldname_dev_status    TYPE fieldname VALUE 'ZZDEV_STATUS',
    lc_fieldname_board_name    TYPE fieldname VALUE 'ZZBOARD_NAME',
    lc_fieldname_board_date    TYPE fieldname VALUE 'ZZBOARD_DATE',
    lc_fieldname_customer_vis  TYPE fieldname VALUE 'ZZCUSTOMER_VIS',
    lc_fieldname_contract_typ  TYPE fieldname VALUE 'ZZCONTRACT_TYP',
    lc_fieldname_plan_field    TYPE fieldname VALUE 'ZZPLAN_FIELD',
    lc_fieldname_sub_planfield TYPE fieldname VALUE 'ZZSUB_PLANFIELD',
    lc_fieldname_tech_center   TYPE fieldname VALUE 'ZZTECH_CENTER',
    lc_fieldname_sub_system    TYPE fieldname VALUE 'ZZSUB_SYSTEM',
    lc_fieldname_fkstl         TYPE fieldname VALUE 'ZZFKSTL',
    lc_domain_devstatus        TYPE domname VALUE 'ZDEV_STATUS',
    lc_domain_boardname        TYPE domname VALUE 'ZBOARD_NAME',
    lc_domain_customervis      TYPE domname VALUE 'ZCUSTOMER_VIS',
    lc_domain_contracttyp      TYPE domname VALUE 'ZCONTRACT_TYP'.
  FIELD-SYMBOLS:
    <ls_hdr>      TYPE any,
    <ls_rep_attr> TYPE any.
  CHECK gt_upload IS NOT INITIAL.
  SORT gt_upload BY proj_type.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = /cpd/cl_pws_ws_mp_bo_constant=>gc_cust_repo_attri_struct
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  "Project Type Master Data
  SELECT DISTINCT EngagementProjectType AS proj_type,MasterProjectTypeName AS name FROM i_engagementprojecttypetext AS _text
*    INNER JOIN @gt_upload AS _up ON _text~MasterProjectTypeName = _up~proj_type
    WHERE _text~Language = @sy-langu
    INTO TABLE @DATA(lt_text_proj_type).
  "Project Member
  SELECT DISTINCT
    businesspartner,
    businesspartnerrole,
    businesspartnerfullname
    FROM
      i_mstrprojbpcontact AS _bp
    WHERE  businesspartnerrole = @/cpd/cl_pws_ws_mp_bo_constant=>cv_member_emp_bupa
    INTO TABLE @DATA(lt_bp) .
  IF sy-subrc = 0.
    SORT lt_bp BY businesspartner.
  ENDIF.
  " External Cooperation
  SELECT DISTINCT
    _text~ExternalCooperation AS value,
    _text~ExternalCooperationName AS  text
  FROM zcon_cpm_ddl_c_extcpnvh AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~ExternalCooperationName = _up~zzext_cooper
    INTO TABLE @DATA(lt_text_ec).

  " Cooperation Detail
  SELECT DISTINCT
    _text~CooperationDetail AS value,
    _text~CooperationDetailName AS  text
  FROM zcon_cpm_ddl_c_cpndetvh AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~CooperationDetailName = _up~zzcooper_detail
  INTO TABLE @DATA(lt_text_cd).
  " Development Status
  SELECT DISTINCT
    _text~domainvalue AS value,
    _text~domaintext AS  text
  FROM i_domainfixedvaluetext AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~domaintext = _up~zzdev_status
  WHERE _text~sapdatadictionarydomain = @lc_domain_devstatus AND _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_ds).
  " Board Name
  SELECT DISTINCT
    _text~domainvalue AS value,
    _text~domaintext AS  text
  FROM i_domainfixedvaluetext AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~domaintext = _up~zzboard_name
  WHERE _text~sapdatadictionarydomain = @lc_domain_boardname AND _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_bn).
  " Customer Visibility
  SELECT DISTINCT
    _text~domainvalue AS value,
    _text~domaintext AS  text
  FROM i_domainfixedvaluetext AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~domaintext = _up~zzcustomer_vis
  WHERE _text~sapdatadictionarydomain = @lc_domain_customervis AND _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_cv).
  " Contract Type
  SELECT DISTINCT
    _text~domainvalue AS value,
    _text~domaintext AS  text
  FROM i_domainfixedvaluetext AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~domaintext = _up~zzcontract_typ
  WHERE _text~sapdatadictionarydomain = @lc_domain_contracttyp AND _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_ct).

  " Planning Field
  SELECT DISTINCT
    _text~planningfield  AS value,
    _text~planningfieldName AS  text
  FROM zcon_cpm_ddl_i_plfldt AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~planningfieldName = _up~zzplan_field
    WHERE  _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_pf).
  " Sub Planning Field
  SELECT DISTINCT
    _text~subplanningfield AS value,
    _text~subplanningfieldName AS  text
  FROM zcon_cpm_ddl_i_subpft AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~subplanningfieldName = _up~zzsub_planfield
    WHERE  _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_sp).
  " TC/CE
  SELECT DISTINCT
    _text~techcentercentralenabler AS value,
    _text~techcentercentralenablerName AS  text
  FROM zcon_cpm_ddl_i_tecctt AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~techcentercentralenablerName = _up~zztech_center
    WHERE  _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_tc).
  " Sub System
  SELECT DISTINCT
    _text~subsystem AS  value,
    _text~subsystemName AS  text
  FROM zcon_cpm_ddl_i_subsyt AS _text
  INNER JOIN @gt_upload AS _up
  ON _text~subsystemName = _up~zzsub_system
    WHERE  _text~language = @sy-langu
  INTO TABLE @DATA(lt_text_ss).
* External Cooperation/Cooperation Detail data napping
  SELECT ExternalCooperation,CooperationDetail AS value,CooperationDetailName AS text FROM zcon_cpm_ddl_c_cpndetvh
    INTO TABLE @DATA(lt_cpndetvh).

* Convert input data
  LOOP AT gt_upload INTO ls_upload.
    APPEND INITIAL LINE TO gt_output_data ASSIGNING FIELD-SYMBOL(<ls_proj_data>).

    <ls_proj_data>-external_id = ls_upload-proj_id.
    "Project Name
    <ls_proj_data>-proj_description = ls_upload-proj_desc.
    <ls_proj_data>-zzorg_id = 'CAR0'.
*   item type
    READ TABLE lt_text_proj_type INTO DATA(ls_text_proj_type) WITH KEY name  = ls_upload-proj_type.
    IF sy-subrc = 0.
      <ls_proj_data>-item_type = ls_text_proj_type-proj_type.
    ENDIF.

    IF ls_upload-proj_sdate IS NOT INITIAL.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = ls_upload-proj_sdate
        IMPORTING
          date_internal            = <ls_proj_data>-planned_start
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
    ENDIF.
    IF ls_upload-proj_edate IS NOT INITIAL.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = ls_upload-proj_edate
        IMPORTING
          date_internal            = <ls_proj_data>-planned_finish
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
    ENDIF.

    IF ls_upload-zzboard_date IS NOT INITIAL.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = ls_upload-zzboard_date
        IMPORTING
          date_internal            = ls_upload-zzboard_date
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
    ENDIF.
*     External Cooperation
    READ TABLE lt_text_ec INTO DATA(ls_text_ec) WITH KEY text = ls_upload-zzext_cooper.
    IF sy-subrc = 0.
      <ls_proj_data>-zzext_cooper = ls_text_ec-value.

*     External Cooperation / Cooperation Detail
      READ TABLE lt_cpndetvh INTO DATA(ls_cpndetvh) WITH KEY ExternalCooperation = ls_text_ec-value  text = ls_upload-zzcooper_detail .
      IF sy-subrc <> 0.
        <ls_proj_data>-zzcooper_detail = ls_cpndetvh-value.
      ENDIF.
    ENDIF.

    READ TABLE lt_text_ds INTO DATA(ls_text_ds) WITH KEY text = ls_upload-zzdev_status.
    IF sy-subrc = 0.
      <ls_proj_data>-zzdev_status = ls_text_ds-value.
    ENDIF.
    READ TABLE lt_text_bn INTO DATA(ls_text_bn) WITH KEY text = ls_upload-zzboard_name.
    IF sy-subrc = 0.
      <ls_proj_data>-zzboard_name = ls_text_bn-value.
    ENDIF.
    READ TABLE lt_text_cv INTO DATA(ls_text_cv) WITH KEY text = ls_upload-zzcustomer_vis.
    IF sy-subrc = 0.
      <ls_proj_data>-zzcustomer_vis = ls_text_cv-value.
    ENDIF.
    READ TABLE lt_text_ct INTO DATA(ls_text_ct) WITH KEY text = ls_upload-zzcontract_typ.
    IF sy-subrc = 0.
      <ls_proj_data>-zzcontract_typ = ls_text_ct-value.
    ENDIF.
    READ TABLE lt_text_pf INTO DATA(ls_text_pf) WITH KEY text = ls_upload-zzplan_field.
    IF sy-subrc = 0.
      <ls_proj_data>-zzplan_field = ls_text_pf-value.
    ENDIF.
    READ TABLE lt_text_sp INTO DATA(ls_text_sp) WITH KEY text = ls_upload-zzsub_planfield.
    IF sy-subrc = 0.
      <ls_proj_data>-zzsub_planfield = ls_text_sp-value.
    ENDIF.
    READ TABLE lt_text_tc INTO DATA(ls_text_tc) WITH KEY text = ls_upload-zztech_center.
    IF sy-subrc = 0.
      <ls_proj_data>-zztech_center = ls_text_tc-value.
    ENDIF.
    READ TABLE lt_text_ss INTO DATA(ls_text_ss) WITH KEY text = ls_upload-zzsub_system.
    IF sy-subrc = 0.
      <ls_proj_data>-zzsub_system = ls_text_ss-value.
    ENDIF.
    READ TABLE lt_bp INTO DATA(ls_bp) WITH KEY businesspartner = ls_upload-proj_manager BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_proj_data>-zzpm_bupa_id = ls_upload-proj_manager.
    ENDIF.
    IF ls_upload-proj_controller IS NOT INITIAL.
      READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = ls_upload-proj_controller BINARY SEARCH.
      IF sy-subrc = 0.
        ls_role_member-role_id = lc_role_z001.
        ls_role_member-mem_id = ls_upload-proj_controller.
        APPEND  ls_role_member TO <ls_proj_data>-members.
        CLEAR ls_role_member.
      ENDIF.
    ENDIF.
    IF ls_upload-pc_deputy IS NOT INITIAL.
      IF ls_upload-pc_deputy CS lc_separator.
        SPLIT ls_upload-pc_deputy AT lc_separator INTO TABLE DATA(lt_pcdeputys).
        LOOP AT lt_pcdeputys INTO DATA(lv_pcdeputy).
          READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = lv_pcdeputy BINARY SEARCH.
          IF sy-subrc = 0.
            ls_role_member-role_id = lc_role_z003.
            ls_role_member-mem_id = lv_pcdeputy.
            APPEND  ls_role_member TO <ls_proj_data>-members.
            CLEAR ls_role_member.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = ls_upload-pc_deputy BINARY SEARCH.
        IF sy-subrc = 0.
          ls_role_member-role_id = lc_role_z003.
          ls_role_member-mem_id = ls_upload-pc_deputy.
          APPEND  ls_role_member TO <ls_proj_data>-members.
          CLEAR ls_role_member.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_upload-pm_deputy IS NOT INITIAL.
      IF ls_upload-pm_deputy CS lc_separator.
        SPLIT ls_upload-pm_deputy AT lc_separator INTO TABLE DATA(lt_pmdeputys).
        LOOP AT lt_pmdeputys INTO DATA(lv_pmdeputy).
          READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = lv_pmdeputy BINARY SEARCH.
          IF sy-subrc = 0.
            ls_role_member-role_id = lc_role_z004.
            ls_role_member-mem_id = lv_pmdeputy.
            APPEND  ls_role_member TO <ls_proj_data>-members.
            CLEAR ls_role_member.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = ls_upload-pm_deputy BINARY SEARCH.
        IF sy-subrc = 0.
          ls_role_member-role_id = lc_role_z004.
          ls_role_member-mem_id = ls_upload-pm_deputy.
          APPEND  ls_role_member TO <ls_proj_data>-members.
          CLEAR ls_role_member.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_upload-syslead IS NOT INITIAL.
      READ TABLE lt_bp TRANSPORTING NO FIELDS WITH KEY businesspartner = ls_upload-syslead BINARY SEARCH.
      IF sy-subrc = 0.
        ls_role_member-role_id = lc_role_z005.
        ls_role_member-mem_id = ls_upload-syslead.
        APPEND  ls_role_member TO <ls_proj_data>-members.
        CLEAR ls_role_member.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
