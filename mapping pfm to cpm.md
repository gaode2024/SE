METHOD if_rpm_appl_plug_in_subsystem~initialize_after_save.

  DATA:
    lt_messages     TYPE inm_of_tt_messages,
    lt_cpm_messages TYPE inm_of_tt_messages,
    ls_messages     TYPE inm_of_ts_message,
    ls_message      TYPE inm_of_ts_message,
    lv_rc           TYPE i.

  DATA:
    ls_orig_opr_context       TYPE inm_operation_context,
    ls_item_opr_context       TYPE ty_inm_operation_context,
    ls_item_obj_context       TYPE inm_object_context,
    ls_dp_opr_context         TYPE ty_inm_operation_context,
    ls_dp_obj_context         TYPE inm_object_context,
    ls_initiative_opr_context TYPE ty_inm_operation_context,
    ls_initiative_obj_context TYPE inm_object_context,
    lt_templates              TYPE inm_tt_group_template_assoc,
    ls_template               TYPE inm_ts_group_template_assoc,
    lv_guid                   TYPE /rpm/tv_guid,
    lr_item_o                 TYPE REF TO cl_rpm_project_o,
    lv_lines                  TYPE i,
    lv_update_object          TYPE i,
    lv_tabix                  TYPE sy-tabix,
    ls_params                 TYPE inm_of_ts_field_name_value,
    lt_params                 TYPE inm_of_tt_field_names_values,
    lt_msg                    TYPE /rpm/tt_messages,
    lv_ret                    TYPE boole_d.

  DATA: ls_comm_proj_context TYPE ty_comm_proj_context,
        lv_item              TYPE /rpm/tv_guid.
  DATA:
        lr_dpr_subsys             TYPE REF TO if_dpr_appl_plug_in_subsystem.

  DATA lr_extract_factory     TYPE REF TO  if_eve_extract_factory.
  DATA lr_object_manager      TYPE REF TO cl_dpr_appl_object_manager.
  DATA lr_eve_extract_factory TYPE REF TO cl_eve_extract_factory.
  DATA: lv_item_count TYPE i.
  DATA ls_item_cp_template TYPE ty_item_cp_template.                                       "NOTE:2641436
  DATA: lt_items                 TYPE rpm_tt_projects.
*  IF sy-uname NE 'KSP'.
*    me->initialize_after_save_old( ).
*    RETURN.
*  ENDIF.

* check the object context of each object type
* if it is not initial then it means that synch has to happen else do nothing

* do check for initiative
  LOOP AT mt_initiatives_obj_context INTO ls_initiative_obj_context.

*    Below line is added to avoid getting into infinite saving for multiple creation
    cl_inm_dfm_object_integration=>sv_first = 'X'.

    IF ls_initiative_obj_context IS NOT INITIAL.

      READ TABLE mt_initiatives_opr_context WITH KEY guid = ls_initiative_obj_context-object_id
        INTO ls_initiative_opr_context.

* do be decided what to set depending on where inbound_synch_fw() is called
* if in changes committed then it is true
* if in prepare to save then it is false
      ls_initiative_opr_context-allow_commit_on_success = /rpm/cl_co=>sc_true.

* check if the operation is 'update' then at least attribute/status/authorization update happens
      IF ls_initiative_opr_context-operation = /rpm/cl_co=>sc_change_mode_update.
        IF ls_initiative_opr_context-no_auth_update = cl_rpm_co=>sc_true AND
          ls_initiative_opr_context-no_status_update = cl_rpm_co=>sc_true AND
          ls_initiative_opr_context-no_attribute_update = cl_rpm_co=>sc_true.

* all types of updates are disabled, then no need for synchronization
          CONTINUE.
        ENDIF.
      ENDIF.

* call the synch framework with the correct object and operation context
      MOVE-CORRESPONDING ls_initiative_opr_context TO ls_orig_opr_context.
      CALL FUNCTION 'DFM_INBOUND_SYNCH_FRAMEWORK'
        EXPORTING
          is_source_object_context = ls_initiative_obj_context
          is_operation_context     = ls_orig_opr_context
*         IT_TARGET_TEMPLATES      =
        IMPORTING
          et_messages              = lt_messages
          ev_rc                    = lv_rc.
    ENDIF.

    CLEAR cl_inm_dfm_object_integration=>sv_first.

* the et_messages contains empty text and hence there is a warning message in the UI
* so delete all rows where the msgtxt is not populated.
    DELETE lt_messages WHERE msgtxt IS INITIAL.
    DELETE lt_messages WHERE msgtxt = ''.
    DELETE lt_messages WHERE msgtxt = ' '.

*    IF lv_rc IS NOT INITIAL.
    LOOP AT lt_messages INTO ls_message.
      CALL METHOD cl_rpm_message_services=>message_add
        EXPORTING
          iv_msgty  = ls_message-msgty
          iv_msgid  = ls_message-msgid
          iv_msgno  = ls_message-msgno
          iv_msgv1  = ls_message-msgv1
          iv_msgv2  = ls_message-msgv2
          iv_msgv3  = ls_message-msgv3
          iv_msgv4  = ls_message-msgv4
          iv_msgtxt = ls_message-msgtxt.
    ENDLOOP.
*    ENDIF.
  ENDLOOP.
  CLEAR: mt_initiatives_obj_context, mt_initiatives_opr_context.

* do check for item

* Note 1424003 -->
  DESCRIBE TABLE mt_items_obj_context LINES lv_item_count.
  IF mt_items_obj_context[] IS INITIAL AND mv_eve_is_reset IS NOT INITIAL.
    CALL METHOD cl_dpr_appl_object_manager=>get_instance
      RECEIVING
        rr_instance = lr_object_manager.
    CALL METHOD lr_object_manager->get_eve_extract_factory
      RECEIVING
        rr_eve_extract_factory = lr_extract_factory.

    lr_eve_extract_factory ?= lr_extract_factory.
    IF lr_eve_extract_factory->is_active( ) = abap_false.
      CALL METHOD lr_eve_extract_factory->set_active
        EXPORTING
          iv_active = abap_true.
      CLEAR mv_eve_is_reset.
    ENDIF.
  ENDIF.
* <-- Note 1424003

  LOOP AT mt_items_obj_context INTO ls_item_obj_context.
    lv_tabix = sy-tabix.
* Clearing the templates should be done or else they cause a problem in multiple item/cproject create scenario
    CLEAR lt_templates.
    CLEAR ls_template.
    cl_inm_dfm_object_integration=>sv_first = 'X'.

    IF ls_item_obj_context IS NOT INITIAL.

      READ TABLE mt_items_opr_context WITH KEY guid = ls_item_obj_context-object_id
        INTO ls_item_opr_context.

*     If a target object needs to be created, call sync in creation mode
*      IF ms_target_template-type        IS NOT INITIAL AND
*         ms_target_template-template_id IS NOT INITIAL.

      IF ls_item_opr_context-operation = /rpm/cl_co=>sc_change_mode_create.
        "->>NOTE:2641436
*        MOVE-CORRESPONDING ms_target_template TO ls_template.
        READ TABLE mt_item_cp_template INTO ls_item_cp_template WITH KEY object_id = ls_item_obj_context-object_id.
        MOVE-CORRESPONDING ls_item_cp_template TO ls_template.
        "<<-NOTE:2641436
        APPEND ls_template TO lt_templates.
        " Commented clearing due to failure of target cpro creation in multiple item creation
*        CLEAR ms_target_template.

      ENDIF.

      ls_item_opr_context-allow_commit_on_success = /rpm/cl_co=>sc_true.

* check if the operation is 'update' then at least attribute/status/authorization update happens
      IF ls_item_opr_context-operation = /rpm/cl_co=>sc_change_mode_update.
        IF ls_item_opr_context-no_auth_update = cl_rpm_co=>sc_true AND
          ls_item_opr_context-no_status_update = cl_rpm_co=>sc_true AND
          ls_item_opr_context-no_attribute_update = cl_rpm_co=>sc_true.
          IF mt_comm_proj_context IS INITIAL.                  "CPM Project sync not required
* all types of updates are disabled, then no need for synchronization
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

* call the synch framework with the correct object and operation context
      MOVE-CORRESPONDING ls_item_opr_context TO ls_orig_opr_context.
* Note 1424003 -->
      IF mv_eve_is_reset IS NOT INITIAL AND lv_item_count EQ lv_tabix.

        CALL METHOD cl_dpr_appl_object_manager=>get_instance
          RECEIVING
            rr_instance = lr_object_manager.
        CALL METHOD lr_object_manager->get_eve_extract_factory
          RECEIVING
            rr_eve_extract_factory = lr_extract_factory.

        lr_eve_extract_factory ?= lr_extract_factory.
        IF lr_eve_extract_factory->is_active( ) = abap_false.
          CALL METHOD lr_eve_extract_factory->set_active
            EXPORTING
              iv_active = abap_true.
          CLEAR mv_eve_is_reset.
        ENDIF.
      ENDIF.
* <-- Note 1424003
* in case of delete operation, retrieve the project id and pass as it_params
* else call normally.
* fill the it_params only if source grouping is item and target grouping is project.
      IF ls_item_opr_context-operation = /rpm/cl_co=>sc_change_mode_delete
        AND ( ls_item_obj_context-target_object_grouping = cl_rpm_co=>sc_cpproject OR
              ls_item_obj_context-target_object_grouping = cl_rpm_co=>sc_psproject )
        AND ls_item_obj_context-grouping = /rpm/cl_co=>sc_rpm_item
        AND ls_item_obj_context-target_object_id IS NOT INITIAL.

        cl_inm_dfm_repository=>initialize_links( ).             " Note 2058696

        CLEAR ls_params.
        CLEAR lt_params.
        ls_params-ext_field_name = cl_inm_dfm_object_integration=>mc_proj_guid.
        ls_params-ext_field_value = ls_item_obj_context-target_object_id.
        APPEND ls_params TO lt_params.
        ls_params-ext_field_name = cl_inm_dfm_object_integration=>mc_proj_obl.
        ls_params-ext_field_value = ls_item_obj_context-target_object_link_type.
        APPEND ls_params TO lt_params.

        CALL FUNCTION 'DFM_INBOUND_SYNCH_FRAMEWORK'
          EXPORTING
            is_source_object_context = ls_item_obj_context
            is_operation_context     = ls_orig_opr_context
            it_params                = lt_params
          IMPORTING
            et_messages              = lt_messages
            ev_rc                    = lv_rc.

      ELSE.
        CALL FUNCTION 'DFM_INBOUND_SYNCH_FRAMEWORK'
          EXPORTING
            is_source_object_context = ls_item_obj_context
            is_operation_context     = ls_orig_opr_context
            it_target_templates      = lt_templates
          IMPORTING
            et_messages              = lt_messages
            ev_rc                    = lv_rc.

        " PPM-CPM Int
        IF ls_item_obj_context-grouping = /rpm/cl_co=>sc_rpm_item.
          lv_item = ls_item_obj_context-object_id.
          READ TABLE mt_comm_proj_context INTO ls_comm_proj_context WITH KEY object_id = lv_item.
          IF sy-subrc = 0 AND ls_comm_proj_context-operation = /rpm/cl_co=>sc_change_mode_create.
            mv_cpm_commit = abap_true.                                     "To commit object links(Item-CPM)
            DELETE mt_comm_proj_context WHERE object_id = lv_item.         "To avoid recursive calls during multiple item create scenario.

            CALL FUNCTION 'INM_CREATE_COMMERCIAL_PROJECT'
              EXPORTING
                iv_item_guid = ls_comm_proj_context-object_id
                iv_item_type = ls_comm_proj_context-object_type
                iv_mp_type   = ls_comm_proj_context-target_type
              IMPORTING
                et_messages  = lt_cpm_messages
                ev_rc        = lv_rc.
            IF lv_rc IS NOT INITIAL.
              mv_cpm_commit = abap_false.
            ENDIF.
            APPEND LINES OF lt_cpm_messages TO lt_messages.
          ENDIF.
          IF sy-subrc = 0 AND ls_comm_proj_context-operation = /rpm/cl_co=>sc_change_mode_update.
              CALL FUNCTION 'INM_UPDATE_COMMERCIAL_PROJECT'
              EXPORTING
                iv_item_guid           = ls_comm_proj_context-object_id
                iv_item_type           = ls_comm_proj_context-object_type
                iv_mp_type             = ls_comm_proj_context-target_type
                is_item_attr           = ls_comm_proj_context-item_attr
                is_item_attr_old       = ls_comm_proj_context-item_attr_old
             IMPORTING
               ET_MESSAGES            = lt_cpm_messages
               EV_RC                  = lv_rc.
             APPEND LINES OF lt_cpm_messages TO lt_messages.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR cl_inm_dfm_object_integration=>sv_first.

* the et_messages contains empty text and hence there is a warning message in the UI
* so delete all rows where the msgtxt is not populated.
      DELETE lt_messages WHERE msgtxt IS INITIAL.
      DELETE lt_messages WHERE msgtxt = ''.
      DELETE lt_messages WHERE msgtxt = ' '.

*      IF lv_rc IS NOT INITIAL.
      LOOP AT lt_messages INTO ls_message.
        CALL METHOD cl_rpm_message_services=>message_add
          EXPORTING
            iv_msgty  = ls_message-msgty
            iv_msgid  = ls_message-msgid
            iv_msgno  = ls_message-msgno
            iv_msgv1  = ls_message-msgv1
            iv_msgv2  = ls_message-msgv2
            iv_msgv3  = ls_message-msgv3
            iv_msgv4  = ls_message-msgv4
            iv_msgtxt = ls_message-msgtxt.
      ENDLOOP.

*     Target object creation is done - clear the template on the item reference
*      ELSE.
*        IF ls_item_opr_context-operation = 'C'.
*          lv_guid = ls_item_obj_context.
*          lr_item_o ?= get_item_reference( lv_guid ).
*          CLEAR lv_guid.
*          lr_item_o->set_cp_project_template_guid( lv_guid ).
*        ENDIF.
*      ENDIF.
    ENDIF.

  ENDLOOP.

  CLEAR: mt_items_obj_context, mt_items_opr_context.
  CLEAR mt_item_cp_template.                                                               "NOTE:2641436
  CLEAR mt_comm_proj_context.          " PPM-CPM Int

  IF mv_cpm_commit = abap_true.
* Trigger this save to commit CPM Object links
    mv_cpm_commit = abap_false.
    cl_inm_ppm_services=>save(
    IMPORTING
      et_messages = lt_msg
      ev_rejected = lv_ret ).
  ENDIF.
  "<<<<<<new changes for COMMIT

* do check for decision point
*  DESCRIBE TABLE mt_dps_obj_context LINES lv_lines.
  DATA:lr_synch_framework TYPE REF TO cl_inm_dfm_object_integration.
  DATA:lt_triggering_objects  TYPE inm_tt_object_context.


* get the one and only instance to the synch framework
  CALL METHOD cl_inm_dfm_object_integration=>get_instance
    RECEIVING
      rr_instance = lr_synch_framework.
* Get call stack from previous calls
  CALL METHOD lr_synch_framework->get_triggering_objects
    IMPORTING
      et_triggering_objects = lt_triggering_objects.
* only the last decision point that has an update should trigger a commit.
  CLEAR lv_update_object.
  LOOP AT mt_dps_obj_context INTO ls_dp_obj_context.
    lv_tabix = sy-tabix.

    IF ls_dp_obj_context IS NOT INITIAL.

      READ TABLE mt_dps_opr_context WITH KEY guid = ls_dp_obj_context-object_id
        INTO ls_dp_opr_context.

      IF ls_dp_opr_context-operation <> /rpm/cl_co=>sc_change_mode_update
        AND ls_dp_opr_context-operation <> /rpm/cl_co=>sc_change_mode_create
        AND ls_dp_opr_context-operation <> /rpm/cl_co=>sc_change_mode_delete.
        CONTINUE.
      ENDIF.

      IF ls_dp_opr_context-operation = /rpm/cl_co=>sc_change_mode_update.
        IF ls_dp_opr_context-no_auth_update = cl_rpm_co=>sc_true AND
          ls_dp_opr_context-no_status_update = cl_rpm_co=>sc_true AND
          ls_dp_opr_context-no_attribute_update = cl_rpm_co=>sc_true.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_triggering_objects TRANSPORTING NO FIELDS
      WITH KEY object_id = ls_dp_obj_context-object_id.
      IF sy-subrc NE 0.
        lv_update_object = lv_tabix.
      ENDIF.

    ENDIF.
  ENDLOOP.

  LOOP AT mt_dps_obj_context INTO ls_dp_obj_context.
    lv_tabix = sy-tabix.
    IF ls_dp_obj_context IS NOT INITIAL.

      READ TABLE mt_dps_opr_context WITH KEY guid = ls_dp_obj_context-object_id
        INTO ls_dp_opr_context.

      IF lv_tabix = lv_update_object.
        ls_dp_opr_context-allow_commit_on_success = /rpm/cl_co=>sc_true.
      ELSE.
        ls_dp_opr_context-allow_commit_on_success = /rpm/cl_co=>sc_false.
      ENDIF.

* check if the operation is 'update' then at least attribute/status/authorization update happens
      IF ls_dp_opr_context-operation = /rpm/cl_co=>sc_change_mode_update.
        IF ls_dp_opr_context-no_auth_update = cl_rpm_co=>sc_true AND
          ls_dp_opr_context-no_status_update = cl_rpm_co=>sc_true AND
          ls_dp_opr_context-no_attribute_update = cl_rpm_co=>sc_true.

* all types of updates are disabled, then no need for synchronization
          CONTINUE.
        ENDIF.
      ENDIF.

* call the synch framework with the correct object and operation context
      MOVE-CORRESPONDING ls_dp_opr_context TO ls_orig_opr_context.
      CALL FUNCTION 'DFM_INBOUND_SYNCH_FRAMEWORK'
        EXPORTING
          is_source_object_context = ls_dp_obj_context
          is_operation_context     = ls_orig_opr_context
        IMPORTING
          et_messages              = lt_messages
          ev_rc                    = lv_rc.

* the et_messages contains empty text and hence there is a warning message in the UI
* so delete all rows where the msgtxt is not populated.
      DELETE lt_messages WHERE msgtxt IS INITIAL.
      DELETE lt_messages WHERE msgtxt = ''.
      DELETE lt_messages WHERE msgtxt = ' '.

*      IF lv_rc IS NOT INITIAL.
      LOOP AT lt_messages INTO ls_message.
        CALL METHOD cl_rpm_message_services=>message_add
          EXPORTING
            iv_msgty  = ls_message-msgty
            iv_msgid  = ls_message-msgid
            iv_msgno  = ls_message-msgno
            iv_msgv1  = ls_message-msgv1
            iv_msgv2  = ls_message-msgv2
            iv_msgv3  = ls_message-msgv3
            iv_msgv4  = ls_message-msgv4
            iv_msgtxt = ls_message-msgtxt.
      ENDLOOP.
*      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR: mt_dps_obj_context, mt_dps_opr_context.

* clear the contents of the context tables after processing once
*  IF lv_rc IS INITIAL.
  CLEAR:
  mt_items_obj_context,
  mt_items_opr_context,
  mt_dps_obj_context,
  mt_dps_opr_context,
  mt_initiatives_obj_context,
  mt_initiatives_opr_context,
  ms_target_template.

  CLEAR:
    mt_init_status,
    mt_acls_changed,
    mt_initiative_attr_old,
    mt_project_data.

* Initialize the contents of lt_trigerring_objects also once all synch processing is complete
  lr_synch_framework->initialize( ).

* Free up the object contexts from the DPR subsystem when all synch objects are initialized
  lr_dpr_subsys = cl_dpr_inm_outbound=>if_dpr_appl_plug_in_subsystem~get_instance( ).
  lr_dpr_subsys->free( ).

*  ENDIF.

ENDMETHOD.
