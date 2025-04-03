 CALL FUNCTION 'INM_CREATE_COMMERCIAL_PROJECT'
      EXPORTING
        iv_item_guid       = ls_comm_proj_context-object_id
        iv_item_type       = ls_comm_proj_context-object_type
        iv_mp_type         = ls_comm_proj_context-target_type
      IMPORTING
        et_messages        = lt_cpm_messages
        ev_rc              = lv_return.
    APPEND LINES OF lt_cpm_messages TO et_messages.
    ![image](https://github.com/user-attachments/assets/9f9255e8-c768-4fec-9761-0b6255b3b827)
