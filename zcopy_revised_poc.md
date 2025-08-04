REPORT zcopy_revised_poc.

* Data Definitions
TABLES: ekpo, acesobj_item, acesobj_item_per.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR acesobj_item_per-bukrs NO INTERVALS.
  PARAMETERS: p_zdate TYPE d OBLIGATORY DEFAULT sy-datum.
  SELECT-OPTIONS: s_refkey FOR acesobj_item_per-ref_key NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

* Internal Tables
TYPES: BEGIN OF ty_output,
         bukrs               TYPE bukrs,
         ref_key            TYPE char32,
         ref_subkey         TYPE char32,
         adjusted_per_amnt  TYPE wrbtr,
         adjstmnt_reason    TYPE char20,
         adjstmnt_comment   TYPE char255,
       END OF ty_output.

DATA: lt_output TYPE TABLE OF ty_output,
      ls_output TYPE ty_output.

* Main Program
START-OF-SELECTION.
  PERFORM get_revised_poc_data.
  PERFORM copy_to_current_period.

*&---------------------------------------------------------------------*
*&      Form  GET_REVISED_POC_DATA
*&---------------------------------------------------------------------*
FORM get_revised_poc_data.
  SELECT a~bukrs, a~ref_key, a~ref_subkey, 
         a~adjusted_per_amnt_wsl, a~adjstmnt_reason, a~adjstmnt_comment,
         a~period_end_date,
         b~life_end_date,
         c~elikz
    FROM acesobj_item_per AS a
    INNER JOIN acesobj_item AS b
      ON  a~bukrs = b~bukrs
      AND a~ref_key = b~ref_key
      AND a~ref_subkey = b~ref_subkey
      AND a~itemtype = b~itemtype
    INNER JOIN ekpo AS c
      ON  a~ref_key = c~ebeln
      AND substring( val = a~ref_subkey off = 3 len = 2 ) = c~ebelp
    INTO TABLE @DATA(lt_data)
    WHERE a~bukrs IN @s_bukrs
      AND a~ref_key IN @s_refkey
      AND c~elikz IS NULL
      AND b~life_end_date < @p_zdate
      AND a~itemtype = 'SCSTPLN'
      AND a~adjusted_per_amnt_wsl <> 0.

  IF lt_data IS INITIAL.
    MESSAGE 'No relevant data found' TYPE 'I'.
    STOP.
  ENDIF.

  " Get most recent record per key combination
  SORT lt_data BY bukrs ref_key ref_subkey period_end_date DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING bukrs ref_key ref_subkey.

  " Prepare output
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    ls_output-bukrs = <fs_data>-bukrs.
    ls_output-ref_key = <fs_data>-ref_key.
    ls_output-ref_subkey = <fs_data>-ref_subkey.
    ls_output-adjusted_per_amnt = <fs_data>-adjusted_per_amnt_wsl.
    ls_output-adjstmnt_reason = <fs_data>-adjstmnt_reason.
    ls_output-adjstmnt_comment = <fs_data>-adjstmnt_comment.
    APPEND ls_output TO lt_output.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COPY_TO_CURRENT_PERIOD
*&---------------------------------------------------------------------*
FORM copy_to_current_period.
  DATA: lv_success TYPE i,
        lv_failed  TYPE i.

  LOOP AT lt_output INTO ls_output.
    " Call transaction FACRARVWCO via BAPI or direct update
    " Here we simulate with direct update for simplicity
    " In production, should use proper BAPI or function module
    
    DATA: ls_item_per TYPE acesobj_item_per.
    
    " Get current period record
    SELECT SINGLE * FROM acesobj_item_per INTO ls_item_per
      WHERE bukrs = ls_output-bukrs
        AND ref_key = ls_output-ref_key
        AND ref_subkey = ls_output-ref_subkey
        AND period_end_date = p_zdate
        AND itemtype = 'SCSTPLN'.
    
    IF sy-subrc = 0.
      " Update existing record
      ls_item_per-adjusted_per_amnt_wsl = ls_output-adjusted_per_amnt.
      ls_item_per-adjstmnt_reason = ls_output-adjstmnt_reason.
      ls_item_per-adjstmnt_comment = ls_output-adjstmnt_comment.
      
      UPDATE acesobj_item_per FROM ls_item_per.
    ELSE.
      " Create new record
      CLEAR ls_item_per.
      ls_item_per-bukrs = ls_output-bukrs.
      ls_item_per-ref_key = ls_output-ref_key.
      ls_item_per-ref_subkey = ls_output-ref_subkey.
      ls_item_per-period_end_date = p_zdate.
      ls_item_per-itemtype = 'SCSTPLN'.
      ls_item_per-adjusted_per_amnt_wsl = ls_output-adjusted_per_amnt.
      ls_item_per-adjstmnt_reason = ls_output-adjstmnt_reason.
      ls_item_per-adjstmnt_comment = ls_output-adjstmnt_comment.
      
      INSERT acesobj_item_per FROM ls_item_per.
    ENDIF.
    
    IF sy-subrc = 0.
      ADD 1 TO lv_success.
      COMMIT WORK.
    ELSE.
      ADD 1 TO lv_failed.
      ROLLBACK WORK.
    ENDIF.
  ENDLOOP.
  
  " Output result
  MESSAGE s398(00) WITH 'Process completed:' lv_success 'records updated,' lv_failed 'failed'.
ENDFORM.
