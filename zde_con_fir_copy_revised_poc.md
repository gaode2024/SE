*&---------------------------------------------------------------------*
*& Report ZDE_CON_FIR_COPY_REVISED_POC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zde_con_fir_copy_revised_poc.

* Data Definitions
TABLES: ekpo, acesobj_item, acesobj_item_per.

* Internal Tables
TYPES: BEGIN OF ty_output,
         bukrs             TYPE bukrs,
         ref_key           TYPE char32,
         ref_subkey        TYPE char32,
         adjusted_per_amnt TYPE wrbtr,
         adjstmnt_reason   TYPE char20,
         adjstmnt_comment  TYPE char255,
       END OF ty_output.

DATA: lt_output TYPE TABLE OF ty_output,
      ls_output TYPE ty_output.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR acesobj_item_per-bukrs.
  PARAMETERS: p_zdate TYPE dats OBLIGATORY DEFAULT sy-datum.
  SELECT-OPTIONS: s_refkey FOR acesobj_item_per-ref_key.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM frm_get_month_last_day USING sy-datum CHANGING p_zdate.

AT SELECTION-SCREEN ON p_zdate.
  DATA: lv_valid TYPE abap_bool.

  PERFORM frm_validate_month_end USING p_zdate CHANGING lv_valid.

  IF lv_valid = abap_false.
    MESSAGE e398(00) WITH 'Only month-end dates are allowed'.
  ENDIF.
* Main Program
START-OF-SELECTION.
  PERFORM frm_get_revised_poc_data.
  PERFORM frm_copy_to_current_period.

FORM frm_get_month_last_day USING iv_date TYPE d
                        CHANGING cv_lastday TYPE d.
  DATA: lv_year TYPE numc4,
        lv_month TYPE numc2.

  lv_year = iv_date+0(4).
  lv_month = iv_date+4(2).

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = iv_date
    IMPORTING
      last_day_of_month = cv_lastday
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
ENDFORM.

FORM frm_validate_month_end USING iv_date TYPE d
                       CHANGING cv_valid TYPE abap_bool.
  DATA: lv_lastday TYPE d.

  PERFORM frm_get_month_last_day USING iv_date CHANGING lv_lastday.

  cv_valid = boolc( iv_date = lv_lastday ).
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_REVISED_POC_DATA
*&---------------------------------------------------------------------*
FORM frm_get_revised_poc_data.

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
      AND substring( a~ref_subkey , 4, 2 ) = substring( c~ebelp , 4, 2 )
    WHERE a~bukrs IN @s_bukrs
      AND a~ref_key IN @s_refkey
      AND c~elikz = ''
      AND b~life_end_date < @p_zdate
      AND a~itemtype = 'SCSTPLN'
      AND a~adjusted_per_amnt_wsl <> 0
    INTO TABLE @DATA(lt_data).
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
FORM frm_copy_to_current_period.
  DATA: lv_success TYPE i,
        lv_failed  TYPE i.
  DATA: ls_item_per TYPE acesobj_item_per.
  LOOP AT lt_output INTO ls_output.
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
*  MESSAGE s398(00) WITH 'Process completed:' lv_success 'records updated,' lv_failed 'failed'.
  MESSAGE s398(00) WITH lv_success 'records updated,' lv_failed 'failed'.
ENDFORM.
