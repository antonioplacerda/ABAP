CLASS zcl_appl_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF  msg_type,
        error   TYPE symsgty VALUE 'E',
        warning TYPE symsgty VALUE 'W',
        success TYPE symsgty VALUE 'S',
        inform  TYPE symsgty VALUE 'I',
      END OF    msg_type.

    TYPES:
      tt_balnrext TYPE RANGE OF balnrext,
      tt_aldate   TYPE RANGE OF aldate,
      tt_altime   TYPE RANGE OF altime.

    TYPES:
      BEGIN OF  st_list ,
        extnumber  TYPE balhdr-extnumber   ,
        aldate     TYPE balhdr-aldate      ,
        altime     TYPE balhdr-altime      ,
        log_handle TYPE balhdr-log_handle  ,
        msg_cnt_al TYPE balhdr-msg_cnt_al  ,
        msg_cnt_a  TYPE balhdr-msg_cnt_a   ,
        msg_cnt_e  TYPE balhdr-msg_cnt_e   ,
        msg_cnt_w  TYPE balhdr-msg_cnt_w   ,
        msg_cnt_i  TYPE balhdr-msg_cnt_i   ,
        msg_cnt_s  TYPE balhdr-msg_cnt_s   ,
      END OF  st_list,
      tt_list TYPE STANDARD TABLE OF st_list WITH EMPTY KEY.

    CLASS-METHODS:
      load_display_log IMPORTING iv_log_handle TYPE balloghndl,
      get_alv
        IMPORTING
          iv_object    TYPE balobj_d
          it_extnumber TYPE tt_balnrext OPTIONAL
          it_date      TYPE tt_aldate OPTIONAL
          it_time      TYPE tt_altime OPTIONAL,
      get_list
        IMPORTING
          iv_object    TYPE balobj_d
          it_extnumber TYPE tt_balnrext OPTIONAL
          it_date      TYPE tt_aldate OPTIONAL
          it_time      TYPE tt_altime OPTIONAL
        EXPORTING
          et_list      TYPE tt_list.

    METHODS:
      constructor
        IMPORTING
          iv_extnumber  TYPE bal_s_log-extnumber   OPTIONAL
          iv_object     TYPE bal_s_log-object
          iv_subobject  TYPE bal_s_log-subobject   OPTIONAL
          iv_aldate     TYPE bal_s_log-aldate      DEFAULT sy-datum
          iv_altime     TYPE bal_s_log-altime      DEFAULT sy-uzeit
          iv_altcode    TYPE bal_s_log-altcode     DEFAULT sy-tcode
          iv_aluser     TYPE bal_s_log-aluser      DEFAULT sy-uname
          iv_alprog     TYPE bal_s_log-alprog      DEFAULT sy-cprog
          iv_aldate_del TYPE bal_s_log-aldate_del  OPTIONAL
          iv_del_before TYPE bal_s_log-del_before  OPTIONAL ,
      append_string
        IMPORTING
          iv_type TYPE symsgty DEFAULT msg_type-error
          iv_msg  TYPE string ,
      append_line
        IMPORTING is_return TYPE bapiret2 ,
      append_table
        IMPORTING it_return TYPE bapiret2_t ,
      append_message
        IMPORTING
          iv_id   TYPE symsgid
          iv_type TYPE symsgty
          iv_num  TYPE symsgno
          iv_v1   TYPE symsgv OPTIONAL
          iv_v2   TYPE symsgv OPTIONAL
          iv_v3   TYPE symsgv OPTIONAL
          iv_v4   TYPE symsgv OPTIONAL ,
      get_log
        IMPORTING
          it_return  TYPE bapiret2_t OPTIONAL
          ib_save    TYPE abap_bool DEFAULT abap_true
          ib_display TYPE abap_bool DEFAULT abap_true .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      ms_header     TYPE bal_s_log ,
      mb_display    TYPE abap_bool VALUE abap_true ,
      ms_log_handle TYPE balloghndl ,
      mb_save       TYPE abap_bool VALUE abap_true .

    METHODS:
      add_msg_to_log
        IMPORTING
          is_msg TYPE bal_s_msg ,
      convert_bapiret2_to_bal_msg
        IMPORTING
          is_return     TYPE bapiret2
        RETURNING
          VALUE(rs_msg) TYPE bal_s_msg ,
      create_log ,
      display_log ,
      get_probclass
        IMPORTING
          iv_type            TYPE bapiret2-type
        RETURNING
          VALUE(r_probclass) TYPE bal_s_msg-probclass ,
      save_log .

ENDCLASS.



CLASS zcl_appl_log IMPLEMENTATION.


  METHOD add_msg_to_log.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = is_msg
        i_log_handle  = ms_log_handle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD append_line.
    CHECK is_return IS NOT INITIAL.

    add_msg_to_log( convert_bapiret2_to_bal_msg( is_return ) ).

  ENDMETHOD.


  METHOD append_message.

    CHECK iv_id IS NOT INITIAL AND iv_type IS NOT INITIAL AND iv_num IS NOT INITIAL.

    DATA(ls_return) = VALUE bapiret2(
      id          = iv_id
      type        = iv_type
      number      = iv_num
      message_v1  = iv_v1
      message_v2  = iv_v2
      message_v3  = iv_v3
      message_v4  = iv_v4
      ).

    add_msg_to_log( convert_bapiret2_to_bal_msg( ls_return ) ).

  ENDMETHOD.


  METHOD append_string.

    CHECK iv_msg IS NOT INITIAL.

    DATA lv_text TYPE text1024 .
    lv_text = iv_msg.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = ms_log_handle
        i_msgty          = iv_type
        i_probclass      = get_probclass( iv_type )
        i_text           = lv_text
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD append_table.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<return>).

      add_msg_to_log( convert_bapiret2_to_bal_msg( <return> ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA(lv_extnumber) = iv_extnumber.

    IF lv_extnumber IS INITIAL.
      lv_extnumber = sy-datum && sy-uzeit.
    ENDIF.

    ms_header = VALUE #(
        extnumber  = lv_extnumber  " External ID
        object     = iv_object     " Object Name
        subobject  = iv_subobject  " Sub object
        aldate     = iv_aldate     " Creation Date
        altime     = iv_altime     " Creation Time
        altcode    = iv_altcode    " Transaction Code
        aluser     = iv_aluser     " User
        alprog     = iv_alprog     " Program
        aldate_del = iv_aldate_del " Expiration Date
        del_before = iv_del_before " Keep log until
        ).

    create_log( ).

  ENDMETHOD.


  METHOD convert_bapiret2_to_bal_msg.

    rs_msg-probclass = get_probclass( is_return-type ).

    rs_msg-msgty = is_return-type.

    rs_msg-msgid = is_return-id.
    rs_msg-msgno = is_return-number.
    rs_msg-msgv1 = is_return-message_v1.
    rs_msg-msgv2 = is_return-message_v2.
    rs_msg-msgv3 = is_return-message_v3.
    rs_msg-msgv4 = is_return-message_v4.

  ENDMETHOD.


  METHOD create_log.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ms_header
      IMPORTING
        e_log_handle = ms_log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD display_log.

    CHECK mb_display = abap_true.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND ms_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle = lt_log_handle
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD get_log.

    mb_save = ib_save.
    mb_display = ib_display.

    append_table( it_return ).

    save_log( ).
    display_log( ).

  ENDMETHOD.


  METHOD get_probclass.

    CASE iv_type.
      WHEN 'A' OR 'X'.
        r_probclass = '1'.
      WHEN msg_type-error.
        r_probclass = '2'.
      WHEN msg_type-warning.
        r_probclass = '3'.
      WHEN msg_type-success.
        r_probclass = '4'.
    ENDCASE.

  ENDMETHOD.


  METHOD load_display_log.
    DATA lt_msg_handle TYPE bal_t_msgh.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND iv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle     = lt_log_handle
      IMPORTING
        e_t_msg_handle     = lt_msg_handle
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_log_handle
        i_t_msg_handle       = lt_msg_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4.
  ENDMETHOD.


  METHOD save_log.

    CHECK mb_save = abap_true.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND ms_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD get_alv.
    NEW lcl_helper_alv( iv_object    = iv_object
                        it_extnumber = it_extnumber
                        it_date      = it_date
                        it_time      = it_time
                        ).
  ENDMETHOD.


  METHOD get_list.
    SELECT extnumber,
           aldate,
           altime,
           log_handle,
           msg_cnt_al,
           msg_cnt_a,
           msg_cnt_e,
           msg_cnt_w,
           msg_cnt_i,
           msg_cnt_s
      FROM balhdr
      INTO TABLE @et_list
      WHERE object = @iv_object
        AND extnumber IN @it_extnumber
        AND aldate    IN @it_date
        AND altime    IN @it_time.
  ENDMETHOD.

ENDCLASS.