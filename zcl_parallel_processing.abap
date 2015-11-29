REPORT ztest_apl_001.

INTERFACE zif_parallel_process.
  METHODS:
  
    excecute 
      IMPORTING 
        i_master TYPE REF TO zcl_parallel_process
        i_group TYPE rzllitab-classname
        i_task TYPE char20
        i_data TYPE STANDARD TABLE
      RETURNING 
        VALUE(r_rc) TYPE sysubrc,
        
    call_back RETURNING VALUE(r_rc) TYPE sysubrc.
ENDINTERFACE.

CLASS zcl_parallel_process DEFINITION
*  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_task TYPE c LENGTH 20.
  
    METHODS:

      constructor 
        IMPORTING 
          i_classname TYPE rzllitab-classname DEFAULT 'parallel_generators'
          i_lines_by_task TYPE i DEFAULT 150
          i_task_name TYPE string DEFAULT 'PARALLEL_TASK'
          i_slave TYPE REF TO zif_parallel_process OPTIONAL
          i_nr_of_tries TYPE i DEFAULT 5
          i_function_name TYPE string OPTIONAL,
          
      process 
        IMPORTING i_data TYPE STANDARD TABLE
        RETURNING VALUE(ro) TYPE REF TO zcl_parallel_process,
      
      wait.     
      
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF st_interval,
        task       TYPE ty_task,
        index_from TYPE i,
        index_to   TYPE i,
        active     TYPE flag,
        failed     TYPE flag,
      END OF st_interval,
      tt_intervals TYPE STANDARD TABLE OF st_interval WITH EMPTY KEY.

    TYPES:
      BEGIN OF st_task,
        name TYPE ty_task,
        free TYPE flag,
      END OF st_task,
      tt_tasks TYPE STANDARD TABLE OF st_task WITH EMPTY KEY.
      
    DATA group TYPE rzllitab-classname.
    DATA lines_by_task TYPE i.
    DATA slave TYPE REF TO zif_parallel_process.
    DATA function_name TYPE string.

    DATA tasks TYPE tt_tasks.
    DATA intervals TYPE tt_intervals.

    DATA sent TYPE i.
    DATA received TYPE i.
    
    METHODS:
      
      set_group_name IMPORTING i_classname TYPE rzllitab-classname,
      
      init_tasks IMPORTING i_task_name TYPE string,
      
      set_intervals
        IMPORTING
          VALUE(i_lines_pack)  TYPE i
          VALUE(i_total_lines) TYPE i,

      finish_interval
        IMPORTING
          i_task TYPE ty_task
          i_rc TYPE sysubrc,

      get_free_task RETURNING VALUE(r_task) TYPE ty_task,
      
      release_task IMPORTING VALUE(i_task) TYPE ty_task,
      
      call_back IMPORTING p_task TYPE ty_task.
      
ENDCLASS.

CLASS zcl_parallel_process IMPLEMENTATION.
  METHOD zif_parallel_process~execute.
    CALL FUNCTION me->function_name
      STARTING NEW TASK i_task
      DESTINATION IN GROUP i_group
      CALLING i_master->call_back ON END OF TASK
        EXPORTING
          i_input = i_data
      EXCEPTIONS
        communication_failure = 1 "MESSAGE lv_msg
        system_failure        = 2 "MESSAGE lv_msg
        resource_failure      = 3.

    r_rc = sy-subrc.
        
  ENDMETHOD.
  
  METHOD zif_parallel_process~call_back.
    RECEIVE RESULTS FROM FUNCTION me->function_name
        EXCEPTIONS
          communication_failure = 1 "MESSAGE lv_msg
          system_failure        = 2. "MESSAGE lv_msg .
  
    r_rc = sy-subrc.
       
  ENDMETHOD.
  
  METHOD wait.
    WAIT UNTIL me->sent = me->received. 
  ENDMETHOD.
  
  METHOD call_back.
    ADD 1 TO me->received.
    me->release_task( p_task ).
    
    DATA(rc) = slave->zif_parallel_process~call_back( ).
    
    me->finish_interval(  i_task = p_task
                          i_return_code = rc
                          ).
    
  ENDMETHOD.
  
  METHOD finish_interval.
    ASSIGN me->intervals[ task = i_task
                          active = abap_true
                        ] TO FIELD-SYMBOL(<interval>).
                        
    CHECK <interval> IS ASSIGNED.
    <interval>-failed = xsdbool( i_return_code <> 0 ).
                                
    CLEAR <interval>-active.
    
  ENDMETHOD.
  
  METHOD get_free_task.
    ASSIGN me->tasks[ free = abap_true ] TO FIELD-SYMBOL(<task>).
    IF <task> IS ASSIGNED.
      r_task = <task>-name.
      CLEAR <task>-free.
      
    ELSE.
      WAIT UNTIL ( me->sent - me->received ) < lines( me->tasks ).
      r_task = me->get_free_task( ).
        
    ENDIF.
  ENDMETHOD.

  METHOD set_group_name.
    SELECT SINGLE classname
      INTO me->group
      FROM rzllitab
      WHERE grouptype = 'S'
        AND classname = i_classname.
        
    CHECK sy-subrc <> 0.
    SELECT SINGLE classname
      INTO me->group
      FROM rzllitab
      WHERE grouptype = 'S'.
        
    CHECK sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_ref_creation.

  ENDMETHOD.
  
  METHOD init_tasks.
    DATA free_tasks TYPE i.
    
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = me->group
      IMPORTING
        free_pbt_wps                   = free_tasks
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    IF sy-subrc = 3.
      CLEAR sy-subrc.
      CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
        IMPORTING
          free_pbt_wps                = free_tasks
        EXCEPTIONS
          internal_error              = 1
          pbt_env_not_initialized_yet = 2
          OTHERS                      = 3.
        
    ENDIF.
    
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_ref_creation.
    ENDIF.

    SUBTRACT 2 FROM free_tasks.

    IF free_tasks >= 1.
      initialized = abap_true.
      me->tasks = VALUE #( FOR i = 1 THEN i + 1 WHILE i <= free_tasks ( name = |{ i_task_name }_{ i }| ) ).
      
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_ref_creation.
      
    ENDIF.
  ENDMETHOD.
  
  METHOD constructor.
    DO i_nr_of_tries TIMES.
      DATA(index) = sy-index.
      TRY.
          me->set_group_name( me->classname ).
          me->init_tasks( i_task_name ).
          EXIT.
         
        CATCH cx_sy_ref_creation.
          IF index = i_nr_of_tries.
            RAISE EXCEPTION TYPE cx_sy_ref_is_initial.
          ENDIF.
          
          WAIT UP TO 1 SECONDS.
          
      ENDTRY.
    ENDDO.
    
    me->lines_by_task = i_lines_by_task.
    me->slave = COND #( WHEN i_slave IS NOT INITIAL THEN i_slave ELSE me ).
    me->function_name = i_function_name.
          
  ENDMETHOD.
  
  METHOD set_intervals.
    IF  me->lines_by_task > 0
    AND me->lines_by_task < i_total_lines.
      DATA(lv_packs_number) = i_total_lines DIV me->lines_by_task.

      IF i_total_lines MOD me->lines_by_task <> 0. ADD 1 TO lv_packs_number. ENDIF.

      me->intervals =
        VALUE #(
          FOR i = 0 THEN i + 1 WHILE i < lv_packs_number
          ( index_from = ( me->lines_by_task * i ) + 1
            index_to = nmin( val1 = ( me->lines_by_task * ( i + 1 ) ) val2 = i_total_lines ) )
          ).

    ELSE.
      me->intervals = VALUE #( (  index_from = 1
                                  index_to = i_total_lines )
                                  ).
    ENDIF.
  ENDMETHOD.

  METHOD release_task.
    TRY.
        me->tasks[ name = i_task ]-free = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD process.
    DATA data TYPE REF TO data.
    CREATE DATA data LIKE i_data.
    
    FIELD-SYMBOL <data> TYPE STANDARD TABLE
    ASSIGN data->* TO <data>.
    
    me->set_intervals( lines( i_data ) ).
        
    LOOP AT me->intervals ASSIGNING FIELD-SYMBOL(<interval>).
    
      <interval>-task = me->get_free_task( ).
      <interval>-active = abap_true.
    
      <data> = VALUE #( FOR wa IN i_data FROM <interval>-index_from TO <interval>-index_to ( wa ) ).
*      <data> = i_data.
*      IF <interval>-index_to <> lines( i_data ).
*        DELETE <data> FROM <interval>-index_to + 1 TO lines( i_data ).
*      ENDIF.
*      IF <interval>-index_from <> 1.
*        DELETE <data> FROM 1 TO <interval>-index_from - 1.
*      ENDIF.
*      
      ADD 1 TO me->sent.
      
      DATA(rc) = i_slave->zif_parallel_process~execute( i_master = me
                                                        i_group = me->group
                                                        i_task = <interval>-task
                                                        i_data = <data>
                                                        ).
      
      IF rc <> 0.
        me->finish_interval(  i_task = <interval>-task
                              i_rc = rc
                              ).
                              
        me->release_task( <interval>-task ).
        
        ADD 1 TO me->received.
      ENDIF.
    ENDLOOP.
    
    ro = me.
  ENDMETHOD.
  
ENDCLASS.

" USE CASE
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_parallel_process.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD constructor.
    SELECT *
      FROM bseg
      INTO TABLE @DATA(lt).
      
    NEW zcl_parallel_process( i_slave = me 
          )->process( lt 
          )->wait( ).
    
  ENDMETHOD.
  METHOD zif_parallel_process~execute.
    CALL FUNCTION 'Z'
      STARTING NEW TASK i_task
      DESTINATION IN GROUP i_group
      CALLING i_master->call_back ON END OF TASK
        EXPORTING
          i_input = i_data
      EXCEPTIONS
        communication_failure = 1 "MESSAGE lv_msg
        system_failure        = 2 "MESSAGE lv_msg
        resource_failure      = 3.

    r_rc = sy-subrc.
        
  ENDMETHOD.
  
  METHOD zif_parallel_process~call_back.
    RECEIVE RESULTS FROM FUNCTION 'Z'
        EXCEPTIONS
          communication_failure = 1 "MESSAGE lv_msg
          system_failure        = 2. "MESSAGE lv_msg .
  
    r_rc = sy-subrc.
       
  ENDMETHOD.
ENDCLASS.                    "lcl_mains IMPLEMENTATION

START-OF-SELECTION.
  NEW lcl_main( ).