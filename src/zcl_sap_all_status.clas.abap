CLASS zcl_sap_all_status DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: activated   TYPE char11 VALUE 'ACTIVATED',
               deactivated TYPE char11 VALUE 'DEACTIVATED',
               hidden      TYPE char11 VALUE 'HIDDEN'.

    TYPES: unames TYPE STANDARD TABLE OF uname WITH DEFAULT KEY.

    CLASS-METHODS: mass_update IMPORTING unames TYPE unames status TYPE char11.

    METHODS: constructor IMPORTING uname TYPE uname,
      role_buffer_is_filled RETURNING VALUE(is_filled) TYPE abap_bool,
      user_has_sap_all RETURNING VALUE(has_sap_all) TYPE abap_bool,
      get_status RETURNING VALUE(status) TYPE char11,
      update_status IMPORTING status TYPE char11.

    CLASS-METHODS: test IMPORTING p_task TYPE clike .
    CLASS-DATA: update_done TYPE abap_bool,
                processes   TYPE i,
                max type i,
                free type i,
                maxt type i,
                tasks       TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: uname            TYPE uname,
          buffer_is_filled TYPE abap_bool,
          has_sap_all      TYPE abap_bool,
          status           TYPE char11.


    METHODS: write_to_role_buffer.

ENDCLASS.



CLASS zcl_sap_all_status IMPLEMENTATION.

  METHOD constructor.
    me->uname = uname.
  ENDMETHOD.

  METHOD role_buffer_is_filled.
    SELECT COUNT( * ) FROM usrbf2 WHERE bname = @uname AND auth = '&_SAP_ALL' INTO @DATA(buffer_entry_amount).

    "SAP_ALL buffer roles were above 3000 but inconsistent in tests so can't use the exact number
    is_filled = buffer_is_filled = COND #( WHEN buffer_entry_amount > 3000 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD user_has_sap_all.
    SELECT SINGLE profile FROM ust04 WHERE bname = @uname AND profile = 'SAP_ALL' INTO @DATA(_profile).

    has_sap_all = me->has_sap_all = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD get_status.

    role_buffer_is_filled( ).
    user_has_sap_all( ).

    status = COND #( WHEN buffer_is_filled = abap_true AND has_sap_all = abap_true THEN activated
                             WHEN buffer_is_filled = abap_true AND has_sap_all = abap_false THEN hidden
                             WHEN buffer_is_filled = abap_false AND has_sap_all = abap_false THEN deactivated ).
  ENDMETHOD.

  METHOD update_status.

    DELETE FROM ust04 WHERE bname = uname AND profile = 'SAP_ALL'.

    DELETE FROM usrbf2 WHERE bname = uname AND auth = '&_SAP_ALL'.

    IF status = hidden.
      write_to_role_buffer( ).
    ELSEIF status = activated.

      write_to_role_buffer( ).

      DATA(profile) = VALUE ust04( bname = uname profile = 'SAP_ALL' ).

      INSERT ust04 FROM profile.

    ENDIF.


    me->status = status.
  ENDMETHOD.

  METHOD write_to_role_buffer.
    SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @DATA(roles).

    DATA role_buffer TYPE TABLE OF usrbf2.

    role_buffer = VALUE #( FOR role IN roles ( objct = role-objct auth = role-auth bname = uname ) ).

    INSERT usrbf2 FROM TABLE role_buffer.
  ENDMETHOD.

  METHOD mass_update.

*data: max type i, free type i.
call function 'SPBT_INITIALIZE'
*  EXPORTING
*    group_name                     = space            " PBT-Servergruppnenname
  IMPORTING
    max_pbt_wps                    =  max                " max. Anzahl PBT-Ressourcen im System
    free_pbt_wps                   =  free                " im Moment freie PBT-Ressourcen
  EXCEPTIONS
*    invalid_group_name             = 1                " der PBT-Servergruppenname ist ungültig
*    internal_error                 = 2                " interner Fehler ist aufgetreten ( s. Syslog )
    pbt_env_already_initialized    = 3                " PBT-Umgebung ist für Gruppe schon initialisiert
*    currently_no_resources_avail   = 4                " im Moment sind alle PBT-Ressourcen belegt
*    no_pbt_resources_found         = 5                " keine PBT-Ressourcen im System konfiguriert
*    cant_init_different_pbt_groups = 6                " unterschiedl. PBT-Gruppen nicht initialisierbar
*    others                         = 7
  .
IF sy-subrc = 0.
free = free * '0.75'.
*free = 8.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

maxt = lines( unames ).
*message |Updating users on { free } threads| type 'S'.
*do 20 times.
*call function 'Z_SAP_ALL_MASS_UPDATE'
*  EXPORTING
*    unames =
*    status =
*  .
*data(processes) = 0.
data fiunames type table of z_sap_all_unames.
data(abcunames) = unames.

do free times.
append value #( ) to fiunames.
enddo.
data(countery) = 1.
loop at abcunames into data(unamy).
if countery > free.
countery = 1.
endif.
data(tab) = fiunames[ countery ].
append unamy to tab.
fiunames[ countery ] = tab.
add 1 to countery.
endloop.
*append fiunames[  ]
*endwhile.
clear processes.
*clear processes.
tasks = 1.
    DATA(xxunames) = unames.
*    data counter type i.
*while xxunames is not initial.
    LOOP AT xxunames into data(unamex).
        cl_progress_indicator=>progress_indicate( i_text = |updating users in { free } threads - processed { sy-tabix } / { maxt } | i_processed = sy-tabix i_total = maxt i_output_immediately = abap_true ).

      WHILE processes >= free.
**
        WAIT UNTIL processes < free.
      ENDWHILE.
      DATA(tasknamex) = 'SAP_ALL' && tasks. "&& sy-tabix.
      DATA(xsu) = VALUE z_sap_all_unames( ( value #( uname = unamex ) ) ).
      CALL FUNCTION 'Z_SAP_ALL_MASS_UPDATE' STARTING NEW TASK tasknamex CALLING test ON END OF TASK
        EXPORTING
          uname = unamex
          status = status.
*          TABLES unames = xsu.
*      IF sy-subrc = 0.
        ADD 1 TO processes.
        add 1 to tasks.
*        delete xxunames where table_line eq unamex.
*        delete fiunames INDEX 1.
*        DELETE xxunames where TABLE_LINE eq unamex.
*      ENDIF.
    ENDLOOP.
*endwhile.
    WAIT FOR ASYNCHRONOUS TASKS UNTIL update_done = abap_true.

    RETURN.
    DATA: split_unames TYPE TABLE OF z_sap_all_unames.

    DATA(mod) = lines( unames ) / 5.
    DATA tmp TYPE z_sap_all_unames.
    LOOP AT unames INTO DATA(uname).
      APPEND uname TO tmp.
      IF sy-tabix MOD mod EQ 0.
        APPEND tmp TO split_unames.
        CLEAR tmp.
      ELSE.

      ENDIF.

    ENDLOOP.
    LOOP AT split_unames INTO DATA(s_u).

      DATA(taskname) = 'SAP_ALL' && sy-tabix.

      CALL FUNCTION 'Z_SAP_ALL_MASS_UPDATE' STARTING NEW TASK taskname CALLING test ON END OF TASK
        EXPORTING
          unames = s_u
          status = status.
    ENDLOOP.
    DATA(lines) = lines( split_unames ).
    DATA(wait_for) = 'SAP_ALL' && lines.
    WAIT FOR ASYNCHRONOUS TASKS UNTIL update_done = abap_true.
*call function 'SAPGUI_PRO'
*while update_done = abap_false.
*endwhile.
    RETURN.

    DATA(ranges) = VALUE uname_range_tab( FOR unam IN unames (  sign = 'I' option = 'EQ' low = unam ) ).

    DELETE FROM ust04 WHERE bname IN ranges AND profile = 'SAP_ALL'.

    DELETE FROM usrbf2 WHERE bname IN ranges AND auth = '&_SAP_ALL'.

    IF status = hidden.

      SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @DATA(roles).

      DATA role_buffer TYPE TABLE OF usrbf2.
      role_buffer = VALUE #( FOR role IN roles FOR un IN unames ( objct = role-objct auth = role-auth bname = un ) ).
      INSERT usrbf2 FROM TABLE role_buffer.

    ELSEIF status = activated.


      SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @roles.

*      DATA role_buffer TYPE TABLE OF usrbf2.
*loop at unames into data(un).
*loop at roles into data(role).
*append value #(  )
*endloop.
*endloop.
      role_buffer = VALUE #( FOR role IN roles FOR unx IN unames ( objct = role-objct auth = role-auth bname = unx ) ).
      INSERT usrbf2 FROM TABLE role_buffer.

      DATA profiles TYPE TABLE OF ust04.
      profiles = VALUE #( FOR u IN unames ( bname = u profile = 'SAP_ALL') ).
      INSERT ust04 FROM TABLE profiles.
    ENDIF.


  ENDMETHOD.

  METHOD test.

    DATA(current_task) = CONV i( substring( val = p_task off = strlen( p_task ) - 1 len = 1 ) ).

* cl_progress_indicator=>progress_indicate( i_text = |updating user | i_processed = current_task i_total = tasks i_output_immediately = abap_true ).
    SUBTRACT 1 FROM processes.
    RECEIVE RESULTS FROM FUNCTION 'Z_SAP_ALL_MASS_UPDATE'.
    IF conv i( substring( val = p_task off = strlen( p_task ) - 1 len = 1 ) ) EQ maxt.
      update_done = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
