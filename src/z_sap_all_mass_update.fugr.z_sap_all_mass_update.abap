FUNCTION Z_SAP_ALL_MASS_UPDATE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(UNAME) TYPE  UNAME
*"     VALUE(STATUS) TYPE  CHAR11
*"  TABLES
*"      UNAMES TYPE  Z_SAP_ALL_UNAMES
*"----------------------------------------------------------------------

*data(uname) = unames[ 1 ]-uname.

new zcl_sap_all_status( uname )->update_status( status ).
return.

*data(unames) = value z_sap_all_unames( ( unamex ) ).

    DATA(ranges) = VALUE uname_range_tab( FOR unam IN unames (  sign = 'I' option = 'EQ' low = unam-uname ) ).

    DELETE FROM ust04 WHERE bname IN ranges AND profile = 'SAP_ALL'.

    DELETE FROM usrbf2 WHERE bname IN ranges AND auth = '&_SAP_ALL'.

    IF status = 'HIDDEN'.

      SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @DATA(roles).

      DATA role_buffer TYPE TABLE OF usrbf2.
      role_buffer = VALUE #( FOR role IN roles FOR un IN unames ( objct = role-objct auth = role-auth bname = un ) ).
      INSERT usrbf2 FROM TABLE role_buffer.

    ELSEIF status = 'ACTIVATED'.


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



ENDFUNCTION.
