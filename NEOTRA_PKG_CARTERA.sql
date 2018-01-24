CREATE OR REPLACE PACKAGE          PKG_CARTERA IS

    PROCEDURE SP_NEO_MX_SUM_VAL_BY_ZON (
            P_TXT_OFI_ACT IN VARCHAR2,
            P_OPT IN NUMBER,
            VAR_SUM_VAL_CART OUT SYS_REFCURSOR,
            P_SAL_NBR OUT NUMBER,
            P_SAL_TXT OUT VARCHAR2);

    PROCEDURE SP_NEO_MX_SUM_CTR_BY_ZON (
            P_TXT_ESTRC IN NUMBER,
            P_OPT IN NUMBER,
            VAR_SUM_CTR OUT SYS_REFCURSOR);

    PROCEDURE SP_NEO_MX_SUM_VAL_CTR_DET(
            P_TXT_ESTRC IN NUMBER,
            P_OPT IN NUMBER,
            P_SAL_NBR OUT NUMBER,
            P_SAL_TXT OUT VARCHAR2, 
            VAR_SUM_VAL_CART OUT SYS_REFCURSOR);

    PROCEDURE SP_GET_GRAP_CART(
            P_OPC IN NUMBER,
            P_TXT_ESTR IN VARCHAR2,
            P_FCH_INI VARCHAR2,
            P_FCH_FIN VARCHAR2,
            P_SAL_NBR OUT NUMBER,
            P_SAL_TXT OUT VARCHAR2, 
            P_JSON OUT VARCHAR2);

END PKG_CARTERA;


/


CREATE OR REPLACE PACKAGE BODY          "PKG_CARTERA" is

    PROCEDURE SP_NEO_MX_SUM_VAL_BY_ZON (
        P_TXT_OFI_ACT IN VARCHAR2,
        P_OPT IN NUMBER,
        VAR_SUM_VAL_CART OUT SYS_REFCURSOR,
        P_SAL_NBR OUT NUMBER,
        P_SAL_TXT OUT VARCHAR2) AS
    
    /* SP que regresa el sumarizado de la cartera por ZONA
       Recibe como parametro de entrada el TXT_OFI_ACT del ejecutivo Zonal
       y regresa el sumarizado de la cartera por Zona
     */

    v_Query VARCHAR2(5000);
    BEGIN

    v_Query:='
        SELECT TO_CHAR(SUM(NVL(a.num_tot_cart,0))) TOT_CART
               ,CASE WHEN SUM(NVL(a.NUM_TOT_CART,0))=0 THEN TO_CHAR(0)  
                    WHEN SUM(NVL(a.NUM_TOT_ANT,0))=0 THEN TO_CHAR(100) 
                    ELSE TO_CHAR(ROUND((SUM(NVL(a.NUM_TOT_CART,0))/SUM(NVL(a.NUM_TOT_ANT,0))*100)-100,2)) END AS TOT_CART_PCT
               ,CASE WHEN SUM(NVL(a.NUM_TOT_CART,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(a.NUM_TOT_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(a.NUM_TOT_CART,0))>SUM(NVL(a.NUM_TOT_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(a.NUM_TOT_CART,0)) BETWEEN SUM(NVL(a.NUM_TOT_ANT,0)) AND SUM(NVL(a.NUM_TOT_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS TOT_CART_COL
               ,TO_CHAR(SUM(NVL(a.NUM_CAPTA,0))) CAPTACION
               ,CASE WHEN SUM(NVL(a.NUM_CAPTA,0))=0 THEN TO_CHAR(0)
                    WHEN SUM(NVL(a.NUM_CAPTA_ANT,0))=0 THEN TO_CHAR(100) 
                    ELSE TO_CHAR(ROUND((SUM(NVL(a.NUM_CAPTA,0))/SUM(NVL(a.NUM_CAPTA_ANT,0))*100)-100,2)) END AS CAPTACION_PCT
               ,CASE WHEN SUM(NVL(a.NUM_CAPTA,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(a.NUM_CAPTA_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(a.NUM_CAPTA,0))>SUM(NVL(a.NUM_CAPTA_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(a.NUM_CAPTA,0)) BETWEEN SUM(NVL(a.NUM_CAPTA_ANT,0)) AND SUM(NVL(a.NUM_CAPTA_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS CAPTACION_COL
               ,TO_CHAR(SUM(NVL(a.NUM_COLC,0))) COLOCACION
               ,CASE WHEN SUM(NVL(a.NUM_COLC,0))=0 THEN TO_CHAR(0) 
                    WHEN SUM(NVL(a.NUM_COLC_ANT,0))=0 THEN TO_CHAR(100) 
                    ELSE TO_CHAR(ROUND((SUM(NVL(a.NUM_COLC,0))/SUM(NVL(a.NUM_COLC_ANT,0))*100)-100,2)) END AS COLOCACION_PCT
               ,CASE WHEN SUM(NVL(a.NUM_COLC,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(a.NUM_COLC_ANT,0))=0 THEN ''064627'' 
                    WHEN SUM(NVL(a.NUM_COLC,0))>SUM(NVL(a.NUM_COLC_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(a.NUM_COLC,0)) BETWEEN SUM(NVL(a.NUM_COLC_ANT,0)) AND SUM(NVL(a.NUM_COLC_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS COLOCACION_COL
               ,TO_CHAR(ROUND(SUM(NVL(a.NUM_MBB_CART,0)),2)) MBB
               ,CASE WHEN SUM(NVL(a.NUM_MBB_CART,0))=0 THEN TO_CHAR(0)
                    WHEN SUM(NVL(a.NUM_CTES_MBB_ANT,0))=0 THEN TO_CHAR(100) 
                    ELSE TO_CHAR(ROUND((SUM(NVL(a.NUM_MBB_CART,0))/SUM(NVL(a.NUM_MBB_ANT,0))*100)-100,2)) END AS MBB_PCT
               ,CASE WHEN SUM(NVL(a.NUM_MBB_CART,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(a.NUM_MBB_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(a.NUM_MBB_CART,0))>SUM(NVL(a.NUM_MBB_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(a.NUM_MBB_CART,0)) BETWEEN SUM(NVL(a.NUM_MBB_ANT,0)) AND SUM(NVL(a.NUM_MBB_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS MBB_COL
               ,TO_CHAR(SUM(NVL(A.NUM_VAR_DIA,0))) VARIACION ';

    CASE P_OPT
        WHEN 1 THEN
        v_Query:=v_Query ||' ,NVL(b.id_zon,0) AS ID_ESTRUCTURA 
          FROM NEO_MX_MAE_VAL_CART A
          JOIN NEO_MX_MAE_EJEC B ON A.TXT_OFI_ACT = B.TXT_OFI_ACT 
           AND B.ID_ZON =TO_NUMBER('||P_TXT_OFI_ACT||')
         GROUP BY NVL(b.id_zon,0)';
        WHEN 2 THEN    
        v_Query:=v_Query ||' ,NVL(b.id_reg,0) AS ID_ESTRUCTURA 
          FROM NEO_MX_MAE_VAL_CART A
          JOIN NEO_MX_MAE_EJEC B ON A.TXT_OFI_ACT = B.TXT_OFI_ACT 
           AND B.ID_REG =TO_NUMBER('||P_TXT_OFI_ACT||')
         GROUP BY NVL(b.id_reg,0)';
        ELSE
            DBMS_OUTPUT.PUT_LINE('ERROR, PLEASE SELECT THE OPTIONS 1 OR 2 FOR ZONE OR REGION');
            P_SAL_NBR:=0;
            P_SAL_TXT:='SE INGRESO UNA OPCIÓN NO VÁLIDA (NIVEL DE ESTRUCTURA)';
    END CASE;

    DBMS_OUTPUT.PUT_LINE(v_Query);
    OPEN VAR_SUM_VAL_CART FOR v_Query;
    P_SAL_NBR:=1;
    P_SAL_TXT:='CONSULTA EXITOSA';

    END;

    PROCEDURE SP_NEO_MX_SUM_CTR_BY_ZON (
        P_TXT_ESTRC IN NUMBER,
        P_OPT IN NUMBER,
        VAR_SUM_CTR OUT SYS_REFCURSOR) AS
    /* SP que regresa el sumarizado de la cartera por ZONA
     Recibe como parametro de entrada el TXT_OFI_ACT del ejecutivo Zonal
     y regresa el sumarizado de la cartera por Zona*/

    BEGIN
        IF P_OPT = 1 THEN
            open var_sum_ctr for 
            SELECT NVL(IE.ID_ZON,0) ID_ZON,NVL(IE.TOT_CTES,0) TOT_CTES_COD,
                   NVL(PROM.PROM_CTES_ZON,0)PROM_CTES,NVL(IE.TOT_CTES_ZON,0) TOT_CTES,
                   NVL(IE.COD_STA_CTE,0)COD_STA_CTE, NVL(COD.DSC_STA,0) DESC_STA_CTE
              from (select ID_ZON,SUM(NUM_IND_EJE) TOT_CTES,COD_STA_CTE,
                   (select SUM(NUM_IND_EJE) from NEO_MX_MAE_IND_EJE where ID_ZON = P_TXT_ESTRC) as TOT_CTES_ZON
                      from NEO_MX_MAE_IND_EJE
                     where ID_ZON = P_TXT_ESTRC
                     GROUP BY ID_ZON,COD_STA_CTE) IE,
                    (select TRUNC(SUM(NUM_IND_EJE)/COUNT(distinct(ID_ZON)),2) PROM_CTES_ZON,COD_STA_CTE
                       from NEO_MX_MAE_IND_EJE group by COD_STA_CTE) PROM,
                    (select COD_STA_CTE,dsc_sta from neo_mx_mae_cat_sta_cte ) COD
                             where IE.COD_STA_CTE = PROM.COD_STA_CTE
                             and COD.COD_STA_CTE = PROM.COD_STA_CTE;
        ELSE
            IF P_OPT = 2 THEN              
                open var_sum_ctr for 
                SELECT NVL(IE.ID_REG,0) ID_REG,NVL(IE.TOT_CTES,0) TOT_CTES_COD,
                NVL(PROM.PROM_CTES_REG,0)PROM_CTES,NVL(IE.TOT_CTES_REG,0) TOT_CTES,
                NVL(IE.COD_STA_CTE,0)COD_STA_CTE, NVL(COD.DSC_STA,0) DESC_STA_CTE 
                FROM 
                    (SELECT ID_REG,SUM(NUM_IND_EJE) TOT_CTES,COD_STA_CTE,
                    (SELECT SUM(NUM_IND_EJE) FROM  NEO_MX_MAE_IND_EJE WHERE ID_REG = P_TXT_ESTRC) AS TOT_CTES_REG
                           from  NEO_MX_MAE_IND_EJE
                           WHERE ID_REG = P_TXT_ESTRC
                           group by ID_REG,COD_STA_CTE) IE,
                    (select  TRUNC(SUM(NUM_IND_EJE)/COUNT(distinct(ID_REG)),2) PROM_CTES_REG,COD_STA_CTE
                           from  NEO_MX_MAE_IND_EJE group by COD_STA_CTE) PROM,
                    (select COD_STA_CTE,dsc_sta from neo_mx_mae_cat_sta_cte ) COD
                where IE.COD_STA_CTE = PROM.COD_STA_CTE
                and COD.COD_STA_CTE = PROM.COD_STA_CTE;                             
            ELSE
                DBMS_OUTPUT.PUT_LINE('ERROR, PLEASE SELECT THE OPTION 1 FOR ZONE OR 2 FOR REGION');
            END IF;
        END IF;
    END;

    PROCEDURE SP_NEO_MX_SUM_VAL_CTR_DET(
        P_TXT_ESTRC IN NUMBER,
        P_OPT IN NUMBER,
        P_SAL_NBR OUT NUMBER,
        P_SAL_TXT OUT VARCHAR2, 
        VAR_SUM_VAL_CART OUT SYS_REFCURSOR) IS

    v_Query VARCHAR2(5000);
    BEGIN

    v_Query:=' 
        SELECT TO_CHAR(ROUND(SUM(NVL(C.NUM_TOT_CART,0)),2)) AS TOT_CART
              ,CASE WHEN SUM(NVL(C.NUM_TOT_CART,0))=0 THEN TO_CHAR(0) 
                    WHEN SUM(NVL(C.NUM_TOT_ANT,0))=0 THEN TO_CHAR(100) 
                    ELSE TO_CHAR(ROUND((SUM(NVL(C.NUM_TOT_CART,0))/SUM(NVL(C.NUM_TOT_ANT,0))*100)-100,2)) END AS TOT_CART_PCT
              ,CASE WHEN SUM(NVL(C.NUM_TOT_CART,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(C.NUM_TOT_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(C.NUM_TOT_CART,0))>SUM(NVL(C.NUM_TOT_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(C.NUM_TOT_CART,0)) BETWEEN SUM(NVL(C.NUM_TOT_ANT,0)) AND SUM(NVL(C.NUM_TOT_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS TOT_CART_COL
              ,TO_CHAR(ROUND(SUM(NVL(C.NUM_CAPTA,0)),2)) AS CAPTACION
              ,CASE WHEN SUM(NVL(C.NUM_CAPTA,0))=0 THEN TO_CHAR(0)
                    WHEN SUM(NVL(C.NUM_CAPTA_ANT,0))=0 THEN TO_CHAR(100)
                    ELSE TO_CHAR(ROUND((SUM(NVL(C.NUM_CAPTA,0))/SUM(NVL(C.NUM_CAPTA_ANT,0))*100)-100,2)) END AS CAPTACION_PCT
              ,CASE WHEN SUM(NVL(C.NUM_CAPTA,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(C.NUM_CAPTA_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(C.NUM_CAPTA,0))>SUM(NVL(C.NUM_CAPTA_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(C.NUM_CAPTA,0)) BETWEEN SUM(NVL(C.NUM_CAPTA_ANT,0)) AND SUM(NVL(C.NUM_CAPTA_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS CAPTACION_COL
              ,TO_CHAR(ROUND(SUM(NVL(C.NUM_COLC,0)),2)) AS COLOCACION
              ,CASE WHEN SUM(NVL(C.NUM_COLC,0))=0 THEN TO_CHAR(0)
                    WHEN SUM(NVL(C.NUM_COLC_ANT,0))=0 THEN TO_CHAR(100)
                    ELSE TO_CHAR(ROUND((SUM(NVL(C.NUM_COLC,0))/SUM(NVL(C.NUM_COLC_ANT,0))*100)-100,2)) END AS COLOCACION_PCT
              ,CASE WHEN SUM(NVL(C.NUM_COLC,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(C.NUM_COLC_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(C.NUM_COLC,0))>SUM(NVL(C.NUM_COLC_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(C.NUM_COLC,0)) BETWEEN SUM(NVL(C.NUM_COLC_ANT,0)) AND SUM(NVL(C.NUM_COLC_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS COLOCACION_COL
              ,TO_CHAR(ROUND(SUM(NVL(C.NUM_MBB_CART,0)),2)) AS MBB
              ,CASE WHEN SUM(NVL(C.NUM_MBB_CART,0))=0 THEN TO_CHAR(0)
                    WHEN SUM(NVL(C.NUM_CTES_MBB_ANT,0))=0 THEN TO_CHAR(100)
                    ELSE TO_CHAR(ROUND((SUM(NVL(C.NUM_MBB_CART,0))/SUM(NVL(C.NUM_MBB_ANT,0))*100)-100,2)) END AS MBB_PCT
              ,CASE WHEN SUM(NVL(C.NUM_MBB_CART,0))=0 THEN ''FFE63D''
                    WHEN SUM(NVL(C.NUM_MBB_ANT,0))=0 THEN ''064627''
                    WHEN SUM(NVL(C.NUM_MBB_CART,0))>SUM(NVL(C.NUM_MBB_ANT,0))*1.05 THEN ''064627'' 
                    WHEN SUM(NVL(C.NUM_MBB_CART,0)) BETWEEN SUM(NVL(C.NUM_MBB_ANT,0)) AND SUM(NVL(C.NUM_MBB_ANT,0))*1.05 THEN ''FFE63D''
                    ELSE ''F71104'' END AS MBB_COL
              ,TO_CHAR(ROUND(SUM(NVL(C.NUM_VAR_DIA,0)),2)) AS VARIACION ';

    CASE P_OPT
        WHEN 0 THEN    
        v_Query:=v_Query|| ' 
              ,TO_CHAR(Z.ID_ZON) AS ID_ESTRUCTURA
              ,Z.TXT_NOM_ZON AS NOMBRE_ESTRUCTURA
          FROM NEO_MX_MAE_CAT_ZONS Z
          LEFT OUTER JOIN NEO_MX_MAE_VAL_CART C ON C.ID_ZON=Z.ID_ZON
         WHERE Z.ID_REG='||P_TXT_ESTRC||'
         GROUP BY TO_CHAR(Z.ID_ZON), Z.TXT_NOM_ZON '; 

        WHEN 1 THEN
        v_Query:=v_Query||' 
              ,TO_CHAR(S.NUM_CC) AS ID_ESTRUCTURA
              ,S.TXT_NOM_SUC AS NOMBRE_ESTRUCTURA               
          FROM NEO_MX_MAE_SUC S
          LEFT OUTER JOIN NEO_MX_MAE_VAL_CART C ON C.NUM_CC=S.NUM_CC
         WHERE S.ID_ZON='||P_TXT_ESTRC||'
         GROUP BY TO_CHAR(S.NUM_CC),S.TXT_NOM_SUC ';

        WHEN 2 THEN
        v_Query:=v_Query||' 
              ,TO_CHAR(E.TXT_OFI_ACT) AS ID_ESTRUCTURA
              ,E.TXT_NOM_EJV AS NOMBRE_ESTRUCTURA
          FROM NEO_MX_MAE_EJEC E
          LEFT OUTER JOIN NEO_MX_MAE_VAL_CART C ON C.TXT_OFI_ACT=E.TXT_OFI_ACT
         WHERE E.NUM_CC='||P_TXT_ESTRC||'
         GROUP BY TO_CHAR(E.TXT_OFI_ACT),E.TXT_NOM_EJV ';

        ELSE
            P_SAL_NBR:=0;
            P_SAL_TXT:='SE INGRESO UNA OPCIÓN NO VÁLIDA (NIVEL DE ESTRUCTURA)';
    END CASE;

    v_Query:=v_Query||' ORDER BY TOT_CART DESC,CAPTACION DESC';

    DBMS_OUTPUT.PUT_LINE(v_Query);

    open VAR_SUM_VAL_CART for v_Query;
    P_SAL_NBR:=1;
    P_SAL_TXT:='CONSULTA EXITOSA';

    EXCEPTION
    WHEN NO_DATA_FOUND THEN
        P_SAL_NBR:=0;
        P_SAL_TXT:='NO EXISTEN DATOS: '||SQLERRM();
    WHEN OTHERS THEN
        P_SAL_NBR:=0;
        P_SAL_TXT:='ERROR: '||SQLERRM();
  END;

-----------------------------------------------------------------------------------------------
    PROCEDURE SP_GET_GRAP_CART(
            P_OPC IN NUMBER,
            P_TXT_ESTR IN VARCHAR2,
            P_FCH_INI VARCHAR2,
            P_FCH_FIN VARCHAR2,
            P_SAL_NBR OUT NUMBER,
            P_SAL_TXT OUT VARCHAR2, 
            P_JSON OUT VARCHAR2) IS

    V_NO_CTES NUMBER(12);
    V_NO_CTES_GEST NUMBER(12);
    V_NO_CTES_NO_GEST NUMBER(12);
    V_porClientesGes NUMBER(12,2);
    V_porClientesNoGes NUMBER(12,2);
    V_ctesNoGes NUMBER(12,2);
    V_SQL VARCHAR2(10000);

    BEGIN
    --PARA OBTENER EL VALOR DE LA CARTERA, NIVEL EJECUTIVO Y SUCURSAL

    IF P_OPC=1 THEN
        --PARA OBTENER EL NUMERO DE CLIENTES
        V_SQL := '
            SELECT SUM(NUM_IND_EJE)
              FROM NEO_MX_MAE_IND_EJE MIE JOIN NEO_MX_MAE_EJEC ME
                ON MIE.TXT_OFI_ACT=ME.TXT_OFI_ACT
             WHERE MIE.COD_STA_CTE IN (1,2,3,4)    
               AND MIE.TXT_OFI_ACT = ''' || P_TXT_ESTR || ''' ';

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO V_NO_CTES;

        --PARA OBTENER EL NUMERO DE CLIENTES GESTIONADOS
        V_SQL :='    
            SELECT COUNT(1)      
              FROM NEO_MX_MAE_CTR A
              JOIN NEO_MX_MAE_CTR_GES B ON A.NUM_BUC_CTE=B.NUM_BUC_CTE
             WHERE A.TXT_OFI_ACT = ''' || P_TXT_ESTR || ''' ';

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO V_NO_CTES_GEST;

        V_porClientesGes:=ROUND((V_NO_CTES_GEST/V_NO_CTES*100),2);
        V_porClientesNoGes:=ROUND(100-(V_NO_CTES_GEST/V_NO_CTES*100),2);
        V_ctesNoGes:=V_NO_CTES - V_NO_CTES_GEST;

        DBMS_OUTPUT.PUT_LINE('TOTAL CTES: '||V_NO_CTES);
        DBMS_OUTPUT.PUT_LINE('CTES GEST: '||V_NO_CTES_GEST);
        DBMS_OUTPUT.PUT_LINE('CTES NO GEST: '||V_ctesNoGes);
        DBMS_OUTPUT.PUT_LINE('PORC CTES GEST: '||V_porClientesGes);
        DBMS_OUTPUT.PUT_LINE('PORC CTES NO GEST: '||V_porClientesNoGes);

        --OBTIENE LOS DATOS NECESARIOS PARA LLENAR LA GRAFICA DE LA CARTERA CON LOS VALORES CORRESPONDIENTES FORMATO JSON    
        DBMS_OUTPUT.PUT_LINE('QUERY PARA EJECUTIVO');
        V_SQL:= 'SELECT ''['' || LISTAGG(SALIDA, '','') WITHIN GROUP (ORDER BY SALIDA) || '']'' LISTA_NOMBRES
                   FROM (SELECT JSON_QUERY(
                                ''{mediaZona:''|| MIE.NUM_MED_IND || '',
                                   valorZona:''|| MIE.NUM_IND_EJE || '',
                                   totalClientes:'''''|| V_NO_CTES ||''''',
                                   clientesGes:'''''|| V_NO_CTES_GEST ||''''',
                                   porClientesGes:''''' || V_porClientesGes ||''''',
                                   porClientesNoGes:'''''|| V_porClientesNoGes ||''''',
                                   clientesNoGes:''''' || V_ctesNoGes ||''''',
                                   nombreBloq:''''''|| CSC.DSC_STA || ''''''}'',''$'') AS SALIDA
                  FROM NEO_MX_MAE_IND_EJE MIE 
                  JOIN NEO_MX_MAE_CAT_STA_CTE CSC ON MIE.COD_STA_CTE=CSC.COD_STA_CTE
                  JOIN NEO_MX_MAE_EJEC ME ON MIE.TXT_OFI_ACT=ME.TXT_OFI_ACT 
                 WHERE MIE.TXT_OFI_ACT = ''' || P_TXT_ESTR || ''' )';
                   --AND MIE.FCH_ALT BETWEEN TO_DATE(''' || P_FCH_INI || ''',''DD/MM/YYYY'') AND TO_DATE(''' || P_FCH_FIN || ''',''DD/MM/YYYY''))';

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO P_JSON;

        DBMS_OUTPUT.PUT_LINE(P_JSON);
    ELSE
        --PARA OBTENER EL NUMERO DE CLIENTES
        V_SQL := '
            SELECT SUM(NUM_IND_EJE)
              FROM NEO_MX_MAE_IND_EJE MIE 
			  JOIN NEO_MX_MAE_EJEC ME
                ON MIE.TXT_OFI_ACT=ME.TXT_OFI_ACT
			  JOIN NEO_MX_MAE_SUC SUC ON SUC.NUM_CC=ME.NUM_CC
             WHERE MIE.COD_STA_CTE IN (1,2,3,4)    
               AND SUC.NUM_SUC_NDR = ' || P_TXT_ESTR ;

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO V_NO_CTES;

        --PARA OBTENER EL NUMERO DE CLIENTES GESTIONADOS
        V_SQL :='    
            SELECT COUNT(1)      
              FROM NEO_MX_MAE_CTR A
            JOIN NEO_MX_MAE_CTR_GES B ON A.NUM_BUC_CTE=B.NUM_BUC_CTE
            JOIN NEO_MX_MAE_EJEC ME ON A.TXT_OFI_ACT=ME.TXT_OFI_ACT
            JOIN NEO_MX_MAE_SUC SUC ON SUC.NUM_CC=ME.NUM_CC
            WHERE SUC.NUM_SUC_NDR=' || P_TXT_ESTR || ' ';

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO V_NO_CTES_GEST;

        V_porClientesGes:=ROUND((V_NO_CTES_GEST/V_NO_CTES*100),2);
        V_porClientesNoGes:=ROUND(100-(V_NO_CTES_GEST/V_NO_CTES*100),2);
        V_ctesNoGes:=V_NO_CTES - V_NO_CTES_GEST;

        DBMS_OUTPUT.PUT_LINE('TOTAL CTES: '||V_NO_CTES);
        DBMS_OUTPUT.PUT_LINE('CTES GEST: '||V_NO_CTES_GEST);
        DBMS_OUTPUT.PUT_LINE('CTES NO GEST: '||V_ctesNoGes);
        DBMS_OUTPUT.PUT_LINE('PORC CTES GEST: '||V_porClientesGes);
        DBMS_OUTPUT.PUT_LINE('PORC CTES NO GEST: '||V_porClientesNoGes);

        DBMS_OUTPUT.PUT_LINE('QUERY PARA SUCURSAL');
        V_SQL:= 'SELECT ''['' || LISTAGG(SALIDA, '','') WITHIN GROUP (ORDER BY SALIDA) || '']'' LISTA_NOMBRES
                   FROM (SELECT JSON_QUERY(
                                ''{mediaZona:''|| SUM(MIE.NUM_MED_IND) || '',
                                   valorZona:''|| SUM(MIE.NUM_IND_EJE) || '',
                                   totalClientes:'''''|| V_NO_CTES ||''''',
                                   clientesGes:'''''|| V_NO_CTES_GEST ||''''',
                                   porClientesGes:''''' || V_porClientesGes ||''''',
                                   porClientesNoGes:'''''|| V_porClientesNoGes ||''''',
                                   clientesNoGes:''''' || V_ctesNoGes ||''''',
                                   nombreBloq:''''''|| CSC.DSC_STA || ''''''}'',''$'') AS SALIDA
                  FROM NEO_MX_MAE_IND_EJE MIE 
                  JOIN NEO_MX_MAE_CAT_STA_CTE CSC ON MIE.COD_STA_CTE=CSC.COD_STA_CTE
                  JOIN NEO_MX_MAE_EJEC ME ON MIE.TXT_OFI_ACT=ME.TXT_OFI_ACT 
				  JOIN NEO_MX_MAE_SUC SUC ON SUC.NUM_CC=ME.NUM_CC
                 WHERE SUC.NUM_SUC_NDR = ''' || P_TXT_ESTR || '''
                 GROUP BY CSC.DSC_STA)';

        dbms_output.put_line(V_SQL);
        EXECUTE IMMEDIATE V_SQL INTO P_JSON;

        DBMS_OUTPUT.PUT_LINE(P_JSON);

    END IF;

    P_SAL_NBR:=1;
    P_SAL_TXT:='CONSULTA EXITOSA';

    EXCEPTION
    WHEN NO_DATA_FOUND THEN
        P_SAL_NBR:=0;
        P_SAL_TXT:='NO EXISTEN DATOS: '||SQLERRM();
    WHEN OTHERS THEN
        P_SAL_NBR:=0;
        P_SAL_TXT:='ERROR: '||SQLERRM();

    END;

END;

/
