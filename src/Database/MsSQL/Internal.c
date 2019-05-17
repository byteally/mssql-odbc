
#define UNICODE

#include <stdio.h>

#include <stdlib.h>

#include <sqlext.h>

#include <sqltypes.h>

#include <sqlucode.h>

#include <msodbcsql.h>

int inline_c_Database_MsSQL_Internal_0_12568841e06c0015be316b5c75973f35c46bee9e(SQLHENV * henvp_inline_c_0, SQLHDBC * hdbcp_inline_c_1, SQLWCHAR * ctxt_inline_c_2, int ctxtLen_inline_c_3) {

        SQLRETURN ret = 0;
        SQLHENV* henvp = henvp_inline_c_0;
        SQLHDBC* hdbcp = hdbcp_inline_c_1;
      
        ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, henvp);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLSetEnvAttr(*henvp, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLAllocHandle(SQL_HANDLE_DBC, *henvp, hdbcp);
        if (!SQL_SUCCEEDED(ret)) return ret;

        ret = SQLSetConnectAttr(*hdbcp, SQL_LOGIN_TIMEOUT, (SQLPOINTER)5, 0);
        if (!SQL_SUCCEEDED(ret)) return ret;

        SQLWCHAR* cstr = ctxt_inline_c_2;
        ret = SQLDriverConnectW(*hdbcp, 0, cstr, (SQLSMALLINT)ctxtLen_inline_c_3, 0, 0, 0, SQL_DRIVER_NOPROMPT);

        return ret;
        
}


int inline_c_Database_MsSQL_Internal_1_19828a9829c1a96b5a7cb7ccc5f4e7ec80a1127e(SQLHENV henv_inline_c_0, SQLHDBC hdbc_inline_c_1) {

    SQLRETURN ret = 0;
    SQLHENV henv = henv_inline_c_0;
    SQLHDBC hdbc = hdbc_inline_c_1;

    if (hdbc != SQL_NULL_HDBC) {
      ret = SQLDisconnect(hdbc);
      if (!SQL_SUCCEEDED(ret)) return ret;
      
      ret = SQLFreeHandle(SQL_HANDLE_DBC, hdbc);
      if (!SQL_SUCCEEDED(ret)) return ret;
    }

    if (henv != SQL_NULL_HENV)
    {
      ret = SQLFreeHandle(SQL_HANDLE_ENV, henv);
      if (!SQL_SUCCEEDED(ret)) return ret;
    }
    return ret;

  
}


int inline_c_Database_MsSQL_Internal_2_78c9f54187ca27c1a76c3e445bce8c4a2963c42d(SQLHANDLE handle_inline_c_0, void (* appendMessage_inline_c_1)(SQLWCHAR *, int , SQLWCHAR *, int ), int handleType_inline_c_2) {

             SQLRETURN ret = 0;
             SQLSMALLINT i = 0;
             SQLWCHAR eState [6]; 
             SQLWCHAR eMSG [SQL_MAX_MESSAGE_LENGTH];
             SQLSMALLINT eMSGLen;
             SQLHANDLE handle = handle_inline_c_0;
             void (*appendMessage)(SQLWCHAR*, int, SQLWCHAR*, int) = appendMessage_inline_c_1;
             do {
               ret = SQLGetDiagRecW((SQLSMALLINT)handleType_inline_c_2, handle, ++i, eState, NULL, eMSG, 1000, &eMSGLen);
               if (SQL_SUCCEEDED(ret)) {
                  appendMessage(eState, 5, eMSG, eMSGLen);
               }
             } while( ret == SQL_SUCCESS );

             if (!SQL_SUCCEEDED(ret)) return ret;
             
             
             return 0;
         
}


int inline_c_Database_MsSQL_Internal_3_795858118c95cd1f23b430e443725afc8e2bece6(SQLHSTMT hstmt_inline_c_0, SQLSMALLINT * numResultColsFP_inline_c_1, SQLWCHAR * queryWStr_inline_c_2, int queryLen_inline_c_3) {

    SQLRETURN ret = 0;
    SQLHSTMT hstmt = hstmt_inline_c_0;
    SQLSMALLINT* numColumnPtr = numResultColsFP_inline_c_1;

    ret = SQLExecDirectW(hstmt, queryWStr_inline_c_2, queryLen_inline_c_3);
    if (!SQL_SUCCEEDED(ret)) return ret;

    ret = SQLNumResultCols(hstmt, numColumnPtr);

    return ret;
    
}


SQLRETURN inline_c_Database_MsSQL_Internal_4_0dd5b1b0e4a1a61b839e9079d1cae4f153bf68cc(SQLHSTMT * hstmtp_inline_c_0, SQLHDBC hdbc_inline_c_1) {

      SQLRETURN ret = 0;
      SQLHSTMT* hstmtp = hstmtp_inline_c_0;
      SQLHDBC hdbc = hdbc_inline_c_1;
      ret = SQLAllocHandle(SQL_HANDLE_STMT, hdbc, hstmtp);
      return ret;
    
}


SQLRETURN inline_c_Database_MsSQL_Internal_5_22d5ab2bf77192c03edc09c3f3479fa93b794d9d(SQLHSTMT hstmt_inline_c_0) {

      SQLRETURN ret = 0;
      SQLHSTMT hstmt =  hstmt_inline_c_0;
      if (hstmt != SQL_NULL_HSTMT) {
        ret = SQLFreeHandle(SQL_HANDLE_STMT, hstmt);
      }
      return ret;
  
}


int inline_c_Database_MsSQL_Internal_6_ac82ef710ac4247bd002f455343710bba38e8ff6(SQLHSTMT hstmt_inline_c_0) {

      SQLRETURN ret = 0;
      SQLHSTMT hstmt = hstmt_inline_c_0;

      ret = SQLFetch(hstmt);

      return ret;
  
}


SQLRETURN inline_c_Database_MsSQL_Internal_7_c91dd9f3ff5280a0b1faffaf347e86300a473f13(SQLHDBC hdbc_inline_c_0, SQLWCHAR * infoFP_inline_c_1, SQLSMALLINT * bufferSizeOut_inline_c_2) {

      SQLRETURN ret = 0;
      SQLHDBC hdbc = hdbc_inline_c_0;
      ret = SQLGetInfo(hdbc, SQL_DATABASE_NAME, infoFP_inline_c_1, (SQLSMALLINT)(16 * 1024), bufferSizeOut_inline_c_2);
      return ret;
  
}


SQLRETURN inline_c_Database_MsSQL_Internal_8_06a444af8a2e153b221c6e98d561cf5789de65fc(SQLHDBC hdbc_inline_c_0, SQLUINTEGER * infoP_inline_c_1) {

             SQLRETURN ret = 0;
             SQLHDBC hdbc = hdbc_inline_c_0;
             ret = SQLGetInfo(hdbc, SQL_MAX_CONCURRENT_ACTIVITIES, infoP_inline_c_1, (SQLSMALLINT)(sizeof(SQLUINTEGER)), NULL);
             return ret;
           
}


SQLRETURN inline_c_Database_MsSQL_Internal_9_51cc2d23044b16fed831eb48ae395b8f22a4425c(SQLHSTMT hstmt_inline_c_0, SQLSMALLINT * nameLengthP_inline_c_1, SQLSMALLINT * dataTypeP_inline_c_2, SQLSMALLINT * decimalDigitsP_inline_c_3, SQLSMALLINT * nullableP_inline_c_4, SQLULEN * colSizeP_inline_c_5, SQLWCHAR * tabNameP_inline_c_6, SQLUSMALLINT colPos_27_inline_c_7) {

                          SQLRETURN ret = 0;
                          SQLHSTMT hstmt = hstmt_inline_c_0;
                          SQLSMALLINT* nameLengthP = nameLengthP_inline_c_1;
                          SQLSMALLINT* dataTypeP = dataTypeP_inline_c_2;
                          SQLSMALLINT* decimalDigitsP = decimalDigitsP_inline_c_3;
                          SQLSMALLINT* nullableP = nullableP_inline_c_4;
                          SQLULEN* colSizeP = colSizeP_inline_c_5;
                          SQLWCHAR* tabNameP = tabNameP_inline_c_6;
               
                          ret = SQLDescribeColW(hstmt, colPos_27_inline_c_7, tabNameP, 16 * 128, nameLengthP, dataTypeP, colSizeP, decimalDigitsP, nullableP);
                          return ret;
                      
}


int inline_c_Database_MsSQL_Internal_10_743af036dde7cbdf9cb90a1673b1a39fe15cc219(SQLHSTMT hstmt_inline_c_0, SQLLEN * rcountP_inline_c_1) {

        SQLRETURN ret = 0;
        SQLHSTMT hstmt = hstmt_inline_c_0;

        ret = SQLRowCount(hstmt, rcountP_inline_c_1);

        return ret;
    
}


SQLRETURN inline_c_Database_MsSQL_Internal_11_bef44e6222a3d00bc60bd3a2d9b66430bb1ccc18() {
return (SQL_NULL_DATA);
}


SQLRETURN inline_c_Database_MsSQL_Internal_12_bef44e6222a3d00bc60bd3a2d9b66430bb1ccc18() {
return (SQL_NULL_DATA);
}


SQLRETURN inline_c_Database_MsSQL_Internal_13_f890b8a5b459b5fc3487cdd5a67aa8301ea710c6() {
return (SQL_NO_TOTAL);
}


SQLRETURN inline_c_Database_MsSQL_Internal_14_f890b8a5b459b5fc3487cdd5a67aa8301ea710c6() {
return (SQL_NO_TOTAL);
}


SQLRETURN inline_c_Database_MsSQL_Internal_15_13167adbe82e58d1de19cb694018975a09c0d672() {
return (SQL_DATA_AT_EXEC);
}


SQLRETURN inline_c_Database_MsSQL_Internal_16_13167adbe82e58d1de19cb694018975a09c0d672() {
return (SQL_DATA_AT_EXEC);
}


SQLRETURN inline_c_Database_MsSQL_Internal_17_ec7ce4af0c85cf12512061f8bef902c048a12817() {
return (SQL_SUCCESS);
}


SQLRETURN inline_c_Database_MsSQL_Internal_18_ec7ce4af0c85cf12512061f8bef902c048a12817() {
return (SQL_SUCCESS);
}


SQLRETURN inline_c_Database_MsSQL_Internal_19_7b879982df057d21415132a9058be1e93d42b5c2() {
return (SQL_SUCCESS_WITH_INFO);
}


SQLRETURN inline_c_Database_MsSQL_Internal_20_7b879982df057d21415132a9058be1e93d42b5c2() {
return (SQL_SUCCESS_WITH_INFO);
}


SQLRETURN inline_c_Database_MsSQL_Internal_21_4d2d0d9a09ebf992c1378c18ba6eef7a4f1f1211() {
return (SQL_NO_DATA);
}


SQLRETURN inline_c_Database_MsSQL_Internal_22_4d2d0d9a09ebf992c1378c18ba6eef7a4f1f1211() {
return (SQL_NO_DATA);
}


SQLRETURN inline_c_Database_MsSQL_Internal_23_c54da332d94e513651198fe2a2a6e1f73e7d4a01() {
return (SQL_ERROR);
}


SQLRETURN inline_c_Database_MsSQL_Internal_24_c54da332d94e513651198fe2a2a6e1f73e7d4a01() {
return (SQL_ERROR);
}


SQLRETURN inline_c_Database_MsSQL_Internal_25_17eae171e54527714495f0ac6e19493e37359e49() {
return (SQL_INVALID_HANDLE);
}


SQLRETURN inline_c_Database_MsSQL_Internal_26_17eae171e54527714495f0ac6e19493e37359e49() {
return (SQL_INVALID_HANDLE);
}


SQLRETURN inline_c_Database_MsSQL_Internal_27_2224e6215480f1837a864276cad4c6cfceccc506() {
return (SQL_STILL_EXECUTING);
}


SQLRETURN inline_c_Database_MsSQL_Internal_28_2224e6215480f1837a864276cad4c6cfceccc506() {
return (SQL_STILL_EXECUTING);
}


SQLRETURN inline_c_Database_MsSQL_Internal_29_f08ddcb98819f7abc4379b8f1b00529ce1fd98fb() {
return (SQL_NEED_DATA);
}


SQLRETURN inline_c_Database_MsSQL_Internal_30_f08ddcb98819f7abc4379b8f1b00529ce1fd98fb() {
return (SQL_NEED_DATA);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_31_5e2ed054d8456a2ebabaabcbd379d92799ccc4db() {
return (SQL_UNKNOWN_TYPE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_32_5e2ed054d8456a2ebabaabcbd379d92799ccc4db() {
return (SQL_UNKNOWN_TYPE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_33_ccb625c238c49245229cd75da9de9bdebe83e36e() {
return (SQL_CHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_34_ccb625c238c49245229cd75da9de9bdebe83e36e() {
return (SQL_CHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_35_a10a97c5713491cfc8d2dde7d0e8b42403c976cf() {
return (SQL_NUMERIC);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_36_a10a97c5713491cfc8d2dde7d0e8b42403c976cf() {
return (SQL_NUMERIC);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_37_30b79c7de9c296fb3f20ae8b97e5bc527da2d596() {
return (SQL_DECIMAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_38_30b79c7de9c296fb3f20ae8b97e5bc527da2d596() {
return (SQL_DECIMAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_39_98ea40fcb41b696f995a31d6373cbaa952600993() {
return (SQL_INTEGER);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_40_98ea40fcb41b696f995a31d6373cbaa952600993() {
return (SQL_INTEGER);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_41_5b9a63bb99511774d4e47253def4a17f30df2b80() {
return (SQL_SMALLINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_42_5b9a63bb99511774d4e47253def4a17f30df2b80() {
return (SQL_SMALLINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_43_bf57bc0d0074a836768dc193f337f597b1c10f29() {
return (SQL_FLOAT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_44_bf57bc0d0074a836768dc193f337f597b1c10f29() {
return (SQL_FLOAT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_45_51847690d19ac66c64d5cf6e9fc2468e761c429f() {
return (SQL_REAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_46_51847690d19ac66c64d5cf6e9fc2468e761c429f() {
return (SQL_REAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_47_4018233afe71fbcb715f63154d5c9871a0a8891d() {
return (SQL_DOUBLE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_48_4018233afe71fbcb715f63154d5c9871a0a8891d() {
return (SQL_DOUBLE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_49_ad970ac134ad13922b18708ed61312104d854a8a() {
return (SQL_DATETIME);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_50_ad970ac134ad13922b18708ed61312104d854a8a() {
return (SQL_DATETIME);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_51_c464f790a372076d66a042ed996dcc451ff20ee1() {
return (SQL_VARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_52_c464f790a372076d66a042ed996dcc451ff20ee1() {
return (SQL_VARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_53_15c0ceb718632bc4903322be8848e1aff7fe69fe() {
return (SQL_DATE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_54_15c0ceb718632bc4903322be8848e1aff7fe69fe() {
return (SQL_DATE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_55_f495beb623feb1a0d18df42db469759b815320c5() {
return (SQL_TYPE_DATE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_56_f495beb623feb1a0d18df42db469759b815320c5() {
return (SQL_TYPE_DATE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_57_564ba52b14f4459d4078462b08b2908f6fed3b7f() {
return (SQL_INTERVAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_58_564ba52b14f4459d4078462b08b2908f6fed3b7f() {
return (SQL_INTERVAL);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_59_f01b2f69c8a6a1f16b0bac12df3f30bafb5a1c2c() {
return (SQL_TIME);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_60_f01b2f69c8a6a1f16b0bac12df3f30bafb5a1c2c() {
return (SQL_TIME);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_61_709356e18256a4d24302af1b8743e56f283e4f59() {
return (SQL_TIMESTAMP);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_62_709356e18256a4d24302af1b8743e56f283e4f59() {
return (SQL_TIMESTAMP);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_63_01f5bc9be2ecd2fbdb8ced4ce18157f9fb75c463() {
return (SQL_TYPE_TIMESTAMP);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_64_01f5bc9be2ecd2fbdb8ced4ce18157f9fb75c463() {
return (SQL_TYPE_TIMESTAMP);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_65_eea8c7945e40002675667df5712e1b063a2d1410() {
return (SQL_LONGVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_66_eea8c7945e40002675667df5712e1b063a2d1410() {
return (SQL_LONGVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_67_55638e090f8a1e53b19710fa6e023e83b00bbea1() {
return (SQL_BINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_68_55638e090f8a1e53b19710fa6e023e83b00bbea1() {
return (SQL_BINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_69_9cc0d57e9cac97910ecfbc54235798ab42f4e1fe() {
return (SQL_VARBINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_70_9cc0d57e9cac97910ecfbc54235798ab42f4e1fe() {
return (SQL_VARBINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_71_c249834b9f90163598795147e97c047502afd8f7() {
return (SQL_LONGVARBINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_72_c249834b9f90163598795147e97c047502afd8f7() {
return (SQL_LONGVARBINARY);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_73_adf0f02b722d13efb79b728d2e9ba8b439bc82f4() {
return (SQL_BIGINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_74_adf0f02b722d13efb79b728d2e9ba8b439bc82f4() {
return (SQL_BIGINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_75_76df75f3597fa6413da7df4ac488c1108282d189() {
return (SQL_TINYINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_76_76df75f3597fa6413da7df4ac488c1108282d189() {
return (SQL_TINYINT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_77_be877581cb90565c8c4553cc7ec98ff0de9d713b() {
return (SQL_BIT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_78_be877581cb90565c8c4553cc7ec98ff0de9d713b() {
return (SQL_BIT);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_79_e653c4e3c9c7459682b23ea48a1a9fa2f0478a3f() {
return (SQL_GUID);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_80_e653c4e3c9c7459682b23ea48a1a9fa2f0478a3f() {
return (SQL_GUID);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_81_ba814d6bb54009e686d8afba7613b227ef8d946a() {
return (SQL_WCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_82_ba814d6bb54009e686d8afba7613b227ef8d946a() {
return (SQL_WCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_83_ad380b00552f8c14735a33c1a654d3491dfde8f3() {
return (SQL_WVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_84_ad380b00552f8c14735a33c1a654d3491dfde8f3() {
return (SQL_WVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_85_01cd838a0219cb225c18fd7d071d3cabdb2b7aa7() {
return (SQL_WLONGVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_86_01cd838a0219cb225c18fd7d071d3cabdb2b7aa7() {
return (SQL_WLONGVARCHAR);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_87_a0b560cef4ffb7fe7edbd818566f63541be55605() {
return (SQL_SS_TIME2);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_88_a0b560cef4ffb7fe7edbd818566f63541be55605() {
return (SQL_SS_TIME2);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_89_4303c72b67f58319d6364975e0d1333fb284eed4() {
return (SQL_SS_TIMESTAMPOFFSET);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_90_4303c72b67f58319d6364975e0d1333fb284eed4() {
return (SQL_SS_TIMESTAMPOFFSET);
}


int inline_c_Database_MsSQL_Internal_91_d036833257f55d3cbe3494cc008c057f93145b12() {
return (SQL_C_CHAR);
}


int inline_c_Database_MsSQL_Internal_92_d036833257f55d3cbe3494cc008c057f93145b12() {
return (SQL_C_CHAR);
}


int inline_c_Database_MsSQL_Internal_93_39e86f548fe745254fc2a522557ca54cf7d8faaf() {
return (SQL_C_LONG);
}


int inline_c_Database_MsSQL_Internal_94_39e86f548fe745254fc2a522557ca54cf7d8faaf() {
return (SQL_C_LONG);
}


int inline_c_Database_MsSQL_Internal_95_ada96119974a5aa2d1cb749faab39108c622801c() {
return (SQL_C_SHORT);
}


int inline_c_Database_MsSQL_Internal_96_ada96119974a5aa2d1cb749faab39108c622801c() {
return (SQL_C_SHORT);
}


int inline_c_Database_MsSQL_Internal_97_323c7345e202b41177deb719f5d340330f9f64f2() {
return (SQL_C_FLOAT);
}


int inline_c_Database_MsSQL_Internal_98_323c7345e202b41177deb719f5d340330f9f64f2() {
return (SQL_C_FLOAT);
}


int inline_c_Database_MsSQL_Internal_99_09dec327b505d1c14f64c8e6ae2ce0e7ec573cb3() {
return (SQL_C_DOUBLE);
}


int inline_c_Database_MsSQL_Internal_100_09dec327b505d1c14f64c8e6ae2ce0e7ec573cb3() {
return (SQL_C_DOUBLE);
}


int inline_c_Database_MsSQL_Internal_101_bde2791a7fc1b883888859e75a70a77e99777606() {
return (SQL_C_NUMERIC);
}


int inline_c_Database_MsSQL_Internal_102_bde2791a7fc1b883888859e75a70a77e99777606() {
return (SQL_C_NUMERIC);
}


int inline_c_Database_MsSQL_Internal_103_7ff58331d4c2060d881c385223a081b472ea75df() {
return (SQL_C_DEFAULT);
}


int inline_c_Database_MsSQL_Internal_104_7ff58331d4c2060d881c385223a081b472ea75df() {
return (SQL_C_DEFAULT);
}


int inline_c_Database_MsSQL_Internal_105_2b95410dc90e45053ab6c439848c027b601322fc() {
return (SQL_C_DATE);
}


int inline_c_Database_MsSQL_Internal_106_2b95410dc90e45053ab6c439848c027b601322fc() {
return (SQL_C_DATE);
}


int inline_c_Database_MsSQL_Internal_107_f323af6dff377d44b556228a4bbce9328ebe0336() {
return (SQL_C_TIME);
}


int inline_c_Database_MsSQL_Internal_108_f323af6dff377d44b556228a4bbce9328ebe0336() {
return (SQL_C_TIME);
}


int inline_c_Database_MsSQL_Internal_109_715fef0c443ac9aeaa667163cda2cc481f6509ed() {
return (SQL_C_TIMESTAMP);
}


int inline_c_Database_MsSQL_Internal_110_715fef0c443ac9aeaa667163cda2cc481f6509ed() {
return (SQL_C_TIMESTAMP);
}


int inline_c_Database_MsSQL_Internal_111_244e4198d11f2226c2e0ce39f2c2ae83aa9c28de() {
return (SQL_C_WCHAR);
}


int inline_c_Database_MsSQL_Internal_112_244e4198d11f2226c2e0ce39f2c2ae83aa9c28de() {
return (SQL_C_WCHAR);
}


int inline_c_Database_MsSQL_Internal_113_e0455d5def34e110e760ce1af4e93776494d6e9d() {
return (SQL_HANDLE_ENV);
}


int inline_c_Database_MsSQL_Internal_114_e0455d5def34e110e760ce1af4e93776494d6e9d() {
return (SQL_HANDLE_ENV);
}


int inline_c_Database_MsSQL_Internal_115_96bfe676e1d20819fae460057c51506274dd8325() {
return (SQL_HANDLE_DBC);
}


int inline_c_Database_MsSQL_Internal_116_96bfe676e1d20819fae460057c51506274dd8325() {
return (SQL_HANDLE_DBC);
}


int inline_c_Database_MsSQL_Internal_117_b93a9ab2f977688ee8629e2e4c954648e101003a() {
return (SQL_HANDLE_STMT);
}


int inline_c_Database_MsSQL_Internal_118_b93a9ab2f977688ee8629e2e4c954648e101003a() {
return (SQL_HANDLE_STMT);
}


int inline_c_Database_MsSQL_Internal_119_b50a716dc36f7c5a6410abc006b7da5611d69579() {
return (SQL_HANDLE_DESC);
}


int inline_c_Database_MsSQL_Internal_120_b50a716dc36f7c5a6410abc006b7da5611d69579() {
return (SQL_HANDLE_DESC);
}


int inline_c_Database_MsSQL_Internal_121_e7a1a9d8bb5ea0753d022c8b7d133fe41d931223() {
return (SQL_ATTR_ACCESS_MODE);
}


int inline_c_Database_MsSQL_Internal_122_e7a1a9d8bb5ea0753d022c8b7d133fe41d931223() {
return (SQL_ATTR_ACCESS_MODE);
}


int inline_c_Database_MsSQL_Internal_123_b0decd0cc14c7e6716bc9f4ac14d8f3331d740f3() {
return (SQL_ATTR_AUTOCOMMIT);
}


int inline_c_Database_MsSQL_Internal_124_b0decd0cc14c7e6716bc9f4ac14d8f3331d740f3() {
return (SQL_ATTR_AUTOCOMMIT);
}


int inline_c_Database_MsSQL_Internal_125_044b106f8547db3b8fc5b291ac557393183efa2f() {
return (SQL_ATTR_CONNECTION_TIMEOUT);
}


int inline_c_Database_MsSQL_Internal_126_044b106f8547db3b8fc5b291ac557393183efa2f() {
return (SQL_ATTR_CONNECTION_TIMEOUT);
}


int inline_c_Database_MsSQL_Internal_127_bf2183a7c446eef790f8e538fd514393f9d3346a() {
return (SQL_ATTR_CURRENT_CATALOG);
}


int inline_c_Database_MsSQL_Internal_128_bf2183a7c446eef790f8e538fd514393f9d3346a() {
return (SQL_ATTR_CURRENT_CATALOG);
}


int inline_c_Database_MsSQL_Internal_129_ddc362d590c38fab0e5e0816744e381bcf21aa51() {
return (SQL_ATTR_DISCONNECT_BEHAVIOR);
}


int inline_c_Database_MsSQL_Internal_130_ddc362d590c38fab0e5e0816744e381bcf21aa51() {
return (SQL_ATTR_DISCONNECT_BEHAVIOR);
}


int inline_c_Database_MsSQL_Internal_131_84db706a5e93c4ede2eee3cfe9f47c3c73834467() {
return (SQL_ATTR_ENLIST_IN_DTC);
}


int inline_c_Database_MsSQL_Internal_132_84db706a5e93c4ede2eee3cfe9f47c3c73834467() {
return (SQL_ATTR_ENLIST_IN_DTC);
}


int inline_c_Database_MsSQL_Internal_133_6e8de97d01da1ca915eb959dc2b588d511155203() {
return (SQL_ATTR_ENLIST_IN_XA);
}


int inline_c_Database_MsSQL_Internal_134_6e8de97d01da1ca915eb959dc2b588d511155203() {
return (SQL_ATTR_ENLIST_IN_XA);
}


int inline_c_Database_MsSQL_Internal_135_3fdbb7d56979c2a92d7a5fa3cd37c6e06bb9f244() {
return (SQL_ATTR_LOGIN_TIMEOUT);
}


int inline_c_Database_MsSQL_Internal_136_3fdbb7d56979c2a92d7a5fa3cd37c6e06bb9f244() {
return (SQL_ATTR_LOGIN_TIMEOUT);
}


int inline_c_Database_MsSQL_Internal_137_fd56dce9d90bfa8c3cd1a61db3e4093f204434d8() {
return (SQL_ATTR_ODBC_CURSORS);
}


int inline_c_Database_MsSQL_Internal_138_fd56dce9d90bfa8c3cd1a61db3e4093f204434d8() {
return (SQL_ATTR_ODBC_CURSORS);
}


int inline_c_Database_MsSQL_Internal_139_78c6df8dfa80754db30b19743d52ea8c89efdcfc() {
return (SQL_ATTR_PACKET_SIZE);
}


int inline_c_Database_MsSQL_Internal_140_78c6df8dfa80754db30b19743d52ea8c89efdcfc() {
return (SQL_ATTR_PACKET_SIZE);
}


int inline_c_Database_MsSQL_Internal_141_162107dcd0974532a3c81fc1eb5f1e96e27e4fa0() {
return (SQL_ATTR_QUIET_MODE);
}


int inline_c_Database_MsSQL_Internal_142_162107dcd0974532a3c81fc1eb5f1e96e27e4fa0() {
return (SQL_ATTR_QUIET_MODE);
}


int inline_c_Database_MsSQL_Internal_143_fd4b42be404e3c6351cda6250f4120c8237485d3() {
return (SQL_ATTR_TRACE);
}


int inline_c_Database_MsSQL_Internal_144_fd4b42be404e3c6351cda6250f4120c8237485d3() {
return (SQL_ATTR_TRACE);
}


int inline_c_Database_MsSQL_Internal_145_16227e086216d65a2296962c25144c9a1264cc7a() {
return (SQL_ATTR_TRACEFILE);
}


int inline_c_Database_MsSQL_Internal_146_16227e086216d65a2296962c25144c9a1264cc7a() {
return (SQL_ATTR_TRACEFILE);
}


int inline_c_Database_MsSQL_Internal_147_0aa87d25f59eb293533b75a15eef199e60d79dbc() {
return (SQL_ATTR_TRANSLATE_LIB);
}


int inline_c_Database_MsSQL_Internal_148_0aa87d25f59eb293533b75a15eef199e60d79dbc() {
return (SQL_ATTR_TRANSLATE_LIB);
}


int inline_c_Database_MsSQL_Internal_149_99be0c734a29e8056078f6a232d192c4a0ebb584() {
return (SQL_ATTR_TRANSLATE_OPTION);
}


int inline_c_Database_MsSQL_Internal_150_99be0c734a29e8056078f6a232d192c4a0ebb584() {
return (SQL_ATTR_TRANSLATE_OPTION);
}


int inline_c_Database_MsSQL_Internal_151_bcb14ad3ad804600afb17a0449e9c834ddab20f4() {
return (SQL_ATTR_TXN_ISOLATION);
}


int inline_c_Database_MsSQL_Internal_152_bcb14ad3ad804600afb17a0449e9c834ddab20f4() {
return (SQL_ATTR_TXN_ISOLATION);
}


int inline_c_Database_MsSQL_Internal_153_65a3946ba4158782014e25453f392c3e6acaa2c0() {
return (SQL_ATTR_METADATA_ID);
}


int inline_c_Database_MsSQL_Internal_154_65a3946ba4158782014e25453f392c3e6acaa2c0() {
return (SQL_ATTR_METADATA_ID);
}


int inline_c_Database_MsSQL_Internal_155_472a224329049c7048b4f789129003e8d50eda3b() {
return (SQL_ATTR_AUTO_IPD);
}


int inline_c_Database_MsSQL_Internal_156_472a224329049c7048b4f789129003e8d50eda3b() {
return (SQL_ATTR_AUTO_IPD);
}


int inline_c_Database_MsSQL_Internal_157_2614d1d4d2727010cb05ad4ff48e1f16e98e0628() {
return (SQL_ATTR_ASYNC_ENABLE);
}


int inline_c_Database_MsSQL_Internal_158_2614d1d4d2727010cb05ad4ff48e1f16e98e0628() {
return (SQL_ATTR_ASYNC_ENABLE);
}


int inline_c_Database_MsSQL_Internal_159_e93ca637515a6a8aab50edbad634b63b567a1209() {
return (SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE);
}


int inline_c_Database_MsSQL_Internal_160_e93ca637515a6a8aab50edbad634b63b567a1209() {
return (SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_161_bb4d922bddb1fb7d68295e24bd98daeeb3649c30() {
return (SQL_NO_NULLS);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_162_bb4d922bddb1fb7d68295e24bd98daeeb3649c30() {
return (SQL_NO_NULLS);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_163_4a3dff474468e55a8dc546e9b9a624bc573fcd16() {
return (SQL_NULLABLE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_164_4a3dff474468e55a8dc546e9b9a624bc573fcd16() {
return (SQL_NULLABLE);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_165_da9bbb330931a1da3ba2d39becaec6cd20a8d0c9() {
return (SQL_NULLABLE_UNKNOWN);
}


SQLSMALLINT inline_c_Database_MsSQL_Internal_166_da9bbb330931a1da3ba2d39becaec6cd20a8d0c9() {
return (SQL_NULLABLE_UNKNOWN);
}


int inline_c_Database_MsSQL_Internal_167_aab4defe88ab1871df0a578f1099241d560ea367(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLGUID * uuidP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_GUID, uuidP_inline_c_3, 16, lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_168_b4913350a67fc60f28bfd5e46b939eb1451ba5dd(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQL_SS_TIMESTAMPOFFSET_STRUCT * ltimeP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_BINARY, ltimeP_inline_c_3, sizeof(SQL_SS_TIMESTAMPOFFSET_STRUCT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_169_b0813d45470097d33b36fe9263ec9a884308b517(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQL_TIMESTAMP_STRUCT * ltimeP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_TYPE_TIMESTAMP, ltimeP_inline_c_3, sizeof(SQL_TIMESTAMP_STRUCT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_170_77601fc0b45740b7bdd76f8d960d7b4d6c8124e1(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQL_SS_TIME2_STRUCT * todP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_BINARY, todP_inline_c_3, sizeof(SQL_SS_TIME2_STRUCT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_171_4572c496162fe3459c2d2f5db1c7840fed73f399(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, unsigned long long * llongP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_UBIGINT, llongP_inline_c_3, sizeof(unsigned long long), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_172_539002be7be21ef3a64def89bf61162eaa8e8232(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, long long * llongP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_SBIGINT, llongP_inline_c_3, sizeof(long long), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_173_11bad61fa1d33925212ab162d62a3210e34a9b52(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQL_DATE_STRUCT * dateP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_TYPE_DATE, dateP_inline_c_3, sizeof(SQL_DATE_STRUCT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_174_d69a4639becda0785b87d2ca1a3b690da534a761(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLCHAR * chrP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_BIT, chrP_inline_c_3, sizeof(1), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_175_4c6c96cd373bfbff3155db6ac05521a4e999f9ca(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLDOUBLE * floatP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_DOUBLE, floatP_inline_c_3, sizeof(SQLDOUBLE), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_176_5f04beb7ea02607b59a0b898af34eb2d193e0914(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLREAL * floatP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_FLOAT, floatP_inline_c_3, sizeof(SQLREAL), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_177_e715d75a258eb11598a7b9e9c5ddc95362a41833(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLUSMALLINT * shortP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_USHORT, shortP_inline_c_3, sizeof(SQLUSMALLINT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_178_f3ac944e3ed5cf4985f80505b1d82557d407027f(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLSMALLINT * shortP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_SHORT, shortP_inline_c_3, sizeof(SQLSMALLINT), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_179_e44410293764a8f782b66edbf6dc1398345862ce(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLUINTEGER * intP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_ULONG, intP_inline_c_3, sizeof(SQLUINTEGER), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_180_1bdccc73289bf65d3e80b2586dbf058417ab70cf(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLINTEGER * intP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_LONG, intP_inline_c_3, sizeof(SQLINTEGER), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_181_e31613374950c55ec8170de225414790b5e9db31(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLCHAR * chrP_inline_c_3) {

                 SQLRETURN ret = 0;
                 SQLHSTMT hstmt = hstmtP_inline_c_0;
                 SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                 ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_TINYINT, chrP_inline_c_3, sizeof(SQLCHAR), lenOrInd);
                 return ret;
             
}


int inline_c_Database_MsSQL_Internal_182_04dd2d30fa81c1727c5fd1a61c1f42c7f318bbd7(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLCHAR * chrP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_UTINYINT, chrP_inline_c_3, sizeof(SQLCHAR), lenOrInd);
                return ret;
            
}


int inline_c_Database_MsSQL_Internal_183_29315c97bfa754637a642280a17d01357d1b7a70(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLWCHAR * txtP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_WCHAR, txtP_inline_c_3, 100, lenOrInd);
                return ret;
              
}


int inline_c_Database_MsSQL_Internal_184_29315c97bfa754637a642280a17d01357d1b7a70(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLUSMALLINT cpos_inline_c_2, SQLWCHAR * txtP_inline_c_3) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                ret = SQLBindCol(hstmt, cpos_inline_c_2, SQL_C_WCHAR, txtP_inline_c_3, 100, lenOrInd);
                return ret;
              
}


int inline_c_Database_MsSQL_Internal_185_e3ec95d7a16c506e8dfceec0fb33b53c5020db49(SQLHSTMT hstmtP_inline_c_0, SQLLEN * lenOrIndFP_inline_c_1, SQLLEN bufferLen_inline_c_2, SQLUSMALLINT cpos_inline_c_3, SQLCHAR * chrp_inline_c_4) {

                SQLRETURN ret = 0;
                SQLHSTMT hstmt = hstmtP_inline_c_0;
                SQLLEN* lenOrInd = lenOrIndFP_inline_c_1;
                SQLLEN bufferLen = bufferLen_inline_c_2;
                ret = SQLBindCol(hstmt, cpos_inline_c_3, SQL_C_CHAR, chrp_inline_c_4, bufferLen, lenOrInd);
                return ret;
              
}

