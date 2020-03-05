#ifndef SQL_C_SS_TIME2
#define SQL_C_SS_TIME2	(0x4000)
#endif

#ifndef SQL_C_SS_TIMESTAMPOFFSET
#define SQL_C_SS_TIMESTAMPOFFSET	(0x4001)
#endif

#define SQL_SS_TIME2                        (-154)
#define SQL_SS_TIMESTAMPOFFSET              (-155)

typedef struct tagSS_TIME2_STRUCT
{
    SQLUSMALLINT   hour;
    SQLUSMALLINT   minute;
    SQLUSMALLINT   second;
    SQLUINTEGER    fraction;
} SQL_SS_TIME2_STRUCT;

typedef struct tagSS_TIMESTAMPOFFSET_STRUCT
{
    SQLSMALLINT    year;
    SQLUSMALLINT   month;
    SQLUSMALLINT   day;
    SQLUSMALLINT   hour;
    SQLUSMALLINT   minute;
    SQLUSMALLINT   second;
    SQLUINTEGER    fraction;
    SQLSMALLINT    timezone_hour;
    SQLSMALLINT    timezone_minute;
} SQL_SS_TIMESTAMPOFFSET_STRUCT;
