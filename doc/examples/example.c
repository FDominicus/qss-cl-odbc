/* qss-cl-odbc documentation, example (a): the ODBC API from C.
 *
 * Connects to a data source, creates a table, inserts two rows through a
 * prepared statement with parameters, reads them back, and shows the
 * diagnostics of a failing statement. Text goes through the Unicode (...W)
 * functions, as in qss-cl-odbc.
 *
 *   Linux/macOS:     cc example.c -lodbc -o example && ./example EXAMPLE
 *   Windows (MSYS2): gcc example.c -lodbc32 -o example.exe && ./example EXAMPLE
 *
 * Copyright (c) 2026 QSS / Friedrich Dominicus, BSD license (see license.txt)
 */
#if defined(_WIN32)
#include <windows.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sql.h>
#include <sqlext.h>

/* ASCII text -> SQLWCHAR (UTF-16); enough for this example */
static SQLWCHAR *w(const char *s)
{
    static SQLWCHAR buf[4][512];
    static int next = 0;
    SQLWCHAR *out = buf[next++ % 4];
    int i = 0;
    for (; s[i] && i < 511; i++) out[i] = (SQLWCHAR)(unsigned char)s[i];
    out[i] = 0;
    return out;
}

/* SQLWCHAR (UTF-16, ASCII part) -> char, for printing */
static const char *a(const SQLWCHAR *s)
{
    static char buf[4][1024];
    static int next = 0;
    char *out = buf[next++ % 4];
    int i = 0;
    for (; s[i] && i < 1023; i++) out[i] = s[i] < 128 ? (char)s[i] : '?';
    out[i] = 0;
    return out;
}

/* Every call is checked: on SQL_ERROR print all diagnostic records and stop. */
static void check(SQLRETURN rc, SQLSMALLINT type, SQLHANDLE handle, const char *what)
{
    if (rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO || rc == SQL_NO_DATA) return;
    SQLWCHAR state[6], message[1024];
    SQLINTEGER native;
    SQLSMALLINT length;
    for (SQLSMALLINT rec = 1;
         SQL_SUCCEEDED(SQLGetDiagRecW(type, handle, rec, state, &native, message, 1024, &length));
         rec++)
        fprintf(stderr, "%s: [%s] %s\n", what, a(state), a(message));
    exit(1);
}

int main(int argc, char **argv)
{
    const char *dsn = argc > 1 ? argv[1] : "EXAMPLE";
    char connection[256];
    SQLHENV env = SQL_NULL_HENV;
    SQLHDBC dbc = SQL_NULL_HDBC;
    SQLHSTMT stmt = SQL_NULL_HSTMT;

    /* environment and connection */
    check(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env), SQL_HANDLE_ENV, env, "env");
    check(SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0),
          SQL_HANDLE_ENV, env, "version");
    check(SQLAllocHandle(SQL_HANDLE_DBC, env, &dbc), SQL_HANDLE_ENV, env, "dbc");
    snprintf(connection, sizeof connection, "DSN=%s", dsn);
    check(SQLDriverConnectW(dbc, NULL, w(connection), SQL_NTS, NULL, 0, NULL, SQL_DRIVER_NOPROMPT),
          SQL_HANDLE_DBC, dbc, "connect");
    check(SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt), SQL_HANDLE_DBC, dbc, "stmt");

    /* a table */
    SQLExecDirectW(stmt, w("DROP TABLE IF EXISTS person"), SQL_NTS);
    check(SQLExecDirectW(stmt, w("CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(50))"), SQL_NTS),
          SQL_HANDLE_STMT, stmt, "create");

    /* prepared INSERT with two parameters, executed twice */
    SQLINTEGER id;
    SQLWCHAR name[51];
    SQLLEN id_ind = 0, name_ind = SQL_NTS;
    check(SQLPrepareW(stmt, w("INSERT INTO person (id, name) VALUES (?, ?)"), SQL_NTS),
          SQL_HANDLE_STMT, stmt, "prepare");
    check(SQLBindParameter(stmt, 1, SQL_PARAM_INPUT, SQL_C_SLONG, SQL_INTEGER, 0, 0, &id, 0, &id_ind),
          SQL_HANDLE_STMT, stmt, "bind 1");
    check(SQLBindParameter(stmt, 2, SQL_PARAM_INPUT, SQL_C_WCHAR, SQL_WVARCHAR, 50, 0, name,
                           sizeof name, &name_ind),
          SQL_HANDLE_STMT, stmt, "bind 2");
    const char *names[] = {"Ana", "Bo"};
    for (int i = 0; i < 2; i++) {
        id = i + 1;
        SQLWCHAR *n = w(names[i]);
        int k = 0;
        for (; n[k]; k++) name[k] = n[k];
        name[k] = 0;
        check(SQLExecute(stmt), SQL_HANDLE_STMT, stmt, "insert");
    }
    check(SQLFreeStmt(stmt, SQL_RESET_PARAMS), SQL_HANDLE_STMT, stmt, "reset");
    check(SQLEndTran(SQL_HANDLE_DBC, dbc, SQL_COMMIT), SQL_HANDLE_DBC, dbc, "commit");

    /* SELECT, read row by row with SQLGetData */
    check(SQLExecDirectW(stmt, w("SELECT id, name FROM person ORDER BY id"), SQL_NTS),
          SQL_HANDLE_STMT, stmt, "select");
    while (SQLFetch(stmt) == SQL_SUCCESS) {
        SQLINTEGER rid;
        SQLWCHAR rname[51];
        SQLLEN ind;
        check(SQLGetData(stmt, 1, SQL_C_SLONG, &rid, 0, &ind), SQL_HANDLE_STMT, stmt, "id");
        check(SQLGetData(stmt, 2, SQL_C_WCHAR, rname, sizeof rname, &ind), SQL_HANDLE_STMT, stmt, "name");
        printf("%d: %s\n", (int)rid, a(rname));
    }
    check(SQLFreeStmt(stmt, SQL_CLOSE), SQL_HANDLE_STMT, stmt, "close");

    /* a failing statement: read its diagnostics */
    if (SQLExecDirectW(stmt, w("SELECT * FROM no_such_table"), SQL_NTS) == SQL_ERROR) {
        SQLWCHAR state[6], message[1024];
        SQLINTEGER native;
        SQLSMALLINT length;
        SQLGetDiagRecW(SQL_HANDLE_STMT, stmt, 1, state, &native, message, 1024, &length);
        printf("error %s: %s\n", a(state), a(message));
    }

    /* clean up */
    SQLExecDirectW(stmt, w("DROP TABLE person"), SQL_NTS);
    SQLEndTran(SQL_HANDLE_DBC, dbc, SQL_COMMIT);
    SQLFreeHandle(SQL_HANDLE_STMT, stmt);
    SQLDisconnect(dbc);
    SQLFreeHandle(SQL_HANDLE_DBC, dbc);
    SQLFreeHandle(SQL_HANDLE_ENV, env);
    return 0;
}
