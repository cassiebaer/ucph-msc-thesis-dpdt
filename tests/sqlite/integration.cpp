#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h> 


static int uniqueIDcallback(void *data, int argc, char **argv, char **azColName){
   printf("Noisy unique AnonIDs: %s\n", argv[0]);
   return 0;
}

static int averageIDcallback(void *data, int argc, char **argv, char **azColName){
   printf("Noisy average AnonID: %s\n", argv[0]);
   return 0;
}

void checkForErrors(int rc, char *zErrMsg){
   if( rc != SQLITE_OK ){
      fprintf(stderr, "SQL error: %s\n", zErrMsg);
      sqlite3_free(zErrMsg);
   }
}

int main(int argc, char* argv[])
{
   sqlite3 *db;
   char *zErrMsg = 0;
   int rc;
   
   // Open database
   rc = sqlite3_open("AOL_sm.db", &db);

   if( rc ){
      fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
      exit(0);
   }

   // Load laplace extension
   int status = sqlite3_enable_load_extension(db, 1);
   char *errMsg = 0;
   const char* sql;

   status = sqlite3_load_extension(db, "../../src/Database/DPDT/laplace", "sqlite3_laplace_init", &errMsg);
   printf("%s", errMsg);
   sql = "SELECT (COUNT(*) + samplePure(0, 1)) FROM (Select * From (AOL) Group By (AnonID))";
   rc = sqlite3_exec(db, sql, uniqueIDcallback, NULL, &zErrMsg);
   checkForErrors(rc, zErrMsg);
   
   sql = "SELECT (CASE WHEN COUNT(*) == 0 THEN (random() / 9223372036854775808) ELSE noisyAverage(AVG(ClampedValues), 2) END) FROM (SELECT (CASE WHEN AnonID > 1 THEN 1 ELSE (CASE WHEN AnonID < -1 THEN -1 ELSE AnonID END) END) AS ClampedValues FROM (AOL))";
   rc = sqlite3_exec(db, sql, averageIDcallback, NULL, &zErrMsg);
   checkForErrors(rc, zErrMsg);

   sqlite3_close(db);
   return 0; 
}



