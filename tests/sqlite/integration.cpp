#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h> 


static int uniqueIDcallback(void *data, int argc, char **argv, char **azColName){
   printf("Noisy unique AnonIDs: %s\n", argv[0]);
   return 0;
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
   status = sqlite3_load_extension(db, "../../src/Database/DPDT/laplace", "sqlite3_laplace_init", &errMsg);
   
   const char* sql = "SELECT (COUNT(*) + samplePure(0, 1)) FROM (Select * From (AOL) Group By (AnonID))";

   rc = sqlite3_exec(db, sql, uniqueIDcallback, NULL, &zErrMsg);

   if( rc != SQLITE_OK ){
      fprintf(stderr, "SQL error: %s\n", zErrMsg);
      sqlite3_free(zErrMsg);
   }

   sqlite3_close(db);
   return 0; 
}
