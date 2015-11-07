#include <math.h>
#include <stdio.h>
#include <sqlite3ext.h>
#include <time.h>
#include <stdlib.h>

SQLITE_EXTENSION_INIT1

#ifdef _WIN32
__declspec(dllexport)
#endif

static double signum(double x){
	if (x < 0) return -1; 
	else return 1;
}

static void samplePure(sqlite3_context *context, int argc, sqlite3_value **argv){
	double mean = sqlite3_value_double(argv[0]);
	double sd   = sqlite3_value_double(argv[1]);
	srand(time(NULL));
	double x = (double)rand() / (double)((unsigned)RAND_MAX + 1) - 0.5;
	double sample =  mean - (sd * signum(x) * log(1.0 - 2.0 * fabs(x)));
	sqlite3_result_double(context, sample);
}

int sqlite3_laplace_init(sqlite3 *db, char **pzErrMsg, const sqlite3_api_routines *pApi){
  int rc = SQLITE_OK;
  SQLITE_EXTENSION_INIT2(pApi);
  rc = sqlite3_create_function(db, "samplePure", 2, SQLITE_UTF8, 0, samplePure, 0, 0); 
  return rc;
}
