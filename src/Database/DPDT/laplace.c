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

static double cdf(double mean, double sd, double p){
	return 0.5 + 0.5 * signum(p - mean) * (1.0 - exp(-(fabs(p - mean) / sd)));
}

static double samplePureRnd(double mean, double sd, double randomVar){
        double rx = randomVar - 0.5;
        return mean - (sd * signum(rx) * log(1.0 - 2.0 * fabs(rx)));
}
static double noisyAveragePure(double trueAvg, double width){
	double upperBound = cdf(0.0, width, -1.0 - trueAvg);
	double lowerBound = cdf(0.0, width,  1.0 - trueAvg);
	srand(time(NULL));
	double rx = (double)rand() / ((double)(unsigned)RAND_MAX/2) - 1;
	double rxBounded = rx * (upperBound-lowerBound) + lowerBound;
	printf("upper: %f, lower: %f, rx: %f", upperBound, lowerBound, rx);
	return samplePureRnd(0.0, width, rxBounded);
}
static void noisyAverage(sqlite3_context *context, int argc, sqlite3_value **argv){
	double trueAvg = sqlite3_value_double(argv[0]);
	double width   = sqlite3_value_double(argv[1]);
	double upperBound = cdf(0.0, width, -1.0 - trueAvg);
	double lowerBound = cdf(0.0, width,  1.0 - trueAvg);
	srand(time(NULL));
	double rx = (double)rand() / ((double)(unsigned)RAND_MAX/2) - 1;
	double rxBounded = rx * (upperBound-lowerBound) + lowerBound;
	sqlite3_result_double(context, samplePureRnd(0.0, width, rxBounded));
}

int main(){
    noisyAveragePure(400, 2); 
}

static void samplePure(sqlite3_context *context, int argc, sqlite3_value **argv){
	double mean = sqlite3_value_double(argv[0]);
	double sd   = sqlite3_value_double(argv[1]);
	srand(time(NULL));
	double rx = (double)rand() / (double)((unsigned)RAND_MAX + 1.0);
	sqlite3_result_double(context, samplePureRnd(mean, sd, rx));
}

int sqlite3_laplace_init(sqlite3 *db, char **pzErrMsg, const sqlite3_api_routines *pApi){
  int rc = SQLITE_OK;
  SQLITE_EXTENSION_INIT2(pApi);
  rc = sqlite3_create_function(db, "samplePure", 2, SQLITE_UTF8, 0, samplePure, 0, 0); 
  if(rc == 0)
  	rc = sqlite3_create_function(db, "noisyAverage", 2, SQLITE_UTF8, 0, noisyAverage, 0, 0); 
  return rc;
}
