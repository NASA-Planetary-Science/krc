/*
*_TITL  GTIME	get the system time, return in the form hh:mm:ss (24 hrs)
*_ARGS
*	char *stime	string that will contain trhe system time
*	long len	length of stime string
*_KEYS	system utility
*_DESC	returns the system time in stime, in the form hh:mm:ss
*_HIST	92JUN01	ECisneros USGS FLagstaff Original Version
*_END
*/
#include <stdio.h>
#include <time.h>

void gtime_(char *stime, long len)
{
	time_t ltime;
	struct tm *t_time;

	/* get time from system */
	time(&ltime);

	/* convert to locale time and format date string */
	t_time = localtime(&ltime);
	strftime(stime,len,"%H:%M:%S",t_time);

}
