/*
*_TITL  GDATE	get the system date, return in the form Date-Month-Year
*_ARGS
*	char *sdate	string that will contain the system date
*	long LEN	length of sdate string
*_KEYS	system utility
*_DESC	returns the system date in sdate, in the form Date-Month-Year
*_HIST	92JUN01 ECisneros USGS Flagstaff Original Version
*_END
*/
#include <stdio.h>
#include <time.h>

void gdate_(char *sdate, long len)

{
	time_t ltime;
	struct tm *t_date;

	/* get time from system */
	time(&ltime);

	/* convert to locale time and format date string */
	t_date = localtime(&ltime);
	strftime(sdate,len,"%d-%b-%y",t_date);

}
