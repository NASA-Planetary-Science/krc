/*
*_TITL  UPPER converts a string to all upper-case characters
*_ARGS
*	char *ustring	string passed to upper, to be converted
*	long len	length of passed character string, not used
*_KEYS	character utility
*_DESC	Converts a given string to upper-case characters
*_HIST	92JUN02	ECisneros USGS Flagstaff Original Version
*_END
*/
#include <string.h>
#include <stdio.h>
#include <ctype.h>

void upper_(char *ustring, long len)
{
    char *p;

    /* Reverse case of each character. */
    for( p = ustring; *p; p++ )
    {
	if( islower( *p ) )
	    *p = toupper( *p );
    }
}

