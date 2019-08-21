#ifndef IDL_H
#define IDL_H

/***********************************************************
  Define the interface to an IDL string structure
  Note that IDL gives the length of the string MINUS the
  terminating NULL character.  This is critical when passing
  true string sizes into ISIS routines.  ISIS routines want
  the size of the string INCLUDING the NULL terminator.
************************************************************/
typedef struct {		/* Define string descriptor */
  unsigned short slen;		/* Length of string, 0 for null */
  short stype;			/* type of string, static or dynamic */
  char *s;			/* Addr of string */
} IDL_STRING;

/* The next added by HHK to address need for Fortran COMPLEX arguments */
typedef struct {
  float r,i;
} IDL_COMPLEX;

#define IDL_STRLEN(idl_string)    ((idl_string)->slen)
#define IDL_STRTYP(idl_string)    ((idl_string)->stype)
#define IDL_STRBUF(idl_string)    ((idl_string)->s)

#define IDL_INT                   long
#define IDL_FLOAT                 float
#define IDL_DOUBLE                double

#define IDL_RETURN                IDL_INT

#endif
