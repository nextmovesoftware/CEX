/*****************************************************************************
* cx_iostream.c -- low-level physical I/O functions
*----------------------------------------------------------------------------
*  Contributing author and institution: Roger Sayle, GlaxoWellcome R&D, UK.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#define CX_SOCKETS

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>


#ifdef CX_SOCKETS
#ifndef _WIN32
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include <unistd.h> 
#include <pwd.h>

#ifdef __sun
/* Prototype for "gethostbyname" */
extern struct hostent *gethostbyname( const char *name );
#endif

#else /* Windows NT */
#include <winsock.h>
#endif
#endif

#include "cx_cobweb.h"


/** IOSTREAM struct contains iostream-specific info (mainly methods). **/
 
typedef struct _ci_iostream {
   void       *iodata;
   cx_Object  datatypetable;
   void       (*close)   ( void *iodata );
   cx_Integer (*input)   ( void *iodata );
   void       (*output)  ( void *iodata,  cx_Integer ch );
   void       (*flush)   ( void *iodata );
   cx_Integer (*eof)     ( void *iodata );
} CI_IOSTREAM;
 

#ifdef CX_SOCKETS
#ifdef _WIN32
#define SockType          SOCKET
#define IsValidSocket(x)  ((x)!=INVALID_SOCKET)
#define InvalidSocket     INVALID_SOCKET
#define CloseSocket       closesocket
#else
#define SockType          int
#define IsValidSocket(x)  ((x)>=0)
#define InvalidSocket     -1
#define CloseSocket       close
#endif

#define CX_SOCKET_BUFFER_SIZE  1024

#define CX_HTTP_PROTOCOL       1
#define CX_FTP_PROTOCOL        2
#define CX_GOPHER_PROTOCOL     3
#define CX_CEX_PROTOCOL        4




typedef struct _ci_iosocket {
   unsigned char buffer[CX_SOCKET_BUFFER_SIZE];
   int           head, tail;
   SockType      sock;
   int           eos;
} CI_IOSOCKET;

typedef struct _ci_urlinfo {
   int   protocol;
   char  hostname[80];
   char  filename[1024];
   short port;
} CI_URLINFO;

static int initialized_sockets = FALSE;
#endif

/****************************************************************/
/*** Functions that work on (FILE *) are now local utilities. ***/
/****************************************************************/

static cx_Integer cx_e_read_input_file(FILE *fp)
{   
   return (fp ? getc(fp) : -1);
}

static void cx_e_write_output_file(FILE *fp, int ch)
{   
   if (fp) putc(ch, fp);
}

static int cx_e_end_of_input_file(FILE *fp)
{   
   return(fp ? feof(fp) : TRUE);
}

static void cx_e_flush_output_file(FILE *fp)
{   
   if (fp) fflush(fp);
}

static void cx_e_close_file(FILE *fp)
{   
   if (fp) fclose(fp);
}

/*============================================================================
 *  cx_e_fopen() -- like fopen(), but expands environment variables
 */

static FILE *cx_e_fopen(cx_String fn, cx_String perm)
{
   char path[CX_PATH_MAX], *p, *copy;

   /*** Deal with trivial case. ***/

   if (NULL == fn || NULL == perm) return NULL;

   /*** Deal with no-environment-variable case simply. ***/

   if (NULL == strchr(fn, '$')) return fopen(fn, perm);

   /*** Replace environment variable(s). ***/

   path[0] = '\0';
   for (p = copy = cx_strdup(fn); NULL != (p = strtok(p, "/")); p = NULL) {
      if (p != copy) strcat(path, "/");
      if ('$' != *p || NULL != (p = getenv(p + 1))) strcat(path, p);
   }
   cx_free(copy);

   return fopen(path, perm);
}


/** Generic IOSTREAM methods **/

/*============================================================================
 *  cx_e_iogetc() -- get character from input iostream
 */

cx_Integer cx_e_iogetc(cx_Object ins)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *) cx_e_base_content(ins);

   if ((bios == NULL) || (bios->input == NULL)) return(-1);
   return (bios->input(bios->iodata));
}

/*============================================================================
 *  cx_e_ioeof() -- is input iostream at EOF?
 */

cx_Integer cx_e_ioeof(cx_Object ins)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *) cx_e_base_content(ins);

   if ((bios == NULL) || (bios->eof == NULL)) return(TRUE);
   return (bios->eof(bios->iodata));
}

/*============================================================================
 *  cx_e_ioputc() -- write a character to an output iostream
 */

void cx_e_ioputc(int ch, cx_Object outs)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM*)cx_e_base_content(outs);

   if ((bios != NULL) && (bios->output != NULL))
      bios->output(bios->iodata, ch);
}

/*============================================================================
 *  cx_e_ioputs() -- write a zero-terminated string to an output iostream
 */

void cx_e_ioputs(char *ptr, cx_Object outs)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM*)cx_e_base_content(outs);

   if ((bios != NULL) && (bios->output != NULL))
      while (*ptr)
         bios->output(bios->iodata, *ptr++);
}

/*============================================================================
 *  cx_e_ioflush() -- flush an input or output iostream
 */

void cx_e_ioflush(cx_Object ios)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *) cx_e_base_content(ios);

   if ((bios != NULL) && (bios->flush != NULL))
      bios->flush(bios->iodata);
}

/*============================================================================
 *  cx_e_ioclose() -- close an input or output iostream
 */

void cx_e_ioclose(cx_Object ios)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *)cx_e_base_content(ios);

   if (bios != NULL) {
      if (bios->close != NULL) bios->close(bios->iodata);
      bios->iodata = NULL;
      bios->close  = NULL;
      bios->input  = NULL;
      bios->output = NULL;
      bios->flush  = NULL;
      bios->eof    = NULL;
      cx_free(bios);
   }
   cx_e_base_destroy(ios);
}
 

/*===============================*
 * CX_OB_IOSTREAM Object Package *
 *===============================*/

static int initialized = FALSE;

/*============================================================================
 *  cx_e_iostream_init()
 */

static void cx_e_iostream_init(void)
{
   if (!initialized) {
      cx_e_set_typename(CX_OB_IOSTREAM,"IOStream");

      cx_set_method(CX_OB_IOSTREAM,"cexin",       NULL);
      cx_set_method(CX_OB_IOSTREAM,"count",       NULL);
      cx_set_method(CX_OB_IOSTREAM,"destroy",
	 (cx_Method) cx_e_ioclose);
      cx_set_method(CX_OB_IOSTREAM,"reset",       NULL);
      cx_set_method(CX_OB_IOSTREAM,"send",        NULL);
      cx_set_method(CX_OB_IOSTREAM,"setprop",     
         (cx_Method) cx_e_base_setproperty);
      cx_set_method(CX_OB_IOSTREAM,"stream",      NULL);
      cx_set_method(CX_OB_IOSTREAM,"stringvalue", NULL);
      cx_set_method(CX_OB_IOSTREAM,"binaryvalue", NULL);

      initialized = TRUE;
   }
}

/*============================================================================
 *  cx_e_iostream_cleanup()
 */

void cx_e_iostream_cleanup(void)
{
#ifdef _WIN32
   if (sockets_initialized) WSACleanup();
#endif
}

/*============================================================================
 *  cx_e_iostream_sockets_init()
 */

#ifdef CX_SOCKETS
static void cx_e_iostream_sockets_init(void)
{
#ifdef _WIN32
    static WSADATA wsadata;
#endif

   if (!initialized_sockets) {
      if (!initialized) cx_e_iostream_init();

#ifdef _WIN32
      /* Request WinSock v1.1 */
      if( !WSAStartup(257,&wsadata) ) {
          if( 257 == wsadata.wVersion ) {
              initialized_sockets = TRUE;
          } else WSACleanup();
      }
#else
      initialized_sockets = TRUE;
#endif
   }
}

/*======================================*
 *  BSD-style Socket IOSTREAM Internals *
 *======================================*/

/*============================================================================
 *  cx_e_read_input_socket()
 */

static cx_Integer cx_e_read_input_socket(CI_IOSOCKET *ptr)
{
   register int stat;

   if (!ptr || ptr->eos) return(-1);

   if (ptr->head == ptr->tail) {
      stat = recv(ptr->sock, (void*)ptr->buffer, CX_SOCKET_BUFFER_SIZE, 0);
      if( stat <= 0 ) { ptr->eos = TRUE; return (-1); }
      ptr->tail = stat;
      ptr->head = 0;
   }

   return (ptr->buffer[ptr->head++]);
}


/*============================================================================
 *  cx_e_end_of_socket()
 */

static int cx_e_end_of_socket(CI_IOSOCKET *ptr)
{   
   return (ptr? ptr->eos : TRUE);
}


/*============================================================================
 *  cx_e_close_socket()
 */

static void cx_e_close_socket(CI_IOSOCKET *ptr)
{   
   register SockType sock;

   if (ptr) {
      sock = ptr->sock;
      ptr->eos = TRUE;
      cx_free(ptr);

      CloseSocket(sock);
   }
}

/*============================================================================
 *  cx_e_create_input_socket_iostream()
 */

static cx_Object cx_e_create_input_socket_iostream(SockType sock)
{
   CI_IOSOCKET *content;
   CI_IOSTREAM *ptr;
   cx_Object    result;

   cx_e_iostream_sockets_init();

   ptr = (CI_IOSTREAM *) cx_malloc(sizeof(CI_IOSTREAM));
   if( ptr == NULL ) return( NULL );

   content = (CI_IOSOCKET*)malloc(sizeof(CI_IOSOCKET));
   if( content == NULL )
   {   free(ptr);
       return NULL;
   }

   content->sock = sock;
   content->eos = FALSE;
   content->head = 0;
   content->tail = 0;

   ptr->iodata = (void *)content;
   ptr->datatypetable = NULL;

   ptr->close  = (void (*)(void*))       cx_e_close_socket;
   ptr->input  = (cx_Integer (*)(void*)) cx_e_read_input_socket;
   ptr->output = (void (*)(void*,int))   NULL;
   ptr->flush  = (void (*)(void*))       NULL;
   ptr->eof    = (cx_Integer (*)(void*)) cx_e_end_of_socket;

   result = cx_e_base_create(NULL, CX_OB_IOSTREAM);
   cx_e_base_set_content(result, (void *) ptr);
   return((cx_Object) result);
}


/*============================================================================
 *  is_prefix()
 */

static cx_Integer is_prefix(cx_String ptr, cx_String pref)
{
   while (*pref)
      if (tolower(*ptr) == *pref) { ptr++;  pref++; }
      else                        { return(FALSE);  }
   return (TRUE);
}

/*============================================================================
 *  is_valid_hostname_char()
 */

static cx_Integer is_valid_hostname_char( char ch )
{
    return( isalnum(ch) || ('.'==ch) || ('-'==ch) );
}

/*============================================================================
 *  parse_url_info()
 */

cx_Integer parse_url_info(cx_String fname, CI_URLINFO *url)
{
    register char *src, *dst;
    register int port;

    if( NULL == url )
        return FALSE;

   url->protocol = CX_HTTP_PROTOCOL;
   strcpy(url->hostname,"localhost");
   strcpy(url->filename,"/");
   url->port = 80;

   if( NULL == fname )
       return TRUE;

   src = fname;
   if( is_prefix(src,"http:") )
   {   url->protocol = CX_HTTP_PROTOCOL;
       src += 5;
   } else if( is_prefix(src,"ftp:") )
   {   url->protocol = CX_FTP_PROTOCOL;
       src += 4;
   } else if( is_prefix(src,"gopher:") )
   {   url->protocol = CX_GOPHER_PROTOCOL;
       src += 7;
   } else if( is_prefix(src,"cex:") )
   {   url->protocol = CX_CEX_PROTOCOL;
       src += 4;
   } else /* default */
   {    while( *src && is_valid_hostname_char(*src) ) 
            src++;
        if( (*src==':') && !isdigit(src[1]) )
            return FALSE;
        url->protocol = CX_HTTP_PROTOCOL;
        src = fname;
   }

    if( (src[0]=='/') && (src[1]=='/') )
    {   /* Explicit Hostname! */
        dst = url->hostname;  src += 2;
        while( *src && is_valid_hostname_char(*src) )
        {   *dst++ = tolower(*src);
            src++;
        }
        *dst = '\0';

        if( dst == url->hostname )
            return FALSE;

        if( *src == ':' )
        {   port = 0;  src++;
            while( isdigit(*src) )
                port = (10*port) + (*src++) - '0';
            if( !port ) return FALSE;
            url->port = port;
        }

        if( *src && (*src!='/') )
            return FALSE;
    } else
        return FALSE;

    if( *src ) strcpy(url->filename,src);
    return TRUE;
}


static SockType open_socket_connection( cx_String hostname, cx_Integer port )
{
    register struct sockaddr *ptr;
    static struct sockaddr_in server;
    static struct hostent *hp;
    register SockType sock;

    server.sin_family = AF_INET;
    if( !(hp=gethostbyname(hostname)) )
        return InvalidSocket;
    memcpy(&server.sin_addr, hp->h_addr, hp->h_length);
    server.sin_port = htons((short)port);

    sock = socket(AF_INET,SOCK_STREAM,0);
    if( !IsValidSocket(sock) )
        return InvalidSocket;
      
    ptr = (struct sockaddr*)&server;
    if( connect(sock, ptr, sizeof(server)) < 0 )
        return InvalidSocket;
    return( sock );
}


static cx_Integer send_http_request_packet( SockType sock, cx_String fname )
{
    static char buffer[2048];
    register char *ptr;
    register int stat;
    register int len;

    sprintf(buffer,"GET %s HTTP/1.0\nAccept: */*\n\n",fname);

    len = strlen(buffer);
    ptr = buffer;

    while( len > 0 )
    {   stat = send( sock, ptr, len, 0 );
        if( stat < 0 ) return FALSE;
        ptr += stat;
        len -= stat;
    }
    return TRUE;
}


static cx_Integer receive_http_reply_packet( cx_Object ins )
{
   register int flag;
   register int ch;

   flag = TRUE;
   while( (ch=cx_e_iogetc(ins)) != -1 )
   {   /* fputc(ch,stderr); */
       if( ch == '\n' )
       {   if( flag )
               return TRUE;
           flag = TRUE;
       } else if( ch != '\r' )
           flag = FALSE;
   }
   return FALSE;
}


/*============================================================================
 *  cx_e_open_input_socket_iostream()
 */

cx_Object cx_e_open_input_socket_iostream(cx_String fname)
{
   cx_Object result;
   SockType sock;
   CI_URLINFO url;

   cx_e_iostream_sockets_init();

   if( !parse_url_info(fname,&url) )
       return NULL;

   if( url.protocol != CX_HTTP_PROTOCOL )
       return NULL;

   sock = open_socket_connection(url.hostname,url.port);
   if( !IsValidSocket(sock) ) return NULL;

   if( !send_http_request_packet(sock,url.filename) )
   {   CloseSocket(sock);
       return NULL;
   }

   result = cx_e_create_input_socket_iostream(sock);

   if( !receive_http_reply_packet(result) )
   {   cx_destroy(result);
       return NULL;
   }
   return result;
}

#endif


/*============================================================================
 *  cx_e_create_input_file_iostream()
 */

cx_Object cx_e_create_input_file_iostream(FILE *fp)
{
   cx_Object result;
   CI_IOSTREAM *ptr;

   if (NULL == fp) return NULL;

   cx_e_iostream_init();

   ptr = (CI_IOSTREAM *) cx_malloc(sizeof(CI_IOSTREAM));
   if (ptr == NULL) return( NULL );

   ptr->iodata        = (void *) fp;
   ptr->datatypetable = NULL;

   ptr->close  = (void (*)(void *))       cx_e_close_file;
   ptr->input  = (cx_Integer (*)(void *)) cx_e_read_input_file;
   ptr->output = NULL;
   ptr->flush  = NULL;
   ptr->eof    = (cx_Integer (*)(void *)) cx_e_end_of_input_file;

   result = cx_e_base_create(NULL, CX_OB_IOSTREAM);
   cx_e_base_set_content(result, (void *) ptr);
   return( (cx_Object) result );
}

/*============================================================================
 *  cx_e_create_output_file_iostream()
 */

cx_Object cx_e_create_output_file_iostream(FILE *fp)
{
   cx_Object result;
   CI_IOSTREAM *ptr;

   if( NULL == fp ) return NULL;

   cx_e_iostream_init();

   ptr = (CI_IOSTREAM *) cx_malloc(sizeof(CI_IOSTREAM));
   if(ptr == NULL) return(NULL);

   ptr->iodata        = (void *) fp;
   ptr->datatypetable = NULL;

   ptr->close  = (void (*)(void*))     cx_e_close_file;
   ptr->input  = NULL;
   ptr->output = (void (*)(void*,int)) cx_e_write_output_file;
   ptr->flush  = (void (*)(void*))     cx_e_flush_output_file;
   ptr->eof    = NULL;

   result = cx_e_base_create(NULL, CX_OB_IOSTREAM);
   cx_e_base_set_content(result, (void *) ptr);
   return((cx_Object) result);
}

/*============================================================================
 *  cx_e_create_iostream() -- create iostream object from filename and access
 */

cx_Object cx_e_create_iostream(cx_String fname, cx_Integer access)
{
   FILE *fp = NULL;

   if (NULL == fname) return NULL;

#ifdef CX_SOCKETS
   if( is_prefix(fname,"http:") || 
       is_prefix(fname,"ftp:")  ||
       is_prefix(fname,"cex:") ) {
      if( access != CX_IO_READ ) return NULL;
      return cx_e_open_input_socket_iostream(fname);
   }
#endif

   if (CX_IO_READ == access) {
      fp = (0 == cx_strcmp(fname, "-") ? stdin  : cx_e_fopen(fname, "rb"));
      if (fp) return cx_e_create_input_file_iostream(fp);
   } else if (CX_IO_WRITE == access) {
      fp = (0 == cx_strcmp(fname, "-") ? stdout : cx_e_fopen(fname, "wb"));
      if (fp) return cx_e_create_output_file_iostream(fp);
   }

   return NULL;
}

/*======================================*
 *  CX_OB_IOSTREAM  sequence emulation  *
 *======================================*/

/*============================================================================
 *  cx_e_iostream_append()
 */

cx_Integer cx_e_iostream_append(cx_Object outs, cx_Object ob)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *) cx_e_base_content(outs);

   if (NULL == bios) return FALSE;

   cx_e_send(ob, bios->datatypetable, outs);
   return TRUE;
}

/*============================================================================
 *  cx_e_iostream_next()
 */

cx_Object cx_e_iostream_next(cx_Object ins)
{
   CI_IOSTREAM *bios = (CI_IOSTREAM *) cx_e_base_content(ins);

   if (NULL == bios) return NULL;
   return cx_e_receive(bios->datatypetable, ins, NULL);
}
