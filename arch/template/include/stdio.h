/**
 * @file include/stdio.h
 */
#ifndef _STDIO_H
#define _STDIO_H

#include <sys/__types.h>
#include <stdio_internal.h>

struct __fsc_attribute__((named "stdio_FILE", external)) FILE;
typedef struct FILE FILE;

#define EOF (-1)

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __PTRDIFF_T
#define __PTRDIFF_T
typedef __ptrdiff_t ptrdiff_t;
#endif

#ifndef __OFF_T
#define __OFF_T
typedef __off_t off_t;
#endif

#ifndef __VA_LIST
#define __VA_LIST
typedef __va_list va_list;
#endif

#ifndef NULL
#define NULL 0
#endif

extern FILE *stdin, *stdout, *stderr;

extern int puts(const char *);
extern int fputs(const char *, FILE *);
extern int fputc(int, FILE *);
extern int putchar(int);
#define putc(c,f) (fputc((c),(f)))

extern int fgetc(FILE *);
extern char *fgets(char *, int, FILE *);
extern char *gets(char *);
extern int getchar(void);
#define getc(f) (fgetc(f))
extern int ungetc(int, FILE *);

extern int printf(const char *, ...);
extern int fprintf(FILE *, const char *, ...);
extern int sprintf(char *, const char *, ...);
extern int snprintf(char *, size_t, const char *, ...);

extern int vprintf(const char *, va_list);
extern int vfprintf(FILE *, const char *, va_list);
extern int vsprintf(char *, const char *, va_list);
extern int vsnprintf(char *, size_t, const char *, va_list);

extern int ferror(FILE *);
extern void clearerr(FILE *);

extern int scanf(const char *format, ...);
extern int fscanf(FILE *, const char *format, ...);
extern int sscanf(const char *, const char *format, ...);

extern int vscanf(const char *format, va_list);
extern int vfscanf(FILE *, const char *format, va_list);
extern int vsscanf(const char *, const char *format, va_list);

extern void perror(const char *);

extern FILE *fopen(const char *, const char *);
extern FILE *freopen(const char *, const char *, FILE *);
extern int fclose(FILE *);
extern void rewind(FILE *);
extern int fseek (FILE *stream, long offset, int whence);
extern long ftell (FILE *stream);
extern size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
extern FILE *popen(const char *, const char *);
extern int pclose(FILE *);

extern FILE *fdopen(int, const char *);
extern int feof(FILE *);
extern int ferror(FILE *);
extern int fflush(FILE *);
extern int fileno(FILE *);

extern int fseeko(FILE *, off_t, int);
extern off_t ftello(FILE *);

extern int fgetpos(FILE *, fpos_t *);
extern int fsetpos(FILE *, const fpos_t *);

extern void setbuf(FILE *, char *);
extern int setvbuf(FILE *, char *, int, size_t);
extern int rename(const char *, const char *);
extern int remove(const char *);

extern char *ctermid(char *);
extern FILE *tmpfile(void);
extern char *tmpnam(char *);
extern char *tempnam(const char *, const char *);

#endif
