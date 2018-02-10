/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/scanf.c
 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <fileptr.h>
#include <limits.h>

struct scan_format_specifier {
    int no_conversion;
    int width;
    char typemod;
    char conversion;
};

static void parse_scan_format_conversion
        (struct scan_format_specifier *fmtspec, char **pp) {
    char *p = *pp;
    memset(fmtspec, 0, sizeof(struct scan_format_specifier));
    
    /* looking for modifiers */
    
    for(; *p; p++) {
	if (*p == '*') {
	    fmtspec->no_conversion = 1;
	    p++;
	    break;
	} else
	    break;
    }
    
    /* looking for widths */
    if (*p >= '1' && *p <= '9') {
	fmtspec->width = strtol(p, &p, 10);
    } else {
	fmtspec->width = -1;
    }
    switch (*p) {
    case 'L': case 'q':
	fmtspec->typemod = *p++;
	break;
    case 'h': 
	fmtspec->typemod = *p++;
	if (*p == 'h') {
	    p++;
	    fmtspec->typemod = 'c';
	}
	break;
    case 'l':
	fmtspec->typemod = *p++;
	if (*p == 'l') {
	    p++;
	    fmtspec->typemod = 'q';
	}
	break;
    }
    
    switch (*p) {
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
    case 'e': case 'E': case 'f': case 'F': case 'g': case 'G':
    case 'c': case 's': case 'p': case 'n': case '[':
    case '%':
	fmtspec->conversion = *p++;
    }
    *pp = p;
    return;
}

struct scan_input {
    int p, o, f, n;
    int (*get)(struct scan_input *);
    void (*unget)(struct scan_input *, char);
};

static inline int get(struct scan_input *p) {
    return p->get(p);
}

static inline void unget(struct scan_input *p, int c) {
    return p->unget(p, (char)c);
}

static inline int eat_when_match(struct scan_input *p, int m) {
    int c = p->get(p);
    if (c == m)
	return 1;
    if (c != EOF)
	p->unget(p, (unsigned char)c);
    return 0;
}

static void eat_spaces(struct scan_input *in) {
    for (;;) {
	int c = get(in);
	if (c == EOF)
	    return;
	else if (isascii(c) && isspace(c)) {
	    continue;
	}
	else {
	    in->unget(in, c);
	    break;
	}
    }
}

static inline int parse_digit(int c) {
  if ('0' <= c && c <= '9')
    return c - '0';
  else if ('A' <= c && c <= 'Z')
    return c - 'A' + 10;
  else if ('a' <= c && c <= 'z')
    return c - 'a' + 10;
  else
    return INT_MAX;
}

int vkscanf_internal(struct scan_input *in, 
		     base_t format_b, ofs_t format_o,
		     base_t va_b, ofs_t va_o, char *libloc) {
    int count = 0;

    void *format_rp = NULL;
    char *format = wrapper_get_string_z(format_b, format_o, &format_rp, libloc);
    char *p = format; /* always null-terminated */

    while (*p) {
	char fc = *p;
	if (isascii(fc) && isspace(fc)) {
	    p++;
	    eat_spaces(in);
	    continue;
	} else if (fc != '%') {
	    int c = get(in);
	    p++;
	    if (c == EOF)
		goto end_of_input;
	    if ((unsigned char)fc != (unsigned char)c) {
		unget(in, c);
		goto match_failed;
	    }
	    continue;
	} else {
	    struct scan_format_specifier fmtspec;
	    p++;
	    parse_scan_format_conversion(&fmtspec, &p);
	    switch (fmtspec.conversion) {
	    case 'd': case 'i': case 'o': case 'u': case 'x':
		{
		    int is_signed = fmtspec.conversion == 'd' || fmtspec.conversion == 'i';
		    int radix = (fmtspec.conversion == 'x' ? 16 :
				 fmtspec.conversion == 'o' ? 8 :
				 fmtspec.conversion == 'i' ? 0 : 10);
		    int sign = 1;
		    long long val = 0;
		    unsigned int c, d, charcount = 0, acceptable = 0;
		    
		    eat_spaces(in);
		    c = get(in);
		    if (is_signed && c == '-') {
			charcount++;
			sign = -1;
		    } else if (is_signed && c == '+') {
			charcount++;
		    } else {
			unget(in, c);
		    }
		    /* 1st character */
		    if (fmtspec.width == -1 || charcount < fmtspec.width) {
			c = get(in);
			if (c == EOF) {
			    if (charcount)
				goto match_failed;
			    else
				goto end_of_input;
			} else if (radix == 0 && c == '0') {
			    charcount++;
			    radix = 8;
			    if ((fmtspec.width == -1 || charcount < fmtspec.width)
				&& (eat_when_match(in, 'x') || eat_when_match(in, 'X'))) {
				charcount++;
				radix = 16;
			    } else {
				acceptable = 1;
			    }
			} else {
			    if (radix == 0)
				radix = 10;
			    val = parse_digit(c);
			    if (val >= radix) {
				unget(in, c);
				goto match_failed;
			    }
			    acceptable = 1;
			    charcount++;
			}
		    }
		    /* second and more */
		    while (fmtspec.width == -1 || charcount < fmtspec.width) {
			c = get(in);
			if (c == EOF)
			    break;
			d = parse_digit(c);
			if (d >= radix) {
			    unget(in, c);
			    break;
			}
			val *= radix;
			val += d;
			acceptable = 1;
			charcount++;
		    }

		    if (!acceptable)
			goto match_failed;

		    val *= sign;
		    /* done. write to address. */
		    if (!fmtspec.no_conversion) {
			value p = read_word(va_b, va_o);
			base_t b = base_of_value(p);
			ofs_t o = ofs_of_value(p);
			va_o += sizeof(void *);
			
			switch (fmtspec.typemod) {
			case 'c':
			    write_byte(b, o, (char)val, NULL);
			    break;
			case 'h':
			    write_hword(b, o, (short)val, NULL);
			    break;
			case 'L': case 'q':
			    write_dword(b, o, dvalue_of_dword(val), NULL);
			    break;
			case 0: case 'l': default:
			    write_word(b, o, value_of_int((long)val), NULL);
			    break;
			}
			count++;
		    }
		    break;
		}
	    case 's': {
		int c = 0, no_output = 0, charcount = 0;
		value p = value_of_int (0);
		base_t b = 0;
		ofs_t o = 0;
		if (fmtspec.no_conversion) {
		    no_output = 1;
		} else {
		    p = read_word(va_b, va_o);
		    b = base_of_value(p);
		    o = ofs_of_value(p);
		    va_o += sizeof(void *);
		}
		eat_spaces(in);
		while (fmtspec.width == -1 || charcount < fmtspec.width) {
		    c = get(in);
		    if (c == EOF)
			break;
		    if (isascii(c) && isspace(c)) {
			unget(in, c);
			break;
		    }
		    if (!no_output)
			write_byte(b, o++, (byte)c, NULL);
		    charcount++;
		}
		if (charcount == 0 && c == EOF)
		    goto end_of_input;
		if (!no_output) {
		    write_byte(b, o++, (byte)0, NULL);
		    count++;
		}
		break;
	    }
	    case 'c': {
	        int width = (fmtspec.width == -1) ? 1 : fmtspec.width;
		base_t b;
		ofs_t o;
		int x;

		if (!fmtspec.no_conversion) {
		    value p = read_word(va_b, va_o);
		    b = base_of_value(p);
		    o = ofs_of_value(p);
		    va_o += sizeof(void *);
	        } else {
		    b = 0;
		    o = 0;
		}

		for (x = 0; x < width; x++) {
		    int c = get(in);
		    if (c == EOF)
			goto end_of_input;
		    if (!fmtspec.no_conversion)
			write_byte(b, o++, (byte)c, NULL);
		}

		if (!fmtspec.no_conversion) 
		    count++;
		break;
	    }
	    case '[': {
		unsigned char set[256];
	        int width = (fmtspec.width == -1) ? INT_MAX : fmtspec.width;
		base_t b;
		ofs_t o;
		int x;
		int inverted = 0;
		int start, c;

		if (!fmtspec.no_conversion) {
		    value p = read_word(va_b, va_o);
		    b = base_of_value(p);
		    o = ofs_of_value(p);
		    va_o += sizeof(void *);
	        } else {
		    b = 0;
		    o = 0;
		}
		if (*p == '^') {
		    inverted = 1;
		    p++;
		}
		memset(set, inverted, 256);
		if (*p == ']') {
		    set[']'] = !inverted;
		    p++;
		}
		for(;;) {
		    int fc = (unsigned char)*p++;
		    if (fc == 0)
			goto bad_format;
		    if (fc == ']')
			break;
		    start = fc;
		    set[fc] = !inverted;
		    if (*p == '-') {
			fc = (unsigned char)*(p + 1);
			if (fc == 0)
			    goto bad_format;
			if (fc == ']')
			    break; /* - will be read next time */
			if (fc < start)
			    goto bad_format;
			for (c = start; c <= fc; c++)
			    set[c] = !inverted;
			p += 2;
		    }
		}

		for (x = 0; x < width; x++) {
		    int c = get(in);
		    if (c == EOF) {
			if (x == 0)
			    goto end_of_input;
			else
			    break;
		    }
		    if (! set[c]) {
			unget(in, c);
			if (x == 0) {
			    goto match_failed;
			} else {
			    break;
			}
		    }
		    if (!fmtspec.no_conversion)
			write_byte(b, o++, (byte)c, NULL);
		}

		if (!fmtspec.no_conversion) {
		    write_byte(b, o++, (byte)0, NULL);
		    count++;
		}
		break;
	    }
	    case 'e': case 'E': case 'f': case 'F': case 'g': case 'G': {
#define DONE (-1)
#define NO (-2)
#define PREM (-3)
#define VALSET (-4)
		/* automaton for accepting floating strings */
		static const int automaton[9][6] = {
		    /* digits, sign,    E,  dot, other, EOF */
		    {       2,    1,   NO,    3,    NO, PREM }, /* 0: start */
		    {       2,   NO,   NO,    3,    NO,   NO }, /* 1: +     */
		    {       2, DONE,    5,    4,  DONE, DONE }, /* 2: 1     */
		    {       4,   NO,   NO,   NO,    NO,   NO }, /* 3: .     */
		    {       4, DONE,    5, DONE,  DONE, DONE }, /* 4: 1.2   */
		    {       7,    6,   NO,   NO,    NO,   NO }, /* 5: 1.2E  */
		    {       7,   NO,   NO,   NO,    NO,   NO }, /* 6: 1.2E+ */
		    {       7, DONE, DONE, DONE,  DONE, DONE }, /* 7: 1.2E+1 */
		    {    DONE, DONE, DONE, DONE,  DONE, DONE }, /* 8: NaN | Inf */
		};
		string_buffer buf = INIT_string_buffer;
		int state = 0, c = 0, charcount = 0;
		double sign = 1.0;
		double v = 0;

		eat_spaces(in);

		while (state >= 0 && (fmtspec.width == 1 || charcount >= fmtspec.width)) {
		    c = get(in);
		    if (c == EOF) {
			state = automaton[state][5];
			/* do not call unget */
			break;
		    }
		    /*fprintf(stderr, "in = %c, state = %d ->", c, state);*/
		    switch (c) {
		    case '0': case '1': case '2': case '3': case '4':
		    case '5': case '6': case '7': case '8': case '9':
			state = automaton[state][0];
			break;
		    case '+': case '-':
			if (c == '-') sign = -1.0;
			state = automaton[state][1];
			break;
		    case 'E': case 'e':
			state = automaton[state][2];
			break;
		    case '.':
			state = automaton[state][3];
			break;
		    case 'I': case 'i':
			/* start of inf */
			if (state == 0 || state == 1 /* Inf acceptable */) {
			    c = get(in);
			    if (c != 'n' && c != 'N') {
				state = NO;
				break;
			    }
			    c = get(in);
			    if (c != 'f' && c != 'F') {
				state = NO;
				break;
			    }
			    v = (1.0 / 0.0);
			    state = VALSET;
			    break;
			} else {
			    state = automaton[state][4];
			    break;
			}
		    case 'N': case 'n':
			/* start of nan */
			if (state == 0 || state == 1 /* Inf acceptable */) {
			    c = get(in);
			    if (c != 'a' && c != 'A') {
				state = NO;
				break;
			    }
			    c = get(in);
			    if (c != 'N' && c != 'n') {
				state = NO;
				break;
			    }
			    v = (0.0 / 0.0);
			    state = VALSET;
			    break;
			} else {
			    state = automaton[state][4];
			    break;
			}
		    default:
			state = automaton[state][4];
			break;
		    }
		    /*fprintf(stderr, "%d\n", state);*/
		    if (state == VALSET)
			break;
		    if (state < 0) {
			unget(in, c);
			break;
		    } else {
			put_string_buffer(&buf, c); charcount++;
		    }
		}
		if (state >= 0)
		    state = automaton[state][4];
		if (state == PREM) {
		    release_string_buffer(&buf);
		    goto end_of_input;
		} else if (state == NO) {
		    release_string_buffer(&buf);
		    goto match_failed;
		}
		if (state == DONE) {
		    v = strtod(get_sz_string_buffer(&buf), NULL);
		} else {
		    assert(state == VALSET);
		    v = sign * v;
		}
		release_string_buffer(&buf);
		if (!fmtspec.no_conversion) {
		    value p = read_word(va_b, va_o);
		    base_t b = base_of_value(p);
		    ofs_t o = ofs_of_value(p);
		    va_o += sizeof(void *);
		    
		    switch (fmtspec.typemod) {
		    case 'l': case 'L': case 'q':
			write_dword(b, o, dvalue_of_dword(dword_of_double(v)), NULL);
			break;
		    case 0: default:
			write_word(b, o, value_of_int(word_of_float((float) v)), NULL);
			break;
		    }
		    count++;
		}
		break;
#undef OK
#undef DONE
#undef PREM
#undef VALSET
	    }
	    case 'n':
		if (!fmtspec.no_conversion) {
		    value p = read_word(va_b, va_o);
		    base_t b = base_of_value(p);
		    ofs_t o = ofs_of_value(p);
		    va_o += sizeof(void *);
		    switch (fmtspec.typemod) {
		    case 'h':
			write_hword(b, o, (short)in->n, NULL);
			break;
		    case 'L': case 'q':
			write_dword(b, o, dvalue_of_dword(in->n), NULL);
                        break;
		    case 0: case 'l': default:
			write_word(b, o, value_of_int(in->n), NULL);
			break;
		    }
		}
		break;
	    case '\0':
 bad_format:
		fsc_raise_error_library(format_b, format_o + (p - format), ERR_INVALIDARGS, libloc);
		break;
	    default:
		fsc_raise_error_library(format_b, format_o + (p - format), ERR_UNIMPLEMENTED, libloc);
		break;
	    }
	}
    }
 match_failed:
    wrapper_release_tmpbuf(format_rp);
    return count;
 end_of_input:
    wrapper_release_tmpbuf(format_rp);
    return count ? count : EOF;
}

static int file_get(struct scan_input *s) {
    int c;
    if (s->f)
	return EOF;
    c = getc((FILE *)s->p);
    if (c == EOF)
	s->f = 1;
    else {
	s->n++;
	c = (unsigned char)c;
    }
    return c;
}

static void file_unget(struct scan_input *s, char c) {
    ungetc(c, (FILE *)s->p);
    s->n--;
}

/**
 * @fn int scanf(const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPcV_i_scanf(base_t b, ofs_t o, base_t vb, ofs_t vo) {
    struct scan_input s = { (int)stdin, 0, 0, 0, file_get, file_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "scanf");
    fsc_va_end_base_ofs(vb, vo);
    return value_of_int(r);
}

/**
 * @fn int vscanf(const char *format, va_list v)
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPi_i_vscanf(base_t b, ofs_t o, base_t vb, ofs_t vo) {
    struct scan_input s = { (int)stdin, 0, 0, 0, file_get, file_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "vscanf");
    return value_of_int(r);
}

/**
 * @fn int fscanf(FILE *fp, const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE_PcV_i_fscanf(base_t fb, ofs_t fo, 
				       base_t b, ofs_t o, 
				       base_t vb, ofs_t vo) {
    FILE *fp = get_FILE_pointer(fb, fo);
    struct scan_input s = { (int)fp, 0, 0, 0, file_get, file_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "fscanf");
    fsc_va_end_base_ofs(vb, vo);
    return value_of_int(r);
}

/**
 * @fn int vfscanf(FILE *fp, const char *format, va_list v)
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE_PcPi_i_vfscanf(base_t fb, ofs_t fo, 
                                         base_t b, ofs_t o, 
                                         base_t vb, ofs_t vo) {
    FILE *fp = get_FILE_pointer(fb, fo);
    struct scan_input s = { (int)fp, 0, 0, 0, file_get, file_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "vfscanf");
    return value_of_int(r);
}

static int mem_get(struct scan_input *s) {
    int c = (unsigned char)read_byte((base_t)s->p, (ofs_t)s->o);
    if (c == '\0')
	return EOF;
    s->o++;
    s->n++;
    return c;
}

static void mem_unget(struct scan_input *s, char c) {
    s->o--;
    s->n--;
}

/**
 * @fn int sscanf(const char *s, const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPcPcV_i_sscanf(base_t sb, ofs_t so, 
			 base_t b, ofs_t o, 
			 base_t vb, ofs_t vo) {
    struct scan_input s = { (int)sb, (int)so, 0, 0, mem_get, mem_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "sscanf");
    fsc_va_end_base_ofs(vb, vo);
    return value_of_int(r);
}

/**
 * @fn int vsscanf(const char *s, const char *format, va_list v)
 * @author Yutaka Oiwa.
 */
value FS_FPcPcPi_i_vsscanf(base_t sb, ofs_t so, 
                           base_t b, ofs_t o, 
                           base_t vb, ofs_t vo) {
    struct scan_input s = { (int)sb, (int)so, 0, 0, mem_get, mem_unget };
    int r = vkscanf_internal(&s, b, o, vb, vo, "vsscanf");
    return value_of_int(r);
}

