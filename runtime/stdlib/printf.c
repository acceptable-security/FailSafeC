/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/* printf for Fail-Safe C */
/**
 * @file stdlib/printf.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <fileptr.h>

struct format_specifier {
    int width;
    int precision;
    int sharp;
    int zero;
    int minus;
    int space;
    int plus;
    char typemod;
    char conversion;
};

static int get_int_from_format(char **p) {
    return strtol(*p, p, 10);
}

static void parse_format_conversion
        (struct format_specifier *fmtspec,
	 char **pp, base_t va_b, ofs_t *va_o) {
    char *p = *pp;
    memset(fmtspec, 0, sizeof(struct format_specifier));
    fmtspec->width = fmtspec->precision = -1;
    
    /* looking for modifiers */
    
    for(; *p; p++) {
	if (*p == '+') {
	    fmtspec->plus = 1;
	} else if (*p == '-') {
	    fmtspec->minus = 1;
	} else if (*p == ' ') {
	    fmtspec->space = 1;
	} else if (*p == '#') {
	    fmtspec->sharp = 1;
	} else if (*p == '0') {
	    fmtspec->zero = 1;
	} else
	    break;
    }
    
    /* looking for widths */
    if (*p == '*') {
	fmtspec->width = vaddr_of_value(read_word(va_b, *va_o));
	*va_o += sizeof(int);
	p++;
	if (fmtspec->width < 0) {
	    fmtspec->minus = 1;
	    fmtspec->width *= -1;
	}
    } else if (*p >= '1' && *p <= '9') {
	fmtspec->width = get_int_from_format(&p);
    }
    if (*p == '.') {
	p++;
	if (*p == '*') {
	    fmtspec->precision = vaddr_of_value(read_word(va_b, *va_o));
	    *va_o += sizeof(int);
	    p++;
	} else {
	    fmtspec->precision = get_int_from_format(&p);
	}
	if (fmtspec->precision < 0)
	    fmtspec->precision = 0;
    }
    
    /* looking for width modifiers */
    switch (*p) {
    case 'h': case 'L': case 'q':
	fmtspec->typemod = *p++;
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
    case 'c': case 's': case 'p': case 'n':
    case '%':
    case 'm':
	fmtspec->conversion = *p++;
    }
    *pp = p;
    return;
}

typedef int (*outfunction)(void *of, char *p, int len);

static int output_repeat(outfunction output, void *of, char c, int count) {
    int i;
    for (i = 0; i < count; i++)
	if (output(of, &c, 1) != 1) break;
    return i;
}

static int vkprintf_internal(outfunction output, void *of,
		      base_t format_b, ofs_t format_o,
		      base_t va_b, ofs_t va_o, char *libloc) {
    int count = 0, ct;

    void *format_rp = NULL;
    char *format = wrapper_get_string_z(format_b, format_o, &format_rp, libloc);
    char *p = format; /* always null-terminated */
    
    for(;;) {
	struct format_specifier fmtspec;
	char *pp = p;
	while (*p != '\0' && *p != '%')
	    p++;
	ct = output(of, pp, p - pp);
	count += ct;
	if (ct != p - pp) break;
	if (*p == 0) break;

	assert (*p == '%');
	p++;

	parse_format_conversion(&fmtspec, &p, va_b, &va_o);
	
	switch (fmtspec.conversion) {
	case 0:
	    fsc_raise_error_library(format_b, format_o + (p - format), ERR_INVALIDARGS, libloc);
	case '%':
	    if (output(of, "%", 1) != 1) goto output_over;
	    count++;
	    break;
	case 'm':
	    {
		/* TODO: disable 'm' conversion on non-syslog use. */
		/* TODO: use saved errno on top of syslog */
		char *s = strerror(errno);
		int len = strlen(s);
		output(of, s, len);
	    }
	    break;
	case 's': 
	    {
		int len;
		char *s;
		void *s_rp = NULL;
		value p = read_word(va_b, va_o);
		va_o += sizeof(char *);

		/*fprintf(stderr, "%%%d.%ds arg : %x + %x\n", fmtspec.width, fmtspec.precision,
		  base_of_value(p), ofs_of_value(p));*/

		if (fmtspec.precision == -1)
		    s = wrapper_get_string_z(base_of_value(p), ofs_of_value(p), &s_rp,
					     libloc);
		else
		    s = wrapper_get_string_zn(base_of_value(p), ofs_of_value(p), &s_rp, 
					      fmtspec.precision, libloc);
		len = strlen(s);
		if (fmtspec.width == -1) {
		    ct = output(of, s, len);
		    count += ct;
		    if (ct != len) {
			wrapper_release_tmpbuf(s_rp);
			goto output_over;
		    }
		} else {
		    int spcs = fmtspec.width - len;
		    if (fmtspec.minus) {
			ct = output(of, s, len);
			count += ct;
			if (ct != len) {
			    wrapper_release_tmpbuf(s_rp);
			    goto output_over;
			}
		    }
		    if (spcs < 0)
			spcs = 0;
		    ct = output_repeat(output, of, ' ', spcs);
		    count += ct;
		    if (ct != spcs) {
			wrapper_release_tmpbuf(s_rp);
			goto output_over;
		    }
		    if (!fmtspec.minus) {
			ct = output(of, s, len);
			count += ct;
			if (ct != len) {
			    wrapper_release_tmpbuf(s_rp);
			    goto output_over;
			}
		    }
		}
		wrapper_release_tmpbuf(s_rp);
	    }
	    break;
	case 'p':
	    {
		/* "0x0123456789012345#+18446744073709551615" */
		char buf[50];
		value p;
		base_t b;
		ofs_t o;
		char *c;

		p = read_word(va_b, va_o);
		va_o += sizeof(void *);

		b = base_of_value(p);
		o = ofs_of_value(p);
		c = is_cast(b) ? "^" : "";
		b = base_remove_castflag(b);

		if (b)
		    sprintf(buf, "%#x%s%+i", b, c, o);
		else
		    sprintf(buf, "NULL%+i", o);
		
		ct = output(of, buf, strlen(buf));
		count += ct;
		if (ct != strlen(buf))
		    goto output_over;
	    }
	    break;
	case 'i': case 'd': case 'u': case 'x': case 'X': case 'o':
	    {
		char f = fmtspec.conversion;
		char buf[30]; /* 64bit int == 22 octal digits */
		char buf2[10];
		char *body;
		int len, prefix_len;
		int minus = 0;
		int zeros, spcs;
		char *prefix = "";
		unsigned long long v;
		if (f == 'i')
		    f = 'd';
		if (fmtspec.typemod == 'q') {
		    v = vaddr_of_dvalue(read_dword(va_b, va_o));
		    va_o += sizeof(long long);
		} else {
		    unsigned int vv = vaddr_of_value(read_word(va_b, va_o));
		    va_o += sizeof(int);
		    if (f == 'd')
			v = (unsigned long long)(long long)(int)vv;
		    else
			v = (unsigned long long)(unsigned int)vv;
		}
		sprintf(buf2, "%%ll%c", f);
		len = sprintf(buf, buf2, v);
		body = buf;
		if (buf[0] == '-') {
		    minus = 1;
		    body++;
		    len--;
		}
		/* %#x and %#o conversions do not need prefixes for value 0 (bug #lepidum-85) */
		if ((f == 'x' || f == 'X') && fmtspec.sharp && v != 0) {
		    prefix = (f == 'X' ? "0X" : "0x");
		} else if (f == 'o' && fmtspec.sharp && v != 0) {
		    prefix = "0";
		} else if (minus) {
		    prefix = "-";
		} else if (f == 'd' && fmtspec.plus) {
		    prefix = "+";
		} else if (f == 'd' && fmtspec.space) {
		    prefix = " ";
		}
		prefix_len = strlen(prefix);
		if (fmtspec.precision == -1)
		    zeros = 0;
		else
		    zeros = fmtspec.precision - len;
		if (zeros < 0)
		    zeros = 0;
		if (fmtspec.width == -1)
		    spcs = 0;
		else
		    spcs = fmtspec.width - len - strlen(prefix) - zeros;
		if (spcs < 0)
		    spcs = 0;
		if (spcs && !fmtspec.minus && (!fmtspec.zero || fmtspec.precision != -1)) {
		    count += ct = output_repeat(output, of, ' ', spcs);
		    if (ct != spcs)
			goto output_over;
		}
		if (prefix_len) {
		    count += ct = output(of, prefix, prefix_len);
		    if (ct != prefix_len) goto output_over;
		}
		if (spcs && !fmtspec.minus && (fmtspec.zero && fmtspec.precision == -1)) {
		    count += ct = output_repeat(output, of, '0', spcs);
		    if (ct != spcs)
			goto output_over;
		}
		if (zeros) {
		    count += ct = output_repeat(output, of, '0', zeros);
		    if (ct != zeros)
			goto output_over;
		}
		count += ct = output(of, body, len);
		if (ct != len) goto output_over;
		if (spcs && fmtspec.minus) {
		    count += ct = output_repeat(output, of, ' ', spcs);
		    if (ct != spcs)
			goto output_over;
		}
		break;
	    }
	case 'f': case 'F': case 'e': case 'E': case 'g': case 'G':
	    {
#define MAX_FLOAT_DIGITS 350
		char f = fmtspec.conversion;
		char fmtbuf[60]; /* 64bit int == 22 octal digits, %*.*f */
		char tmpbuf[MAX_FLOAT_DIGITS + 40]; /* IEEE 64bit float = 308digits max */
		char flagsbuf[4] = "\0\0\0";
		char *flags = flagsbuf;
		char *buf4;
		double v;
		int maxwidth, len, ct;
		v = double_of_dword(vaddr_of_dvalue(read_dword(va_b, va_o)));
		va_o += sizeof(double);
		if (fmtspec.precision == -1)
		    fmtspec.precision = 6;
		if (fmtspec.sharp)
		    *flags++ = '#';
		if (fmtspec.minus)
		    *flags++ = '-'; else
			if (fmtspec.zero)
			    *flags++ = '0';
		if (fmtspec.plus)
		    *flags++ = '+'; else
			if (fmtspec.space)
			    *flags++ = ' ';
		if (fmtspec.width == -1) {
		    sprintf(fmtbuf, "%%%s.%d%c", flagsbuf, fmtspec.precision, f);
		} else {
		    sprintf(fmtbuf, "%%%s%d.%d%c", flagsbuf, fmtspec.width, fmtspec.precision, f);
		}
		maxwidth = 
		    (fmtspec.width > fmtspec.precision ? fmtspec.width : fmtspec.precision)
		    + MAX_FLOAT_DIGITS;
		if (maxwidth < sizeof tmpbuf - 1)
		    buf4 = tmpbuf;
		else
		    buf4 = fsc_alloc_raw(maxwidth + 1);
		len = sprintf (buf4, fmtbuf, v);
		assert(len <= maxwidth);
		count += ct = output(of, buf4, len);
		if (buf4 != tmpbuf)
		    fsc_release_raw(buf4);
		if (ct != len)
		    goto output_over;
		break;
	    }
	case 'c': 
	    {
		int spcs;
		char c = (char)vaddr_of_value(read_word(va_b, va_o));
		va_o += sizeof(int);
		if (fmtspec.width == -1)
		    spcs = 0;
		else
		    spcs = fmtspec.width - 1;
		if (spcs < 0)
		    spcs = 0;
		if (spcs && !fmtspec.minus) {
		    count += ct = output_repeat(output, of, ' ', spcs);
		    if (ct != spcs)
			goto output_over;
		}
		count += ct = output(of, &c, 1);
		if (ct != 1) goto output_over;
		if (spcs && fmtspec.minus) {
		    count += ct = output_repeat(output, of, ' ', spcs);
		    if (ct != spcs)
			goto output_over;
		}
		break;
	    }
	case 'n':
	    {
		value p = read_word(va_b, va_o);
		base_t b = base_of_value(p);
		ofs_t o = ofs_of_value(p);
		va_o += sizeof(void *);

		switch (fmtspec.typemod) {
		case 'h':
		    write_hword(b, o, count, NULL);
		    break;
		case 'L': case 'q':
		    write_dword(b, o, dvalue_of_dword(count), NULL);
		    break;
		case 0: case 'l': default:
		    write_word(b, o, value_of_int(count), NULL);
		    break;
		}
		break;
	    }
	default:
	    fsc_raise_error_library(format_b, format_o, ERR_UNIMPLEMENTED, libloc);
	}
    }
 output_over:
    wrapper_release_tmpbuf(format_rp);
    return count;
}

static int file_output(void *fp, char *str, int len) {
    return fwrite(str, 1, len, (FILE *)fp);
}

/**
 * @fn int printf(const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPcV_i_printf(base_t b, ofs_t o, base_t vb, ofs_t vo) {
    int r = vkprintf_internal(file_output, stdout, b, o, vb, vo, "printf");
    fsc_va_end_base_ofs(vb, vo);
    return value_of_int(r);
}

/**
 * @fn int vprintf(const char *format, va_list v)
 *
 * @author Yutaka Oiwa.
 */
value FS_FPcPi_i_vprintf(base_t b, ofs_t o, base_t vb, ofs_t vo) {
    int r = vkprintf_internal(file_output, stdout, b, o, vb, vo, "printf");
    return value_of_int(r);
}

/**
 * @fn int fprintf(FILE *fp, const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE_PcV_i_fprintf(base_t fpb, ofs_t fpo, base_t b, ofs_t o, base_t vb, ofs_t vo) {
    FILE *fp = get_FILE_pointer(fpb, fpo);
    int r = vkprintf_internal(file_output, fp, b, o, vb, vo, "fprintf");
    fsc_va_end_base_ofs(vb, vo);
    return value_of_int(r);
}

/**
 * @fn int vfprintf(FILE *fp, const char *format, va_list v)
 *
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE_PcPi_i_vfprintf(base_t fpb, ofs_t fpo, base_t b, ofs_t o, base_t vb, ofs_t vo) {
    FILE *fp = get_FILE_pointer(fpb, fpo);
    int r = vkprintf_internal(file_output, fp, b, o, vb, vo, "fprintf");
    return value_of_int(r);
}

struct raw_buffer_info {
    base_t base;
    ofs_t ofs;
    char *start_p;
    char *p;
    ofs_t rest;
    int raise_error_p;
};

static int raw_string_output(void *ipv, char *str, int len_orig) {
    struct raw_buffer_info *ip = ipv;
    int len = len_orig;
    if (ip->rest < len) {
	if (ip->raise_error_p)
	    fsc_raise_error_library(ip->base, ip->ofs + (ip->p - ip->start_p + ip->rest), 
				    ERR_OUTOFBOUNDS, "sprintf");
	else
	    len = ip->rest; /* truncation by snprintf */
    }
    ip->rest -= len;
    for(; len; len--) {
	*ip->p++ = *str++;
    }
    return len_orig;
}

struct cooked_buffer_info {
    base_t base;
    ofs_t ofs;
    ofs_t rest;
    int raise_error_p;
};

static int cooked_string_output(void *ipv, char *str, int len_orig) {
    struct cooked_buffer_info *ip = ipv;
    int len = len_orig;
    if (ip->rest < len) {
	if (ip->raise_error_p)
	    fsc_raise_error_library(ip->base, ip->ofs + ip->rest, 
				    ERR_OUTOFBOUNDS, "sprintf");
	else
	    len = ip->rest; /* truncation by snprintf */
    }
    ip->rest -= len;
    for(; len; len--) {
	write_byte(ip->base, ip->ofs++, *str++, &fsc_typeinfo_c.val);
    }
    return len_orig;
}

static int null_string_output(void *ipv, char *str, int len_orig) {
    return len_orig;
}

/**
 * @fn int sprintf(char *str, const char *format, ...)
 * @author Yutaka Oiwa.
 */
value FS_FPcPcV_i_sprintf(base_t strb, ofs_t stro, base_t b, ofs_t o, base_t vb, ofs_t vo) {
    base_t base = base_remove_castflag(strb);
    fsc_header *hdr = get_header_fast(base);
    
    dealloc_check_fast(base, stro);
    if (hdr->total_ofslimit < stro) {
	/* out of bound pointer passed */
	fsc_raise_error_library(strb, stro, ERR_OUTOFBOUNDS, "sprintf");
    }

    if (hdr->tinfo->kind & TI_CONTINUOUS) {
	int r;
	struct raw_buffer_info bi;
	bi.base = base;
	bi.ofs = stro;
	bi.start_p = bi.p = ((char *)base) + stro;
	bi.rest = hdr->total_ofslimit - stro;
	bi.raise_error_p = 1;
	r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "sprintf");
	raw_string_output(&bi, "\0", 1);
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    } else {
	int r;
	struct cooked_buffer_info ci;
	ci.base = base; ci.ofs = stro;
	ci.rest = hdr->total_ofslimit - stro;
	ci.raise_error_p = 1;
	r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "sprintf");
	cooked_string_output(&ci, "\0", 1);
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    }
}

/**
 * @fn int vsprintf(char *str, const char *format, va_list)
 * @author Yutaka Oiwa.
 */
value FS_FPcPcPi_i_vsprintf(base_t strb, ofs_t stro, base_t b, ofs_t o, base_t vb, ofs_t vo) {
    base_t base = base_remove_castflag(strb);
    fsc_header *hdr = get_header_fast(base);
    
    dealloc_check_fast(base, stro);
    if (hdr->total_ofslimit < stro) {
	/* out of bound pointer passed */
	fsc_raise_error_library(strb, stro, ERR_OUTOFBOUNDS, "vsprintf");
    }

    if (hdr->tinfo->kind & TI_CONTINUOUS) {
	int r;
	struct raw_buffer_info bi;
	bi.base = base;
	bi.ofs = stro;
	bi.start_p = bi.p = ((char *)base) + stro;
	bi.rest = hdr->total_ofslimit - stro;
	bi.raise_error_p = 1;
	r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "vsprintf");
	raw_string_output(&bi, "\0", 1);
	return value_of_int(r);
    } else {
	int r;
	struct cooked_buffer_info ci;
	ci.base = base; ci.ofs = stro;
	ci.rest = hdr->total_ofslimit - stro;
	ci.raise_error_p = 1;
	r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "vsprintf");
	cooked_string_output(&ci, "\0", 1);
	return value_of_int(r);
    }
}

/**
 * @fn int snprintf(char *str, size_t n, const char *format, ...)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPciPcV_i_snprintf(base_t strb, ofs_t stro,
                            base_t nb, unsigned int no,
                            base_t b, ofs_t o,
                            base_t vb, ofs_t vo) {
    base_t base = base_remove_castflag(strb);
    fsc_header *hdr;

    if (no == 0) {
	int r;
	r = vkprintf_internal(null_string_output, NULL, b, o, vb, vo, "snprintf");
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    }

    hdr = get_header_fast(base);
    dealloc_check_fast(base, stro);
    if (hdr->total_ofslimit < stro) {
	/* out of bound pointer passed */
	fsc_raise_error_library(strb, stro, ERR_OUTOFBOUNDS, "snprintf");
    }

    if (hdr->tinfo->kind & TI_CONTINUOUS) {
	int r;
	struct raw_buffer_info bi;
	bi.base = base;
	bi.ofs = stro;
	bi.start_p = bi.p = ((char *)base) + stro;
	if(hdr->total_ofslimit - stro >= no){
	    bi.rest = no - 1;
	    bi.raise_error_p = 0;
	    r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "snprintf");
	    bi.rest++;
	    raw_string_output(&bi, "\0", 1);
	}else{
	    bi.rest = hdr->total_ofslimit - stro;
	    bi.raise_error_p = 1;
	    r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "snprintf");
	    raw_string_output(&bi, "\0", 1);
	}
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    } else {
	int r;
	struct cooked_buffer_info ci;
	ci.base = base;
	ci.ofs = stro;
	if(hdr->total_ofslimit - stro >= no){
	    ci.rest = no - 1;
	    ci.raise_error_p = 0;
	    r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "snprintf");
	    ci.rest++;
	    cooked_string_output(&ci, "\0", 1);
	}else{
	    ci.rest = hdr->total_ofslimit - stro;
	    ci.raise_error_p = 1;
	    r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "snprintf");
	    cooked_string_output(&ci, "\0", 1);
	}
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    }
}

/**
 * @fn int vsnprintf(char *str, size_t n, const char *format, va_list v)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPciPcPi_i_vsnprintf(base_t strb, ofs_t stro,
                              base_t nb, unsigned int no,
                              base_t b, ofs_t o,
                              base_t vb, ofs_t vo) {
    base_t base = base_remove_castflag(strb);
    fsc_header *hdr;

    if (no == 0) {
	int r;
	r = vkprintf_internal(null_string_output, NULL, b, o, vb, vo, "snprintf");
	fsc_va_end_base_ofs(vb, vo);
	return value_of_int(r);
    }

    hdr = get_header_fast(base);
    dealloc_check_fast(base, stro);
    if (hdr->total_ofslimit < stro) {
	/* out of bound pointer passed */
	fsc_raise_error_library(strb, stro, ERR_OUTOFBOUNDS, "vsnprintf");
    }

    if (hdr->tinfo->kind & TI_CONTINUOUS) {
	int r;
	struct raw_buffer_info bi;
	bi.base = base;
	bi.ofs = stro;
	bi.start_p = bi.p = ((char *)base) + stro;
	if(hdr->total_ofslimit - stro >= no){
	    bi.rest = no - 1;
	    bi.raise_error_p = 0;
	    r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "vsnprintf");
	    bi.rest++;
	    raw_string_output(&bi, "\0", 1);
	}else{
	    bi.rest = hdr->total_ofslimit - stro;
	    bi.raise_error_p = 1;
	    r = vkprintf_internal(raw_string_output, &bi, b, o, vb, vo, "vsnprintf");
	    raw_string_output(&bi, "\0", 1);
	}
	return value_of_int(r);
    } else {
	int r;
	struct cooked_buffer_info ci;
	ci.base = base;
	ci.ofs = stro;
	if(hdr->total_ofslimit - stro >= no){
	    ci.rest = no - 1;
	    ci.raise_error_p = 0;
	    r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "vsnprintf");
	    ci.rest++;
	    cooked_string_output(&ci, "\0", 1);
	}else{
	    ci.rest = hdr->total_ofslimit - stro;
	    ci.raise_error_p = 1;
	    r = vkprintf_internal(cooked_string_output, &ci, b, o, vb, vo, "vsnprintf");
	    cooked_string_output(&ci, "\0", 1);
	}
	return value_of_int(r);
    }
}
