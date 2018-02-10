/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef FSC_LINKINFO_H
#define FSC_LINKINFO_H

#define BEGIN_FSC_TYPEINFO FSC_TYPEINFO("R\t2\tABI=6,6")
#define END_FSC_TYPEINFO FSC_TYPEINFO_NLF("\0")

/* compatibility */
#define BEGIN_FSC_TYPEINFO_SECTION BEGIN_FSC_TYPEINFO
#define END_FSC_TYPEINFO_SECTION END_FSC_TYPEINFO

#define FSC_TYPEINFO(s) FSC_TYPEINFO_1(#s)
#define FSC_TYPEINFO_1(s) __asm__("\n.pushsection __failsafeC_typeinfo,  \"S\"\n.ascii\t" s " \"\\n\"\n.popsection\n");
#define FSC_TYPEINFO_NLF(s) __asm__("\n.pushsection __failsafeC_typeinfo,  \"S\"\n.ascii\t" #s "\n.popsection\n");

#define FSC_PROVIDE_VALUE(sym,typ) FSC_TYPEINFO("D\t" #sym "\t" typ)

#define FSC_REQUIRE_VALUE(sym,typ) FSC_TYPEINFO("E\t" #sym "\t" typ)

#endif
