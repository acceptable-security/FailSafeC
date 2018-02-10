/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct hoge { int a; };

void bar(struct hoge *s){
}

void foo(){
  struct hoge s;
  bar(&s);
}
