#ifndef RUNTIME_RPIVATE_STORAGE_H
#define RUNTIME_RPIVATE_STORAGE_H

base_t alloc_private_storage(void *init_v, const void *tag);
void  set_private_storage(base_t b, ofs_t o,  void *v, const void *tag);
void *get_private_storage(base_t b, ofs_t o, const void *tag);

#endif
