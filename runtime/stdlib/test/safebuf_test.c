#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_safebuf.h>

int main(void) {
    char *p, *q;

    fsc_init_runtime();

    p = fsc_allocate_overrun_safe_buffer(0, 10000, 10);

    printf("%p\n", fsc_safe_buffer_list);

    q = fsc_allocate_overrun_safe_buffer(0, 20000, 20);

    printf("%p\n", fsc_safe_buffer_list);

    gets(p);
    gets(q);

    printf("%p\n", fsc_safe_buffer_list);

    fsc_free_overrun_safe_buffer(p);

    printf("%p\n", fsc_safe_buffer_list);

    fsc_free_overrun_safe_buffer(q);

    printf("%p\n", fsc_safe_buffer_list);

    return 0;
}

void fsc_init_modules(void) {}
