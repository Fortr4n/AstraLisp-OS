#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "gc/gc.h"
#include "lisp/objects.h"
#include "lisp/tagged.h"

void* kmalloc(size_t size) { return malloc(size); }
void kfree(void* ptr) { free(ptr); }

int main() {
    gc_init();
    
    // Test 1: Alloc Struct
    struct lisp_string* str = (struct lisp_string*)gc_alloc(sizeof(struct lisp_string));
    printf("Allocated at %p\n", str);
    
    SET_TYPE(&str->header, TYPE_STRING);
    printf("Set Type %d (TYPE_STRING=%d)\n", TYPE_STRING, TYPE_STRING);
    
    int type = GET_TYPE(str);
    printf("Read Type %d\n", type);
    
    if (type != TYPE_STRING) {
        printf("FAIL: Type mismatch!\n");
    } else {
        printf("PASS: Type correct.\n");
    }
    
    return 0;
}
