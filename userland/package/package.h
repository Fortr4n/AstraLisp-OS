/* AstraLisp OS Package Manager */

#ifndef PACKAGE_H
#define PACKAGE_H

#include <stdint.h>
#include <stddef.h>

/* Package structure */
struct package {
    char* name;
    char* version;
    char** dependencies;
    size_t dependency_count;
};

/* Initialize package manager */
int package_init(void);

/* Install package */
int package_install(const char* package_name);

/* Remove package */
int package_remove(const char* package_name);

/* List installed packages */
int package_list(void);

/* Update package */
int package_update(const char* package_name);

#endif /* PACKAGE_H */
