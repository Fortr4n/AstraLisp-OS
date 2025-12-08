/* AstraLisp OS Package Manager */
/* Comprehensive package management with dependency resolution */

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

/* Search packages */
int package_search(const char* query, char results[][64], size_t max_results);

/* Get package info */
int package_info(const char* package_name, struct package* pkg_out);

#endif /* PACKAGE_H */

