/* AstraLisp OS AstraUI Framework */
/* Comprehensive framebuffer-based graphics system */

#ifndef ASTRAUI_H
#define ASTRAUI_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Color structure */
struct color {
    uint8_t r, g, b, a;
};

/* Point structure */
struct point {
    int32_t x, y;
};

/* Rectangle structure */
struct rect {
    int32_t x, y, width, height;
};

/* Initialize AstraUI */
int astraui_init(void);

/* Set framebuffer address */
void astraui_set_framebuffer(void* addr, uint32_t width, uint32_t height, uint32_t pitch);

/* Get framebuffer info */
void astraui_get_framebuffer_info(uint32_t* width, uint32_t* height);

/* Window management */
void* window_create(int32_t x, int32_t y, uint32_t width, uint32_t height);
void window_destroy(void* window);

/* Drawing primitives */
void draw_rect(void* surface, struct rect* rect, struct color* color);
void draw_rect_outline(void* surface, struct rect* rect, struct color* color, uint32_t thickness);
void draw_line(void* surface, struct point* p1, struct point* p2, struct color* color);
void draw_text(void* surface, struct point* pos, const char* text, struct color* color);

/* Surface operations */
void surface_present(void* surface);
void surface_clear(void* surface, struct color* color);

#endif /* ASTRAUI_H */

