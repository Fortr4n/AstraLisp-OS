/* AstraLisp OS AstraUI Framework */

#ifndef ASTRAUI_H
#define ASTRAUI_H

#include <stdint.h>
#include <stddef.h>

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

/* Create window */
void* window_create(int32_t x, int32_t y, uint32_t width, uint32_t height);

/* Draw rectangle */
void draw_rect(void* surface, struct rect* rect, struct color* color);

/* Draw text */
void draw_text(void* surface, struct point* pos, const char* text, struct color* color);

/* Present surface */
void surface_present(void* surface);

#endif /* ASTRAUI_H */
