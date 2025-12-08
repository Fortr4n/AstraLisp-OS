/* AstraLisp OS AstraUI Framework Implementation */
/* Comprehensive framebuffer-based graphics system */

#include "astraui.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>

/* Framebuffer state */
static struct {
    uint32_t* buffer;
    uint32_t width;
    uint32_t height;
    uint32_t pitch;
    bool initialized;
} framebuffer = {0};

/* Basic 8x8 font for text rendering */
static const uint8_t font_8x8[128][8] = {
    /* Space through ~ - basic ASCII font bitmap */
    [' '] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00},
    ['!'] = {0x18,0x3C,0x3C,0x18,0x18,0x00,0x18,0x00},
    ['"'] = {0x36,0x36,0x00,0x00,0x00,0x00,0x00,0x00},
    ['0'] = {0x3C,0x66,0x6E,0x76,0x66,0x66,0x3C,0x00},
    ['1'] = {0x18,0x38,0x18,0x18,0x18,0x18,0x7E,0x00},
    ['A'] = {0x18,0x3C,0x66,0x7E,0x66,0x66,0x66,0x00},
    ['B'] = {0x7C,0x66,0x66,0x7C,0x66,0x66,0x7C,0x00},
    ['C'] = {0x3C,0x66,0x60,0x60,0x60,0x66,0x3C,0x00},
    ['a'] = {0x00,0x00,0x3C,0x06,0x3E,0x66,0x3E,0x00},
    ['b'] = {0x60,0x60,0x7C,0x66,0x66,0x66,0x7C,0x00},
    ['c'] = {0x00,0x00,0x3C,0x60,0x60,0x60,0x3C,0x00},
};

/* Window structure */
struct window {
    uint32_t id;
    int32_t x, y;
    uint32_t width, height;
    uint32_t* buffer;
    bool visible;
    bool dirty;
    struct window* next;
};

static struct window* window_list = NULL;
static uint32_t next_window_id = 1;

/* Initialize AstraUI with framebuffer */
int astraui_init(void) {
    /* Try to get framebuffer from multiboot or OPAL */
    /* Default to 1024x768 for QEMU VGA */
    framebuffer.width = 1024;
    framebuffer.height = 768;
    framebuffer.pitch = framebuffer.width * sizeof(uint32_t);
    
    /* Allocate back buffer */
    size_t buf_size = framebuffer.width * framebuffer.height * sizeof(uint32_t);
    framebuffer.buffer = (uint32_t*)kmalloc(buf_size);
    if (!framebuffer.buffer) {
        return -1;
    }
    
    memset(framebuffer.buffer, 0, buf_size);
    framebuffer.initialized = true;
    
    return 0;
}

/* Set framebuffer address (from bootloader info) */
void astraui_set_framebuffer(void* addr, uint32_t width, uint32_t height, uint32_t pitch) {
    if (addr) {
        if (framebuffer.buffer) {
            kfree(framebuffer.buffer);
        }
        framebuffer.buffer = (uint32_t*)addr;
        framebuffer.width = width;
        framebuffer.height = height;
        framebuffer.pitch = pitch;
        framebuffer.initialized = true;
    }
}

/* Convert color struct to 32-bit ARGB */
static inline uint32_t color_to_argb(struct color* c) {
    return ((uint32_t)c->a << 24) | ((uint32_t)c->r << 16) | 
           ((uint32_t)c->g << 8) | c->b;
}

/* Put pixel at coordinates */
static void put_pixel(uint32_t* buffer, uint32_t width, int32_t x, int32_t y, uint32_t color) {
    if (x >= 0 && x < (int32_t)width && y >= 0) {
        buffer[y * width + x] = color;
    }
}

/* Create window */
void* window_create(int32_t x, int32_t y, uint32_t width, uint32_t height) {
    struct window* win = (struct window*)kmalloc(sizeof(struct window));
    if (!win) {
        return NULL;
    }
    
    size_t buf_size = width * height * sizeof(uint32_t);
    win->buffer = (uint32_t*)kmalloc(buf_size);
    if (!win->buffer) {
        kfree(win);
        return NULL;
    }
    
    memset(win->buffer, 0, buf_size);
    
    win->id = next_window_id++;
    win->x = x;
    win->y = y;
    win->width = width;
    win->height = height;
    win->visible = true;
    win->dirty = true;
    
    /* Add to window list */
    win->next = window_list;
    window_list = win;
    
    return win;
}

/* Destroy window */
void window_destroy(void* window) {
    struct window* win = (struct window*)window;
    if (!win) return;
    
    /* Remove from list */
    struct window** pp = &window_list;
    while (*pp) {
        if (*pp == win) {
            *pp = win->next;
            break;
        }
        pp = &(*pp)->next;
    }
    
    if (win->buffer) {
        kfree(win->buffer);
    }
    kfree(win);
}

/* Draw filled rectangle */
void draw_rect(void* surface, struct rect* rect, struct color* color) {
    if (!surface || !rect || !color) {
        return;
    }
    
    struct window* win = (struct window*)surface;
    uint32_t argb = color_to_argb(color);
    
    int32_t x1 = rect->x;
    int32_t y1 = rect->y;
    int32_t x2 = rect->x + rect->width;
    int32_t y2 = rect->y + rect->height;
    
    /* Clamp to window bounds */
    if (x1 < 0) x1 = 0;
    if (y1 < 0) y1 = 0;
    if (x2 > (int32_t)win->width) x2 = win->width;
    if (y2 > (int32_t)win->height) y2 = win->height;
    
    /* Fill rectangle */
    for (int32_t y = y1; y < y2; y++) {
        for (int32_t x = x1; x < x2; x++) {
            win->buffer[y * win->width + x] = argb;
        }
    }
    
    win->dirty = true;
}

/* Draw rectangle outline */
void draw_rect_outline(void* surface, struct rect* rect, struct color* color, uint32_t thickness) {
    if (!surface || !rect || !color) return;
    
    struct window* win = (struct window*)surface;
    uint32_t argb = color_to_argb(color);
    
    /* Top edge */
    for (uint32_t t = 0; t < thickness; t++) {
        for (int32_t x = rect->x; x < rect->x + rect->width; x++) {
            put_pixel(win->buffer, win->width, x, rect->y + t, argb);
        }
    }
    
    /* Bottom edge */
    for (uint32_t t = 0; t < thickness; t++) {
        for (int32_t x = rect->x; x < rect->x + rect->width; x++) {
            put_pixel(win->buffer, win->width, x, rect->y + rect->height - 1 - t, argb);
        }
    }
    
    /* Left edge */
    for (uint32_t t = 0; t < thickness; t++) {
        for (int32_t y = rect->y; y < rect->y + rect->height; y++) {
            put_pixel(win->buffer, win->width, rect->x + t, y, argb);
        }
    }
    
    /* Right edge */
    for (uint32_t t = 0; t < thickness; t++) {
        for (int32_t y = rect->y; y < rect->y + rect->height; y++) {
            put_pixel(win->buffer, win->width, rect->x + rect->width - 1 - t, y, argb);
        }
    }
    
    win->dirty = true;
}

/* Draw text using 8x8 bitmap font */
void draw_text(void* surface, struct point* pos, const char* text, struct color* color) {
    if (!surface || !pos || !text || !color) {
        return;
    }
    
    struct window* win = (struct window*)surface;
    uint32_t argb = color_to_argb(color);
    
    int32_t x = pos->x;
    int32_t y = pos->y;
    
    while (*text) {
        char c = *text++;
        
        if (c == '\n') {
            x = pos->x;
            y += 8;
            continue;
        }
        
        if (c >= 0 && c < 128) {
            const uint8_t* glyph = font_8x8[(int)c];
            
            for (int row = 0; row < 8; row++) {
                for (int col = 0; col < 8; col++) {
                    if (glyph[row] & (0x80 >> col)) {
                        put_pixel(win->buffer, win->width, x + col, y + row, argb);
                    }
                }
            }
        }
        
        x += 8;
    }
    
    win->dirty = true;
}

/* Draw line using Bresenham's algorithm */
void draw_line(void* surface, struct point* p1, struct point* p2, struct color* color) {
    if (!surface || !p1 || !p2 || !color) return;
    
    struct window* win = (struct window*)surface;
    uint32_t argb = color_to_argb(color);
    
    int32_t x0 = p1->x, y0 = p1->y;
    int32_t x1 = p2->x, y1 = p2->y;
    
    int32_t dx = x1 > x0 ? x1 - x0 : x0 - x1;
    int32_t dy = y1 > y0 ? y1 - y0 : y0 - y1;
    int32_t sx = x0 < x1 ? 1 : -1;
    int32_t sy = y0 < y1 ? 1 : -1;
    int32_t err = dx - dy;
    
    while (1) {
        put_pixel(win->buffer, win->width, x0, y0, argb);
        
        if (x0 == x1 && y0 == y1) break;
        
        int32_t e2 = 2 * err;
        if (e2 > -dy) { err -= dy; x0 += sx; }
        if (e2 < dx) { err += dx; y0 += sy; }
    }
    
    win->dirty = true;
}

/* Present surface to framebuffer */
void surface_present(void* surface) {
    if (!surface || !framebuffer.initialized) {
        return;
    }
    
    struct window* win = (struct window*)surface;
    
    if (!win->visible || !win->dirty) {
        return;
    }
    
    /* Blit window to framebuffer */
    for (uint32_t y = 0; y < win->height; y++) {
        int32_t fb_y = win->y + y;
        if (fb_y < 0 || fb_y >= (int32_t)framebuffer.height) continue;
        
        for (uint32_t x = 0; x < win->width; x++) {
            int32_t fb_x = win->x + x;
            if (fb_x < 0 || fb_x >= (int32_t)framebuffer.width) continue;
            
            framebuffer.buffer[fb_y * framebuffer.width + fb_x] = 
                win->buffer[y * win->width + x];
        }
    }
    
    win->dirty = false;
}

/* Clear surface */
void surface_clear(void* surface, struct color* color) {
    if (!surface || !color) return;
    
    struct window* win = (struct window*)surface;
    uint32_t argb = color_to_argb(color);
    
    for (uint32_t i = 0; i < win->width * win->height; i++) {
        win->buffer[i] = argb;
    }
    
    win->dirty = true;
}

/* Get framebuffer info */
void astraui_get_framebuffer_info(uint32_t* width, uint32_t* height) {
    if (width) *width = framebuffer.width;
    if (height) *height = framebuffer.height;
}

