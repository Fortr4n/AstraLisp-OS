/* AstraLisp OS Input Subsystem - Header */

#ifndef INPUT_H
#define INPUT_H

#include <stdint.h>
#include <stdbool.h>

/* Input Event Types */
#define INPUT_TYPE_NONE     0
#define INPUT_TYPE_KEY      1
#define INPUT_TYPE_MOUSE    2

/* Key States */
#define KEY_RELEASED 0
#define KEY_PRESSED  1

/* Mouse Buttons (Bitmap) */
#define MOUSE_BTN_LEFT   0x01
#define MOUSE_BTN_RIGHT  0x02
#define MOUSE_BTN_MIDDLE 0x04

/* Input Event Structure */
typedef struct input_event {
    uint32_t type;      /* INPUT_TYPE_* */
    uint32_t code;      /* Keycode or Mouse Axis (0=X, 1=Y) */
    int32_t  value;     /* Key State (0/1) or Mouse Rel Movement / Btn State */
    uint64_t timestamp; /* Ticks (if available) */
} input_event_t;

/* Initialization */
void input_init(void);

/* Producer Interface (Called by Drivers/ISRs) */
/* Returns 0 on success, -1 if buffer full */
int input_push_event(uint32_t type, uint32_t code, int32_t value);

/* Consumer Interface (Called by Kernel/Userspace) */
/* Pop event. if 'blocking' is true, sleep until event available. */
/* Returns 1 if event popped, 0 if empty (and non-blocking) */
int input_pop_event(input_event_t* ev, bool blocking);

#endif /* INPUT_H */
