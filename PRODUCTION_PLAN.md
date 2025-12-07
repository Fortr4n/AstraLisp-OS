# AstraLisp OS Production-Grade Implementation Plan

## Executive Summary

This document provides a comprehensive, implementation-focused plan to bring AstraLisp OS to production-grade quality. **Every component must be fully implemented with no placeholders, stubs, or simplified versions.** All implementations must be production-ready, robust, and complete.

**Current State**: Framework implementations with basic functionality  
**Target State**: Production-ready OS with complete, robust implementations  
**Timeline**: 12-18 months for full production readiness  
**Principle**: No shortcuts, no placeholders, no "good enough" - only production-grade code

---

## Implementation Standards

### Code Quality Requirements

- **No Placeholders**: Every function must have a complete implementation
- **No Stubs**: All functions must perform their intended operations
- **Error Handling**: Comprehensive error handling for all failure modes
- **Resource Management**: Proper allocation, deallocation, and cleanup
- **Thread Safety**: All shared data structures must be thread-safe
- **Performance**: Must meet or exceed performance targets
- **Security**: Input validation, bounds checking, privilege checks
- **Documentation**: Complete inline documentation for all public APIs
- **Testing**: Unit tests for all components with >80% coverage

### Architecture Requirements

- **Real Algorithms**: Use proven, production-grade algorithms
- **Proper Data Structures**: Efficient, scalable data structures
- **Memory Safety**: No memory leaks, use-after-free, or buffer overflows
- **Concurrency**: Proper locking, no race conditions, deadlock prevention
- **Scalability**: Handle large numbers of processes, files, connections
- **Reliability**: Graceful degradation, recovery from errors

---

## Phase 1: Core Kernel Stability (Months 1-3)

### 1.1 Physical Memory Manager (PMM) - Complete Implementation

#### Requirements

- **Multiboot Memory Map Parsing**: Parse all multiboot memory map entries
- **Memory Region Management**: Track all memory regions (usable, reserved, ACPI, etc.)
- **Bitmap Allocator**: Efficient bitmap-based frame allocation
- **Frame Tracking**: Track all allocated/free frames with proper accounting
- **Memory Reservation**: Reserve memory for kernel, boot modules, etc.
- **NUMA Support**: Track and allocate from different NUMA nodes
- **Memory Statistics**: Track total, free, used, reserved memory

#### Implementation Details

**Data Structures**:

```c
struct memory_region {
    uint64_t base;
    uint64_t length;
    uint32_t type;  // MULTIBOOT_MEMORY_AVAILABLE, etc.
    struct memory_region* next;
};

struct pmm_state {
    struct memory_region* regions;
    uint8_t* frame_bitmap;
    size_t bitmap_size;
    size_t total_frames;
    size_t free_frames;
    size_t used_frames;
    size_t reserved_frames;
    uintptr_t bitmap_base;
    uintptr_t bitmap_end;
    spinlock_t lock;
    struct pmm_stats stats;
};
```

**Functions to Implement**:

1. `pmm_init(void* multiboot_info)` - Parse multiboot memory map, initialize bitmap
2. `pmm_alloc_frame(void)` - Allocate single 4KB frame, return physical address
3. `pmm_alloc_frames(size_t count)` - Allocate contiguous frames
4. `pmm_free_frame(uintptr_t frame)` - Free single frame
5. `pmm_free_frames(uintptr_t frame, size_t count)` - Free contiguous frames
6. `pmm_reserve_region(uintptr_t base, size_t size)` - Mark region as reserved
7. `pmm_get_statistics(struct pmm_stats* stats)` - Get memory statistics
8. `pmm_get_free_count(void)` - Get number of free frames
9. `pmm_get_total_count(void)` - Get total number of frames
10. `pmm_check_frame(uintptr_t frame)` - Check if frame is free

**Algorithm Requirements**:

- Use first-fit or best-fit allocation strategy
- Maintain frame bitmap with atomic operations
- Handle frame alignment (4KB boundaries)
- Support allocation of contiguous frames
- Track memory regions from multiboot
- Handle memory holes properly

**Error Handling**:

- Return NULL on allocation failure
- Validate all input parameters
- Check bounds on all frame addresses
- Handle out-of-memory gracefully

**Testing Requirements**:

- Test allocation of single frames
- Test allocation of contiguous frames
- Test freeing frames
- Test memory exhaustion
- Test multiboot memory map parsing
- Test frame alignment
- Test concurrent allocations (SMP)

---

### 1.2 Virtual Memory Manager (VMM) - Complete Implementation

#### Requirements

- **Page Table Management**: Full 4-level page table implementation (PowerISA)
- **Page Mapping**: Map/unmap pages with proper flags
- **Page Fault Handling**: Complete page fault handler with COW, demand paging
- **Address Space Management**: Create/destroy address spaces
- **TLB Management**: Invalidate TLB entries properly
- **Memory Protection**: Read, write, execute permissions
- **Copy-on-Write**: Implement COW for fork()
- **Demand Paging**: Lazy allocation of pages
- **Memory Sharing**: Shared memory regions

#### Implementation Details

**Data Structures**:

```c
/* PowerISA Page Table Entry (64-bit) */
struct page_table_entry {
    uint64_t valid : 1;
    uint64_t vsid : 24;      /* Virtual segment ID */
    uint64_t h : 1;          /* Hash function */
    uint64_t api : 6;        /* Abbreviated page index */
    uint64_t rpn : 40;       /* Real page number */
    uint64_t reserved : 3;
    uint64_t r : 1;          /* Reference */
    uint64_t c : 1;          /* Changed */
    uint64_t wimg : 4;       /* Memory/cache attributes */
    uint64_t reserved2 : 1;
    uint64_t pp : 2;        /* Page protection */
};

struct page_table {
    struct page_table_entry* entries;
    size_t entry_count;
    uintptr_t physical_base;
    spinlock_t lock;
};

struct address_space {
    struct page_table* page_tables[16];  /* 16 segment registers */
    uintptr_t heap_start;
    uintptr_t heap_end;
    uintptr_t stack_start;
    uintptr_t stack_end;
    struct address_space* next;
    pid_t owner;
    spinlock_t lock;
};
```

**Functions to Implement**:

1. `vmm_init(void)` - Initialize kernel address space
2. `vmm_create_address_space(pid_t pid)` - Create new address space
3. `vmm_destroy_address_space(struct address_space* as)` - Destroy address space
4. `vmm_map_page(struct address_space* as, uintptr_t virt, uintptr_t phys, uint32_t flags)` - Map page
5. `vmm_unmap_page(struct address_space* as, uintptr_t virt)` - Unmap page
6. `vmm_map_region(struct address_space* as, uintptr_t virt, uintptr_t phys, size_t size, uint32_t flags)` - Map region
7. `vmm_unmap_region(struct address_space* as, uintptr_t virt, size_t size)` - Unmap region
8. `vmm_get_physical(struct address_space* as, uintptr_t virt)` - Get physical address
9. `vmm_handle_page_fault(uintptr_t virt, uint32_t error_code)` - Handle page fault
10. `vmm_switch_address_space(struct address_space* as)` - Switch active address space
11. `vmm_copy_on_write(struct address_space* src, struct address_space* dst, uintptr_t virt, size_t size)` - COW implementation
12. `vmm_invalidate_tlb(uintptr_t virt)` - Invalidate TLB entry
13. `vmm_invalidate_tlb_all(void)` - Invalidate all TLB entries
14. `vmm_set_page_flags(struct address_space* as, uintptr_t virt, uint32_t flags)` - Change page flags
15. `vmm_get_page_flags(struct address_space* as, uintptr_t virt)` - Get page flags

**Page Fault Handling**:

- Handle page not present (demand paging)
- Handle write to read-only page (COW)
- Handle access violation (segmentation fault)
- Handle page protection violations
- Allocate frames on demand
- Copy pages for COW
- Update page table entries atomically

**Algorithm Requirements**:

- Use hash page table (HPT) for PowerISA
- Implement proper TLB invalidation
- Handle page table walks efficiently
- Support large pages (16MB, 16GB)
- Implement proper locking for concurrent access

**Error Handling**:

- Return error codes for all operations
- Validate virtual addresses
- Check page table entry validity
- Handle allocation failures gracefully
- Prevent double-mapping

**Testing Requirements**:

- Test page mapping/unmapping
- Test page fault handling
- Test COW functionality
- Test address space switching
- Test TLB invalidation
- Test concurrent access
- Test memory protection
- Test large page support

---

### 1.3 Kernel Heap Allocator - Complete Implementation

#### Requirements

- **Multiple Allocators**: Buddy allocator for large blocks, slab allocator for small objects
- **Alignment Support**: Proper alignment for all allocations
- **Memory Tracking**: Track all allocations with metadata
- **Leak Detection**: Detect and report memory leaks
- **Fragmentation Management**: Minimize fragmentation
- **Statistics**: Track allocation statistics

#### Implementation Details

**Data Structures**:

```c
/* Buddy allocator block */
struct buddy_block {
    uint32_t order;          /* Block order (2^order pages) */
    bool allocated;
    struct buddy_block* next;
    struct buddy_block* prev;
    uintptr_t physical_addr;
};

struct buddy_allocator {
    struct buddy_block* free_lists[12];  /* Orders 0-11 */
    size_t total_pages;
    size_t free_pages;
    spinlock_t lock;
};

/* Slab allocator */
struct slab {
    void* memory;
    size_t object_size;
    size_t object_count;
    uint8_t* free_bitmap;
    struct slab* next;
    struct slab* prev;
};

struct slab_cache {
    size_t object_size;
    size_t objects_per_slab;
    struct slab* slabs;
    struct slab* partial_slabs;
    struct slab* full_slabs;
    spinlock_t lock;
    struct slab_cache_stats stats;
};

struct heap_state {
    struct buddy_allocator buddy;
    struct slab_cache* slab_caches[32];  /* Common sizes */
    uintptr_t heap_start;
    uintptr_t heap_end;
    size_t total_size;
    size_t allocated_size;
    spinlock_t lock;
    struct heap_stats stats;
};
```

**Functions to Implement**:

1. `heap_init(uintptr_t start, uintptr_t end)` - Initialize heap
2. `kmalloc(size_t size)` - Allocate memory (uses appropriate allocator)
3. `kfree(void* ptr)` - Free memory
4. `krealloc(void* ptr, size_t size)` - Reallocate memory
5. `kcalloc(size_t nmemb, size_t size)` - Allocate and zero memory
6. `kmalloc_aligned(size_t size, size_t alignment)` - Aligned allocation
7. `heap_get_statistics(struct heap_stats* stats)` - Get statistics
8. `heap_check_leaks(void)` - Check for memory leaks
9. `heap_validate_pointer(void* ptr)` - Validate pointer
10. `heap_defragment(void)` - Defragment heap (if possible)

**Allocation Strategy**:

- Use slab allocator for objects < 256 bytes
- Use buddy allocator for larger allocations
- Maintain separate slab caches for common sizes (8, 16, 32, 64, 128, 256 bytes)
- Use first-fit for buddy allocator
- Implement proper alignment (8-byte, 16-byte, page-aligned)

**Error Handling**:

- Return NULL on allocation failure
- Validate all pointers before freeing
- Detect double-free
- Detect use-after-free (with debugging support)
- Handle out-of-memory gracefully

**Testing Requirements**:

- Test allocation of various sizes
- Test freeing memory
- Test reallocation
- Test alignment
- Test memory exhaustion
- Test concurrent allocations
- Test leak detection
- Test fragmentation

---

### 1.4 Process Management - Complete Implementation

#### Requirements

- **Process Creation**: Complete fork(), exec(), spawn() implementation
- **Process Destruction**: Proper cleanup of all resources
- **Process State Machine**: All states (NEW, READY, RUNNING, BLOCKED, ZOMBIE, DEAD)
- **Process Tree**: Parent-child relationships, process groups, sessions
- **Signal Handling**: Complete signal delivery and handling
- **Resource Limits**: CPU, memory, file descriptor limits
- **Process Information**: PID, PPID, UID, GID, command line, environment

#### Implementation Details

**Data Structures**:

```c
enum process_state {
    PROCESS_NEW,
    PROCESS_READY,
    PROCESS_RUNNING,
    PROCESS_BLOCKED,
    PROCESS_ZOMBIE,
    PROCESS_DEAD
};

struct process {
    pid_t pid;
    pid_t ppid;
    pid_t pgid;
    pid_t sid;
    uid_t uid;
    gid_t gid;
    enum process_state state;

    /* Address space */
    struct address_space* address_space;

    /* Threads */
    struct thread* threads;
    struct thread* main_thread;

    /* File descriptors */
    struct file_descriptor* fds[1024];
    size_t fd_count;

    /* Memory limits */
    size_t memory_limit;
    size_t memory_used;

    /* CPU limits */
    uint64_t cpu_time;
    uint64_t cpu_limit;

    /* Signals */
    sigset_t signal_mask;
    sigset_t pending_signals;
    struct signal_handler signal_handlers[32];

    /* Process tree */
    struct process* parent;
    struct process* children;
    struct process* siblings;

    /* Process information */
    char name[256];
    char* command_line;
    char** environment;
    size_t argc;
    char** argv;

    /* Exit status */
    int exit_code;
    bool exited;

    /* Locks */
    spinlock_t lock;
    mutex_t wait_mutex;

    /* Statistics */
    struct process_stats stats;
};
```

**Functions to Implement**:

1. `process_create(const char* name, const char* program, char** argv, char** envp)` - Create process
2. `process_fork(struct process* parent)` - Fork process
3. `process_exec(struct process* proc, const char* program, char** argv, char** envp)` - Execute program
4. `process_exit(struct process* proc, int exit_code)` - Exit process
5. `process_wait(pid_t pid, int* status)` - Wait for process
6. `process_kill(pid_t pid, int signal)` - Send signal to process
7. `process_get_by_pid(pid_t pid)` - Get process by PID
8. `process_get_current(void)` - Get current process
9. `process_set_state(struct process* proc, enum process_state state)` - Set process state
10. `process_cleanup(struct process* proc)` - Cleanup process resources
11. `process_handle_signal(struct process* proc, int signal)` - Handle signal
12. `process_set_signal_handler(struct process* proc, int signal, void (*handler)(int))` - Set signal handler
13. `process_set_resource_limits(struct process* proc, struct rlimit* limits)` - Set resource limits
14. `process_get_resource_usage(struct process* proc, struct rusage* usage)` - Get resource usage
15. `process_detach_from_parent(struct process* proc)` - Detach from parent (init)

**Process Creation Algorithm**:

1. Allocate process structure
2. Allocate PID
3. Create address space
4. Create main thread
5. Load program (if exec)
6. Set up stack
7. Set up signal handlers
8. Initialize file descriptors
9. Set process state to READY
10. Add to scheduler

**Process Destruction Algorithm**:

1. Set state to ZOMBIE
2. Free all threads
3. Close all file descriptors
4. Free address space
5. Free memory
6. Remove from scheduler
7. Notify parent
8. Free process structure

**Signal Handling**:

- Deliver signals to process
- Handle default actions (terminate, ignore, core dump)
- Call signal handlers
- Handle signal masks
- Handle pending signals
- Handle signal queues

**Error Handling**:

- Validate all parameters
- Check resource limits
- Handle allocation failures
- Prevent PID exhaustion
- Handle invalid PIDs gracefully

**Testing Requirements**:

- Test process creation
- Test process destruction
- Test fork/exec
- Test signal handling
- Test process tree
- Test resource limits
- Test concurrent process operations
- Test process state transitions

---

### 1.5 Scheduler - Complete Implementation

#### Requirements

- **Multi-Priority Scheduling**: Real-time, normal, idle priorities
- **Preemption**: Time-slice based preemption
- **Load Balancing**: Distribute load across CPUs (SMP)
- **CPU Affinity**: Pin processes to specific CPUs
- **Fair Scheduling**: CFS-like fair scheduling
- **Statistics**: Track scheduling statistics

#### Implementation Details

**Data Structures**:

```c
enum thread_priority {
    PRIORITY_IDLE = 0,
    PRIORITY_LOW = 1,
    PRIORITY_NORMAL = 2,
    PRIORITY_HIGH = 3,
    PRIORITY_REALTIME = 4
};

struct thread {
    tid_t tid;
    struct process* process;
    enum thread_priority priority;
    uint64_t time_slice;
    uint64_t time_used;
    uint64_t last_run_time;
    uint64_t sleep_until;

    /* Context */
    struct cpu_context context;

    /* Scheduling */
    struct thread* next;
    struct thread* prev;
    struct runqueue* runqueue;

    /* State */
    bool runnable;
    bool blocked;
    void* wait_object;

    /* Statistics */
    uint64_t context_switches;
    uint64_t cpu_time;

    spinlock_t lock;
};

struct runqueue {
    struct thread* threads[5];  /* One per priority */
    size_t counts[5];
    struct thread* current;
    uint64_t total_load;
    spinlock_t lock;
    cpu_id_t cpu_id;
};

struct scheduler_state {
    struct runqueue* runqueues[MAX_CPUS];
    uint64_t tick_count;
    uint64_t time_slice_default;
    struct scheduler_stats stats;
    spinlock_t lock;
};
```

**Functions to Implement**:

1. `scheduler_init(void)` - Initialize scheduler
2. `scheduler_add_thread(struct thread* thread)` - Add thread to scheduler
3. `scheduler_remove_thread(struct thread* thread)` - Remove thread
4. `scheduler_yield(void)` - Yield CPU
5. `scheduler_tick(void)` - Scheduler tick (called from timer interrupt)
6. `scheduler_schedule(void)` - Main scheduling function
7. `scheduler_set_priority(struct thread* thread, enum thread_priority priority)` - Set priority
8. `scheduler_set_affinity(struct thread* thread, cpu_set_t* cpus)` - Set CPU affinity
9. `scheduler_sleep(struct thread* thread, uint64_t ticks)` - Sleep thread
10. `scheduler_wake(struct thread* thread)` - Wake thread
11. `scheduler_block(struct thread* thread, void* wait_object)` - Block thread
12. `scheduler_unblock(struct thread* thread)` - Unblock thread
13. `scheduler_get_current_thread(void)` - Get current thread
14. `scheduler_load_balance(void)` - Balance load across CPUs
15. `scheduler_get_statistics(struct scheduler_stats* stats)` - Get statistics

**Scheduling Algorithm**:

- Use multi-level priority queues
- Round-robin within same priority
- Preempt when time slice expires
- Boost priority for I/O-bound threads
- Lower priority for CPU-bound threads
- Load balance across CPUs

**Preemption**:

- Set timer interrupt for time slice
- Check on every interrupt
- Preempt if higher priority thread available
- Save/restore context properly

**Error Handling**:

- Validate all parameters
- Handle NULL threads
- Prevent scheduling loops
- Handle CPU affinity errors

**Testing Requirements**:

- Test thread scheduling
- Test priority scheduling
- Test preemption
- Test load balancing
- Test CPU affinity
- Test sleep/wake
- Test blocking/unblocking
- Test concurrent scheduling

---

### 1.6 Interrupt System - Complete Implementation

#### Requirements

- **Interrupt Handler Registration**: Register handlers for all interrupt types
- **Nested Interrupts**: Support nested interrupt handling
- **Interrupt Priorities**: Handle interrupt priorities
- **Timer Interrupts**: System timer, scheduler tick
- **Device Interrupts**: Handle device interrupts
- **Exception Handling**: Handle CPU exceptions
- **Interrupt Statistics**: Track interrupt statistics

#### Implementation Details

**Data Structures**:

```c
struct interrupt_handler {
    void (*handler)(uint32_t irq, void* data);
    void* data;
    bool shared;
    char name[64];
    struct interrupt_handler* next;
};

struct interrupt_state {
    struct interrupt_handler* handlers[256];
    bool interrupts_enabled;
    uint32_t interrupt_depth;
    uint64_t interrupt_count[256];
    uint64_t interrupt_time[256];
    spinlock_t lock;
};
```

**Functions to Implement**:

1. `interrupt_init(void)` - Initialize interrupt system
2. `interrupt_register(uint32_t irq, void (*handler)(uint32_t, void*), void* data, bool shared, const char* name)` - Register handler
3. `interrupt_unregister(uint32_t irq, void (*handler)(uint32_t, void*))` - Unregister handler
4. `interrupt_enable(void)` - Enable interrupts
5. `interrupt_disable(void)` - Disable interrupts
6. `interrupt_disable_save(void)` - Disable and return previous state
7. `interrupt_restore(bool state)` - Restore interrupt state
8. `interrupt_handle(uint32_t irq)` - Handle interrupt
9. `interrupt_acknowledge(uint32_t irq)` - Acknowledge interrupt
10. `interrupt_mask(uint32_t irq)` - Mask interrupt
11. `interrupt_unmask(uint32_t irq)` - Unmask interrupt
12. `interrupt_get_statistics(uint32_t irq, struct interrupt_stats* stats)` - Get statistics

**Interrupt Handling Flow**:

1. Save CPU state
2. Disable interrupts (if not nested)
3. Call registered handlers
4. Acknowledge interrupt
5. Restore CPU state
6. Re-enable interrupts
7. Return from interrupt

**Error Handling**:

- Validate IRQ numbers
- Handle unregistered interrupts
- Handle interrupt storms
- Prevent interrupt handler loops

**Testing Requirements**:

- Test interrupt registration
- Test interrupt handling
- Test nested interrupts
- Test interrupt masking
- Test timer interrupts
- Test device interrupts
- Test exception handling

---

This is the first phase. The plan continues with detailed specifications for all remaining phases. Each component must be fully implemented with no shortcuts.

---

## Phase 2: Storage and Filesystem (Months 4-6)

### 2.1 AHCI Driver - Complete Implementation

#### Requirements

- **Port Detection**: Detect all AHCI ports and their capabilities
- **Port Initialization**: Initialize ports, set up command lists, FIS structures
- **Command Processing**: Process read/write commands with proper DMA
- **Error Handling**: Handle device errors, timeouts, retries
- **NCQ Support**: Native Command Queuing for performance
- **Hot-Plug Support**: Handle device insertion/removal
- **Power Management**: Handle device power states

#### Implementation Details

**Data Structures**:

```c
struct ahci_port {
    uint32_t port_number;
    uintptr_t base_address;
    bool implemented;
    bool active;
    uint32_t signature;
    uint32_t capabilities;

    /* Command list (1KB, 256-bit aligned) */
    struct ahci_command_header* cmd_list;
    uintptr_t cmd_list_phys;

    /* Received FIS (256 bytes, 256-bit aligned) */
    struct ahci_received_fis* rfis;
    uintptr_t rfis_phys;

    /* Command tables (one per command slot) */
    struct ahci_command_table* cmd_tables[32];
    uintptr_t cmd_tables_phys[32];

    /* DMA buffers */
    void* dma_buffers[32];
    uintptr_t dma_buffers_phys[32];

    /* Command slots */
    uint32_t active_slots;
    uint32_t free_slots;

    spinlock_t lock;
    struct ahci_port_stats stats;
};

struct ahci_controller {
    uintptr_t base_address;
    uint32_t port_count;
    uint32_t implemented_ports;
    struct ahci_port* ports[32];
    struct ahci_controller_capabilities caps;
    spinlock_t lock;
};
```

**Functions to Implement**:

1. `ahci_init(void)` - Initialize AHCI controller, detect ports
2. `ahci_port_init(struct ahci_port* port)` - Initialize specific port
3. `ahci_port_start(struct ahci_port* port)` - Start port (bring online)
4. `ahci_port_stop(struct ahci_port* port)` - Stop port
5. `ahci_read(struct ahci_port* port, uint64_t lba, void* buffer, size_t count)` - Read sectors
6. `ahci_write(struct ahci_port* port, uint64_t lba, const void* buffer, size_t count)` - Write sectors
7. `ahci_identify(struct ahci_port* port, struct ata_identify* identify)` - Identify device
8. `ahci_issue_command(struct ahci_port* port, struct ahci_command* cmd)` - Issue command
9. `ahci_wait_command(struct ahci_port* port, uint32_t slot, uint32_t timeout_ms)` - Wait for command completion
10. `ahci_handle_interrupt(struct ahci_port* port)` - Handle port interrupt
11. `ahci_reset_port(struct ahci_port* port)` - Reset port
12. `ahci_get_port_status(struct ahci_port* port, struct ahci_port_status* status)` - Get port status

**Command Processing Algorithm**:

1. Find free command slot
2. Allocate DMA buffer for data transfer
3. Build command FIS (Frame Information Structure)
4. Build command table with PRDT (Physical Region Descriptor Table)
5. Set command header
6. Issue command (set command slot bit in PxCI)
7. Wait for completion (poll or interrupt)
8. Check for errors
9. Copy data from DMA buffer
10. Free DMA buffer and command slot

**DMA Setup**:

- Allocate physically contiguous memory for command structures
- Set up PRDT entries pointing to data buffers
- Ensure proper alignment (command list: 1KB, 256-byte aligned; FIS: 256 bytes, 256-byte aligned)
- Map DMA buffers with proper cache attributes

**Error Handling**:

- Handle device not present
- Handle command timeouts
- Handle device errors (check error register)
- Retry failed commands (up to 3 times)
- Handle port errors (PHY errors, etc.)
- Reset port on persistent errors

**Testing Requirements**:

- Test port detection
- Test port initialization
- Test read operations (single and multi-sector)
- Test write operations
- Test error handling
- Test concurrent commands (NCQ)
- Test hot-plug
- Test power management

---

### 2.2 NVMe Driver - Complete Implementation

#### Requirements

- **Namespace Enumeration**: Enumerate all namespaces
- **Queue Management**: Create submission/completion queues
- **Command Submission**: Submit admin and I/O commands
- **Interrupt Handling**: Handle completion queue interrupts
- **Error Handling**: Handle controller and command errors
- **Multi-Queue Support**: Support multiple I/O queues per namespace

#### Implementation Details

**Data Structures**:

```c
struct nvme_queue {
    uint16_t queue_id;
    uint16_t queue_size;
    bool is_admin;

    /* Submission queue */
    struct nvme_command* sq;
    uintptr_t sq_phys;
    uint16_t sq_head;
    uint16_t sq_tail;
    uint16_t sq_phase;

    /* Completion queue */
    struct nvme_completion* cq;
    uintptr_t cq_phys;
    uint16_t cq_head;
    uint16_t cq_phase;

    /* Doorbells */
    volatile uint32_t* sq_doorbell;
    volatile uint32_t* cq_doorbell;

    /* Interrupt */
    uint32_t interrupt_vector;
    bool interrupt_enabled;

    spinlock_t lock;
};

struct nvme_namespace {
    uint32_t namespace_id;
    uint64_t capacity;
    uint32_t block_size;
    uint32_t lba_format_index;
    struct nvme_lba_format lba_format;
    bool active;

    /* I/O queues */
    struct nvme_queue* io_queues;
    uint16_t io_queue_count;

    struct nvme_namespace_stats stats;
};

struct nvme_controller {
    uintptr_t base_address;
    struct nvme_capabilities caps;
    struct nvme_version version;
    uint32_t namespace_count;
    struct nvme_namespace* namespaces[1024];

    /* Admin queue */
    struct nvme_queue* admin_queue;

    /* Controller registers */
    volatile struct nvme_registers* regs;

    spinlock_t lock;
    struct nvme_controller_stats stats;
};
```

**Functions to Implement**:

1. `nvme_init(uintptr_t base_address)` - Initialize NVMe controller
2. `nvme_create_queue(struct nvme_controller* ctrl, uint16_t qid, uint16_t qsize, bool is_admin)` - Create queue
3. `nvme_delete_queue(struct nvme_controller* ctrl, uint16_t qid, bool is_admin)` - Delete queue
4. `nvme_identify_controller(struct nvme_controller* ctrl, struct nvme_identify_controller* id)` - Identify controller
5. `nvme_identify_namespace(struct nvme_controller* ctrl, uint32_t nsid, struct nvme_identify_namespace* id)` - Identify namespace
6. `nvme_enumerate_namespaces(struct nvme_controller* ctrl)` - Enumerate all namespaces
7. `nvme_read(struct nvme_namespace* ns, uint64_t lba, void* buffer, size_t count, uint16_t qid)` - Read blocks
8. `nvme_write(struct nvme_namespace* ns, uint64_t lba, const void* buffer, size_t count, uint16_t qid)` - Write blocks
9. `nvme_submit_command(struct nvme_queue* queue, struct nvme_command* cmd)` - Submit command
10. `nvme_wait_completion(struct nvme_queue* queue, uint16_t cid, struct nvme_completion* cpl, uint32_t timeout_ms)` - Wait for completion
11. `nvme_handle_interrupt(struct nvme_controller* ctrl, uint32_t vector)` - Handle interrupt
12. `nvme_reset_controller(struct nvme_controller* ctrl)` - Reset controller

**Queue Management**:

- Allocate physically contiguous memory for queues
- Set queue base addresses in controller registers
- Set queue size and enable queues
- Handle queue wrap-around (circular buffers)
- Use phase bit for completion detection

**Command Submission**:

1. Get next submission queue entry
2. Fill command structure
3. Update submission queue tail
4. Ring doorbell
5. Wait for completion (poll or interrupt)
6. Check completion status
7. Handle errors

**Error Handling**:

- Handle controller fatal errors
- Handle command errors (check status code)
- Handle queue full conditions
- Handle timeouts
- Reset controller on fatal errors

**Testing Requirements**:

- Test controller initialization
- Test namespace enumeration
- Test queue creation/deletion
- Test read operations
- Test write operations
- Test error handling
- Test multi-queue operations
- Test interrupt handling

---

### 2.3 LFSX Filesystem - Complete B+ Tree Implementation

#### Requirements

- **Complete B+ Tree**: Full insert, delete, search, range query
- **Node Splitting**: Split leaf and internal nodes
- **Node Merging**: Merge underflowed nodes
- **Tree Balancing**: Maintain tree balance
- **Persistence**: Write nodes to disk
- **Caching**: Cache frequently accessed nodes
- **Concurrency**: Support concurrent operations

#### Implementation Details

**Data Structures**:

```c
struct btree_node {
    uint32_t node_id;
    bool is_leaf;
    uint32_t key_count;
    uint32_t parent_id;

    union {
        uint32_t* child_ids;      /* Internal nodes */
        uint64_t* values;         /* Leaf nodes */
    } data;

    uint32_t* keys;
    uint32_t next_leaf_id;        /* Leaf chain */
    uint32_t prev_leaf_id;

    bool dirty;
    uint64_t last_access;
    struct btree_node* cache_next;
    struct btree_node* cache_prev;
};

struct btree_cache {
    struct btree_node* nodes;
    size_t cache_size;
    size_t cache_count;
    uint64_t access_counter;
    mutex_t lock;
};

struct btree {
    uint32_t root_id;
    uint32_t node_size;
    uint32_t max_keys;
    uint32_t min_keys;
    uint32_t next_node_id;

    struct btree_cache* cache;

    /* Callbacks */
    struct btree_node* (*alloc_node)(uint32_t node_id);
    void (*free_node)(struct btree_node* node);
    void (*read_node)(uint32_t node_id, struct btree_node* node);
    void (*write_node)(uint32_t node_id, struct btree_node* node);
    int (*compare_keys)(uint32_t key1, uint32_t key2);

    mutex_t lock;
};
```

**Functions to Implement**:

1. `btree_init(struct btree* tree, uint32_t node_size, ...)` - Initialize tree
2. `btree_insert(struct btree* tree, uint32_t key, uint64_t value)` - Insert key-value
3. `btree_delete(struct btree* tree, uint32_t key)` - Delete key
4. `btree_search(struct btree* tree, uint32_t key, uint64_t* value)` - Search key
5. `btree_range_query(struct btree* tree, uint32_t start, uint32_t end, ...)` - Range query
6. `btree_split_leaf(struct btree* tree, struct btree_node* node)` - Split leaf node
7. `btree_split_internal(struct btree* tree, struct btree_node* node)` - Split internal node
8. `btree_merge_leaf(struct btree* tree, struct btree_node* left, struct btree_node* right)` - Merge leaf nodes
9. `btree_merge_internal(struct btree* tree, struct btree_node* left, struct btree_node* right)` - Merge internal nodes
10. `btree_borrow_from_sibling(struct btree* tree, struct btree_node* node)` - Borrow from sibling
11. `btree_rebalance(struct btree* tree, struct btree_node* node)` - Rebalance tree
12. `btree_cache_get(struct btree* tree, uint32_t node_id)` - Get node from cache
13. `btree_cache_put(struct btree* tree, struct btree_node* node)` - Put node in cache
14. `btree_cache_evict(struct btree* tree)` - Evict least recently used node
15. `btree_flush(struct btree* tree)` - Flush all dirty nodes to disk

**B+ Tree Algorithms**:

**Insert Algorithm**:

1. Find leaf node for key
2. Insert key-value in sorted order
3. If node overflows, split node
4. Promote middle key to parent
5. Recursively split parent if needed
6. Create new root if root splits

**Delete Algorithm**:

1. Find leaf node containing key
2. Delete key from leaf
3. If node underflows:
   - Try borrowing from sibling
   - If can't borrow, merge with sibling
   - Update parent (remove key, merge children)
   - Recursively handle parent underflow
4. Delete root if it becomes empty

**Split Leaf Node**:

1. Create new leaf node
2. Move half keys to new node
3. Update leaf chain pointers
4. Promote first key of new node to parent
5. Insert new node pointer in parent

**Split Internal Node**:

1. Create new internal node
2. Move half keys and children to new node
3. Promote middle key to parent
4. Insert new node pointer in parent

**Error Handling**:

- Handle node allocation failures
- Handle disk I/O errors
- Handle corrupted nodes
- Validate tree structure
- Recover from errors (if possible)

**Testing Requirements**:

- Test insert operations (single and bulk)
- Test delete operations
- Test search operations
- Test range queries
- Test node splitting
- Test node merging
- Test tree rebalancing
- Test concurrent operations
- Test persistence
- Test cache behavior

---

### 2.4 LFSX Journaling System - Complete Implementation

#### Requirements

- **Write-Ahead Logging**: Log all changes before applying
- **Transaction Logging**: Group related operations in transactions
- **Checkpointing**: Periodically checkpoint log to filesystem
- **Crash Recovery**: Recover filesystem from log after crash
- **Log Rotation**: Rotate log when it gets too large
- **Atomic Operations**: Ensure atomicity of transactions

#### Implementation Details

**Data Structures**:

```c
enum journal_entry_type {
    JOURNAL_ENTRY_CREATE,
    JOURNAL_ENTRY_DELETE,
    JOURNAL_ENTRY_UPDATE,
    JOURNAL_ENTRY_COMMIT,
    JOURNAL_ENTRY_ABORT
};

struct journal_entry {
    uint64_t sequence;
    enum journal_entry_type type;
    uint32_t transaction_id;
    uint64_t timestamp;
    uint32_t data_size;
    uint8_t data[];
};

struct journal_transaction {
    uint32_t transaction_id;
    uint64_t start_sequence;
    uint64_t end_sequence;
    bool committed;
    bool aborted;
    struct journal_transaction* next;
};

struct journal {
    uint32_t log_block_size;
    uint64_t log_start_block;
    uint64_t log_size_blocks;
    uint64_t current_block;
    uint64_t sequence_number;

    struct journal_transaction* active_transactions;
    struct journal_transaction* completed_transactions;

    uint32_t checkpoint_interval;
    uint64_t last_checkpoint_sequence;

    mutex_t lock;
    struct journal_stats stats;
};
```

**Functions to Implement**:

1. `journal_init(struct journal* journal, uint64_t log_start, uint64_t log_size)` - Initialize journal
2. `journal_start_transaction(struct journal* journal, uint32_t* transaction_id)` - Start transaction
3. `journal_log_entry(struct journal* journal, uint32_t transaction_id, enum journal_entry_type type, void* data, size_t size)` - Log entry
4. `journal_commit_transaction(struct journal* journal, uint32_t transaction_id)` - Commit transaction
5. `journal_abort_transaction(struct journal* journal, uint32_t transaction_id)` - Abort transaction
6. `journal_checkpoint(struct journal* journal)` - Checkpoint journal
7. `journal_recover(struct journal* journal)` - Recover from log
8. `journal_rotate(struct journal* journal)` - Rotate log
9. `journal_get_statistics(struct journal* journal, struct journal_stats* stats)` - Get statistics

**Journaling Algorithm**:

1. Before modifying filesystem, write log entry
2. Write log entry to journal (with sequence number)
3. Flush log to disk (fsync)
4. Apply change to filesystem
5. On commit, write commit entry
6. Periodically checkpoint (write all committed changes to filesystem)
7. Clear log entries after checkpoint

**Recovery Algorithm**:

1. Read journal from disk
2. Find last checkpoint
3. Replay all transactions after checkpoint
4. Apply committed transactions
5. Discard aborted transactions
6. Update filesystem state

**Error Handling**:

- Handle log full condition
- Handle disk I/O errors
- Handle corrupted log entries
- Recover from partial writes
- Validate log integrity

**Testing Requirements**:

- Test transaction logging
- Test commit/abort
- Test checkpointing
- Test crash recovery
- Test log rotation
- Test concurrent transactions
- Test error recovery

---

### 2.5 LFSX Transaction Support - Complete Implementation

#### Requirements

- **ACID Properties**: Atomicity, Consistency, Isolation, Durability
- **MVCC**: Multi-Version Concurrency Control
- **Isolation Levels**: Read committed, repeatable read, serializable
- **Deadlock Detection**: Detect and resolve deadlocks
- **Rollback**: Rollback failed transactions
- **Nested Transactions**: Support nested transactions (savepoints)

#### Implementation Details

**Data Structures**:

```c
enum transaction_state {
    TRANSACTION_ACTIVE,
    TRANSACTION_COMMITTED,
    TRANSACTION_ABORTED
};

enum isolation_level {
    ISOLATION_READ_UNCOMMITTED,
    ISOLATION_READ_COMMITTED,
    ISOLATION_REPEATABLE_READ,
    ISOLATION_SERIALIZABLE
};

struct transaction {
    uint32_t transaction_id;
    enum transaction_state state;
    enum isolation_level isolation;
    uint64_t start_time;
    uint64_t commit_time;

    /* MVCC */
    uint64_t snapshot_version;
    struct version_list* versions;

    /* Locks */
    struct lock_list* locks;

    /* Changes */
    struct change_list* changes;

    /* Nested transactions */
    struct transaction* parent;
    struct transaction* children;

    /* Deadlock detection */
    bool in_deadlock_check;
    struct transaction* wait_for;

    mutex_t lock;
};

struct version {
    uint64_t version_id;
    uint64_t transaction_id;
    uint64_t create_time;
    uint64_t delete_time;
    void* data;
    struct version* next;
    struct version* prev;
};
```

**Functions to Implement**:

1. `transaction_begin(enum isolation_level level, uint32_t* transaction_id)` - Begin transaction
2. `transaction_commit(uint32_t transaction_id)` - Commit transaction
3. `transaction_abort(uint32_t transaction_id)` - Abort transaction
4. `transaction_read(uint32_t transaction_id, uint32_t key, void* value, size_t size)` - Read with isolation
5. `transaction_write(uint32_t transaction_id, uint32_t key, const void* value, size_t size)` - Write
6. `transaction_create_version(uint32_t transaction_id, uint32_t key, void* data)` - Create version
7. `transaction_get_version(uint32_t transaction_id, uint32_t key, uint64_t version_id, void* value)` - Get specific version
8. `transaction_detect_deadlock(void)` - Detect deadlocks
9. `transaction_resolve_deadlock(struct transaction* victim)` - Resolve deadlock
10. `transaction_set_savepoint(uint32_t transaction_id, uint32_t* savepoint_id)` - Set savepoint
11. `transaction_rollback_to_savepoint(uint32_t transaction_id, uint32_t savepoint_id)` - Rollback to savepoint

**MVCC Algorithm**:

1. On write, create new version with transaction ID
2. On read, find appropriate version based on isolation level
3. For read committed: read latest committed version
4. For repeatable read: read version from transaction snapshot
5. For serializable: use strict two-phase locking
6. Mark versions for deletion on commit
7. Garbage collect old versions

**Deadlock Detection**:

1. Build wait-for graph
2. Detect cycles in graph
3. Select victim transaction (youngest, least work)
4. Abort victim transaction
5. Release victim's locks
6. Retry waiting transactions

**Error Handling**:

- Handle deadlock detection
- Handle transaction timeout
- Handle isolation violations
- Handle rollback failures
- Validate transaction state

**Testing Requirements**:

- Test ACID properties
- Test isolation levels
- Test MVCC
- Test deadlock detection
- Test rollback
- Test nested transactions
- Test concurrent transactions
- Test error recovery

---

---

_Document continues with Phase 3-8 specifications..._

---

## Phase 3: Networking Stack (Months 7-9)

### Timeline Overview

| Week  | Milestone                | Deliverables                     |
| ----- | ------------------------ | -------------------------------- |
| 1-2   | Network Driver Framework | Driver model, DMA, IRQ handling  |
| 3-4   | Ethernet Drivers         | Intel e1000, Realtek RTL8139     |
| 5-6   | IPv4/IPv6 Core           | IP stack, routing, fragmentation |
| 7-8   | TCP/UDP Implementation   | Full TCP state machine, UDP      |
| 9-10  | Socket API               | BSD sockets, poll/select         |
| 11-12 | Network Services         | DHCP, DNS, ARP                   |

### 3.1 Network Driver Framework - Complete Implementation

#### Requirements

- **Device Abstraction**: Unified interface for all network devices
- **Ring Buffer Management**: TX/RX ring buffers with DMA
- **Interrupt Coalescing**: Reduce interrupt overhead
- **Scatter-Gather DMA**: Efficient multi-buffer transfers
- **NAPI Support**: New API for high-speed packet processing
- **Checksum Offload**: Hardware checksum validation/generation
- **VLAN Support**: 802.1Q VLAN tagging

#### Implementation Details

**Data Structures**:

```c
struct net_device {
    char name[16];                    /* Interface name (eth0, etc.) */
    uint32_t flags;                   /* IFF_UP, IFF_RUNNING, etc. */
    uint32_t mtu;                     /* Maximum transmission unit */
    uint8_t mac_addr[6];              /* Hardware MAC address */
    uint8_t broadcast_addr[6];        /* Broadcast address */

    /* Device operations */
    struct net_device_ops* ops;

    /* Hardware state */
    void* priv;                       /* Driver private data */
    uintptr_t io_base;               /* I/O base address */
    uintptr_t mmio_base;             /* Memory-mapped I/O base */
    uint32_t irq;                    /* IRQ number */

    /* Ring buffers */
    struct net_ring* tx_ring;
    struct net_ring* rx_ring;

    /* Statistics */
    struct net_stats stats;

    /* Queues */
    struct sk_buff_head tx_queue;
    struct sk_buff_head rx_queue;

    /* State */
    bool link_up;
    uint32_t speed;                  /* Mbps */
    bool full_duplex;

    /* Offload capabilities */
    uint32_t hw_features;            /* HW_CSUM, HW_VLAN, etc. */

    spinlock_t tx_lock;
    spinlock_t rx_lock;
    struct net_device* next;
};

struct net_device_ops {
    int (*init)(struct net_device* dev);
    void (*uninit)(struct net_device* dev);
    int (*open)(struct net_device* dev);
    int (*stop)(struct net_device* dev);
    int (*start_xmit)(struct sk_buff* skb, struct net_device* dev);
    void (*set_rx_mode)(struct net_device* dev);
    int (*set_mac_address)(struct net_device* dev, uint8_t* addr);
    void (*tx_timeout)(struct net_device* dev);
    struct net_stats* (*get_stats)(struct net_device* dev);
    int (*change_mtu)(struct net_device* dev, int new_mtu);
    int (*poll)(struct net_device* dev, int budget);
};

struct net_ring {
    void** buffers;                  /* Array of buffer pointers */
    uintptr_t* dma_addrs;           /* DMA addresses */
    uint32_t* descriptors;          /* Hardware descriptors */
    uintptr_t desc_dma;             /* DMA address of descriptors */
    uint32_t size;                  /* Number of entries */
    uint32_t head;                  /* Producer index */
    uint32_t tail;                  /* Consumer index */
    uint32_t pending;               /* Pending count */
    spinlock_t lock;
};

struct sk_buff {
    uint8_t* data;                  /* Pointer to data */
    uint8_t* head;                  /* Start of buffer */
    uint8_t* tail;                  /* End of data */
    uint8_t* end;                   /* End of buffer */
    uint32_t len;                   /* Length of data */
    uint32_t data_len;              /* Data length */

    /* Network headers */
    uint8_t* mac_header;
    uint8_t* network_header;
    uint8_t* transport_header;

    /* Metadata */
    struct net_device* dev;
    uint16_t protocol;              /* ETH_P_IP, etc. */
    uint16_t vlan_tci;              /* VLAN tag */
    uint32_t priority;              /* QoS priority */

    /* Checksum */
    uint8_t ip_summed;              /* CHECKSUM_NONE, PARTIAL, COMPLETE */
    uint16_t csum;
    uint32_t csum_start;
    uint32_t csum_offset;

    /* Fragmentation */
    bool is_fragment;
    uint16_t frag_offset;
    uint16_t frag_id;

    struct sk_buff* next;
    struct sk_buff* prev;
};
```

**Functions to Implement**:

1. `netdev_alloc(const char* name, size_t priv_size)` - Allocate network device
2. `netdev_register(struct net_device* dev)` - Register device with stack
3. `netdev_unregister(struct net_device* dev)` - Unregister device
4. `netdev_open(struct net_device* dev)` - Open device (bring up)
5. `netdev_close(struct net_device* dev)` - Close device (bring down)
6. `netdev_xmit(struct sk_buff* skb)` - Transmit packet
7. `netdev_rx(struct net_device* dev, struct sk_buff* skb)` - Receive packet
8. `netdev_alloc_skb(struct net_device* dev, uint32_t size)` - Allocate SKB
9. `netdev_free_skb(struct sk_buff* skb)` - Free SKB
10. `ring_buffer_init(struct net_ring* ring, uint32_t size, uint32_t desc_size)` - Init ring
11. `ring_buffer_alloc_entry(struct net_ring* ring)` - Allocate ring entry
12. `ring_buffer_free_entry(struct net_ring* ring, uint32_t index)` - Free entry
13. `netdev_poll(struct net_device* dev)` - Poll for packets (NAPI)
14. `netdev_set_link(struct net_device* dev, bool up, uint32_t speed, bool duplex)` - Set link state

**DMA Management**:

- Allocate physically contiguous memory for ring buffers
- Set up DMA descriptors with proper alignment
- Handle DMA coherency (cache flush/invalidate)
- Map/unmap DMA buffers for each transfer
- Handle scatter-gather lists for large packets

**Interrupt Handling**:

- Register interrupt handler with kernel
- Handle TX completion interrupts
- Handle RX ready interrupts
- Handle link status change interrupts
- Handle error interrupts (underrun, overrun)
- Implement interrupt coalescing

**Error Handling**:

- Handle TX timeout (queue stuck)
- Handle RX overflow (drop packets)
- Handle link down (pause TX)
- Handle DMA errors (reset device)
- Handle out-of-memory (drop packets gracefully)

**Testing Requirements**:

- Test device registration/unregistration
- Test packet transmission
- Test packet reception
- Test ring buffer management
- Test interrupt handling
- Test link state changes
- Test error recovery
- Test performance under load

---

### 3.2 Intel e1000 Ethernet Driver - Complete Implementation

#### Requirements

- **Full Hardware Support**: All e1000 family variants
- **Multi-Queue**: Support for multiple TX/RX queues
- **Jumbo Frames**: Support for MTU up to 9000
- **Checksum Offload**: TCP/UDP/IP checksum offload
- **Interrupt Moderation**: Adaptive interrupt throttling
- **Power Management**: Suspend/resume support

#### Implementation Details

**Data Structures**:

```c
struct e1000_hw {
    uintptr_t hw_addr;              /* Memory-mapped registers */
    uint16_t device_id;
    uint16_t vendor_id;
    uint8_t revision_id;
    uint16_t subsys_device_id;
    uint16_t subsys_vendor_id;

    /* PHY */
    uint32_t phy_id;
    uint32_t phy_type;
    uint32_t phy_addr;

    /* MAC */
    uint8_t mac_addr[6];
    uint8_t perm_mac_addr[6];

    /* Flow control */
    uint32_t fc;
    uint32_t fc_high_water;
    uint32_t fc_low_water;
    uint32_t fc_pause_time;

    /* Offload */
    bool rx_csum_enabled;
    bool tx_csum_enabled;
};

struct e1000_adapter {
    struct net_device* netdev;
    struct e1000_hw hw;

    /* TX */
    struct e1000_tx_ring* tx_ring;
    uint32_t tx_ring_count;

    /* RX */
    struct e1000_rx_ring* rx_ring;
    uint32_t rx_ring_count;

    /* Interrupt */
    uint32_t irq;
    uint32_t itr;                   /* Interrupt throttle rate */
    bool itr_adaptive;

    /* Link */
    bool link_up;
    uint32_t link_speed;
    bool full_duplex;

    /* Statistics */
    struct e1000_stats stats;

    spinlock_t lock;
};

struct e1000_tx_desc {
    uint64_t buffer_addr;
    uint16_t length;
    uint8_t cso;                    /* Checksum offset */
    uint8_t cmd;                    /* Command */
    uint8_t status;                 /* Status */
    uint8_t css;                    /* Checksum start */
    uint16_t special;               /* VLAN tag */
};

struct e1000_rx_desc {
    uint64_t buffer_addr;
    uint16_t length;
    uint16_t checksum;
    uint8_t status;
    uint8_t errors;
    uint16_t special;               /* VLAN tag */
};
```

**Register Definitions**:

```c
/* Control registers */
#define E1000_CTRL      0x00000  /* Device Control */
#define E1000_STATUS    0x00008  /* Device Status */
#define E1000_CTRL_EXT  0x00018  /* Extended Control */
#define E1000_MDIC      0x00020  /* MDI Control */
#define E1000_FCAL      0x00028  /* Flow Control Address Low */
#define E1000_FCAH      0x0002C  /* Flow Control Address High */
#define E1000_FCT       0x00030  /* Flow Control Type */
#define E1000_VET       0x00038  /* VLAN Ether Type */

/* Interrupt registers */
#define E1000_ICR       0x000C0  /* Interrupt Cause Read */
#define E1000_ICS       0x000C8  /* Interrupt Cause Set */
#define E1000_IMS       0x000D0  /* Interrupt Mask Set */
#define E1000_IMC       0x000D8  /* Interrupt Mask Clear */
#define E1000_ITR       0x000C4  /* Interrupt Throttle */

/* Receive registers */
#define E1000_RCTL      0x00100  /* Receive Control */
#define E1000_RDBAL     0x02800  /* RX Descriptor Base Low */
#define E1000_RDBAH     0x02804  /* RX Descriptor Base High */
#define E1000_RDLEN     0x02808  /* RX Descriptor Length */
#define E1000_RDH       0x02810  /* RX Descriptor Head */
#define E1000_RDT       0x02818  /* RX Descriptor Tail */
#define E1000_RDTR      0x02820  /* RX Delay Timer */

/* Transmit registers */
#define E1000_TCTL      0x00400  /* Transmit Control */
#define E1000_TDBAL     0x03800  /* TX Descriptor Base Low */
#define E1000_TDBAH     0x03804  /* TX Descriptor Base High */
#define E1000_TDLEN     0x03808  /* TX Descriptor Length */
#define E1000_TDH       0x03810  /* TX Descriptor Head */
#define E1000_TDT       0x03818  /* TX Descriptor Tail */
#define E1000_TIDV      0x03820  /* TX Interrupt Delay */

/* MAC address registers */
#define E1000_RAL       0x05400  /* Receive Address Low */
#define E1000_RAH       0x05404  /* Receive Address High */

/* Statistic registers */
#define E1000_CRCERRS   0x04000  /* CRC Errors */
#define E1000_GPRC      0x04074  /* Good Packets RX */
#define E1000_GPTC      0x04080  /* Good Packets TX */
```

**Control Bits**:

```c
/* CTRL register bits */
#define E1000_CTRL_FD           0x00000001  /* Full duplex */
#define E1000_CTRL_GIO_MASTER   0x00000004  /* GIO Master Disable */
#define E1000_CTRL_ASDE         0x00000020  /* Auto-speed detection */
#define E1000_CTRL_SLU          0x00000040  /* Set Link Up */
#define E1000_CTRL_ILOS         0x00000080  /* Invert Loss-of-Signal */
#define E1000_CTRL_RST          0x04000000  /* Global reset */
#define E1000_CTRL_VME          0x40000000  /* VLAN Mode Enable */

/* RCTL register bits */
#define E1000_RCTL_EN           0x00000002  /* Receiver Enable */
#define E1000_RCTL_SBP          0x00000004  /* Store Bad Packets */
#define E1000_RCTL_UPE          0x00000008  /* Unicast Promiscuous */
#define E1000_RCTL_MPE          0x00000010  /* Multicast Promiscuous */
#define E1000_RCTL_LPE          0x00000020  /* Long Packet Enable */
#define E1000_RCTL_LBM_NO       0x00000000  /* No Loopback */
#define E1000_RCTL_BAM          0x00008000  /* Broadcast Accept Mode */
#define E1000_RCTL_BSIZE_2048   0x00000000  /* Buffer size 2048 */
#define E1000_RCTL_SECRC        0x04000000  /* Strip Ethernet CRC */

/* TCTL register bits */
#define E1000_TCTL_EN           0x00000002  /* Transmit Enable */
#define E1000_TCTL_PSP          0x00000008  /* Pad Short Packets */
#define E1000_TCTL_CT           0x00000100  /* Collision Threshold */
#define E1000_TCTL_COLD         0x00040000  /* Collision Distance */

/* Interrupt bits */
#define E1000_ICR_TXDW          0x00000001  /* TX Desc Written Back */
#define E1000_ICR_TXQE          0x00000002  /* TX Queue Empty */
#define E1000_ICR_LSC           0x00000004  /* Link Status Change */
#define E1000_ICR_RXDMT0        0x00000010  /* RX Desc Min Threshold */
#define E1000_ICR_RXO           0x00000040  /* RX Overrun */
#define E1000_ICR_RXT0          0x00000080  /* RX Timer Interrupt */
```

**Functions to Implement**:

1. `e1000_probe(uint16_t vendor_id, uint16_t device_id)` - Probe for device
2. `e1000_init(struct net_device* dev)` - Initialize adapter
3. `e1000_reset_hw(struct e1000_adapter* adapter)` - Reset hardware
4. `e1000_init_hw(struct e1000_adapter* adapter)` - Initialize hardware
5. `e1000_setup_tx_resources(struct e1000_adapter* adapter)` - Set up TX ring
6. `e1000_setup_rx_resources(struct e1000_adapter* adapter)` - Set up RX ring
7. `e1000_configure_tx(struct e1000_adapter* adapter)` - Configure TX
8. `e1000_configure_rx(struct e1000_adapter* adapter)` - Configure RX
9. `e1000_open(struct net_device* dev)` - Open device
10. `e1000_close(struct net_device* dev)` - Close device
11. `e1000_xmit_frame(struct sk_buff* skb, struct net_device* dev)` - Transmit frame
12. `e1000_clean_tx_irq(struct e1000_adapter* adapter)` - Clean TX ring
13. `e1000_clean_rx_irq(struct e1000_adapter* adapter)` - Process RX ring
14. `e1000_intr(uint32_t irq, void* data)` - Interrupt handler
15. `e1000_set_multi(struct net_device* dev)` - Set multicast list
16. `e1000_update_stats(struct e1000_adapter* adapter)` - Update statistics
17. `e1000_phy_read(struct e1000_hw* hw, uint32_t reg, uint16_t* data)` - Read PHY
18. `e1000_phy_write(struct e1000_hw* hw, uint32_t reg, uint16_t data)` - Write PHY
19. `e1000_check_link(struct e1000_adapter* adapter)` - Check link status

**Initialization Sequence**:

1. Map PCI BAR0 (memory-mapped registers)
2. Disable interrupts
3. Reset hardware (set RST bit, wait for completion)
4. Read MAC address from EEPROM
5. Initialize PHY
6. Configure flow control
7. Allocate TX/RX ring buffers
8. Set up TX/RX descriptors
9. Configure TX/RX registers
10. Enable interrupts
11. Set link up

**Transmit Algorithm**:

1. Get next available TX descriptor
2. Fill descriptor with buffer address, length, command
3. Set checksum offload if enabled
4. Set VLAN tag if needed
5. Advance TX tail pointer (write TDT register)
6. Device DMA's data from buffer
7. On completion, interrupt handler cleans descriptor

**Receive Algorithm**:

1. Device DMA's received packet to RX buffer
2. Device writes status to RX descriptor
3. Interrupt handler checks for new packets
4. Allocate SKB, copy data or swap buffer
5. Process packet (check status, checksum)
6. Pass SKB to network stack
7. Refill RX ring with new buffer
8. Advance RX tail pointer

**Error Handling**:

- Handle EEPROM read failures
- Handle PHY initialization failures
- Handle TX timeout (reset adapter)
- Handle RX overflow (drop packets)
- Handle link down events
- Handle PCI errors

**Testing Requirements**:

- Test device probe and initialization
- Test TX/RX ring allocation
- Test packet transmission (various sizes)
- Test packet reception
- Test interrupt handling
- Test jumbo frames
- Test checksum offload
- Test VLAN tagging
- Test multicast/broadcast
- Test link status changes
- Test error recovery

---

### 3.3 TCP/IP Protocol Stack - Complete Implementation

#### Requirements

- **IPv4**: Complete IPv4 with fragmentation, options, ICMP
- **IPv6**: Complete IPv6 with extension headers, NDP
- **TCP**: Full TCP state machine, congestion control
- **UDP**: Complete UDP with checksum
- **Routing**: Static and dynamic routing
- **Filtering**: Packet filtering, firewall
- **Quality of Service**: Priority queuing, traffic shaping

#### 3.3.1 IP Layer Implementation

**Data Structures**:

```c
struct ipv4_header {
    uint8_t version_ihl;            /* Version (4 bits) + IHL (4 bits) */
    uint8_t dscp_ecn;               /* DSCP (6 bits) + ECN (2 bits) */
    uint16_t total_length;
    uint16_t identification;
    uint16_t flags_fragment;        /* Flags (3 bits) + Fragment offset */
    uint8_t ttl;
    uint8_t protocol;
    uint16_t header_checksum;
    uint32_t src_addr;
    uint32_t dst_addr;
    /* Options follow (if IHL > 5) */
};

struct ipv6_header {
    uint32_t version_tc_flow;       /* Version (4) + TC (8) + Flow (20) */
    uint16_t payload_length;
    uint8_t next_header;
    uint8_t hop_limit;
    uint8_t src_addr[16];
    uint8_t dst_addr[16];
};

struct route_entry {
    uint32_t destination;           /* Network address */
    uint32_t netmask;              /* Network mask */
    uint32_t gateway;              /* Gateway address */
    struct net_device* dev;        /* Output device */
    uint32_t metric;               /* Route metric */
    uint32_t flags;                /* RTF_UP, RTF_GATEWAY, etc. */
    uint64_t expires;              /* Expiration time */
    struct route_entry* next;
};

struct routing_table {
    struct route_entry* routes;
    uint32_t route_count;
    struct route_entry* default_route;
    rwlock_t lock;
};

/* IP fragment reassembly */
struct ip_frag {
    uint16_t id;
    uint32_t src_addr;
    uint32_t dst_addr;
    uint8_t protocol;
    struct sk_buff* fragments;
    uint32_t total_len;
    uint32_t received_len;
    uint64_t expires;
    struct ip_frag* next;
};

struct ip_stats {
    uint64_t packets_received;
    uint64_t packets_transmitted;
    uint64_t packets_forwarded;
    uint64_t packets_dropped;
    uint64_t fragments_received;
    uint64_t fragments_reassembled;
    uint64_t fragments_created;
    uint64_t checksum_errors;
    uint64_t header_errors;
    uint64_t address_errors;
    uint64_t forwarding_disabled;
    uint64_t no_route;
    uint64_t ttl_exceeded;
};
```

**Functions to Implement**:

1. `ip_init(void)` - Initialize IP stack
2. `ip_rcv(struct sk_buff* skb, struct net_device* dev)` - Receive IP packet
3. `ip_send(struct sk_buff* skb, uint32_t dest, uint8_t protocol)` - Send IP packet
4. `ip_forward(struct sk_buff* skb)` - Forward IP packet
5. `ip_route_input(struct sk_buff* skb)` - Route input
6. `ip_route_output(uint32_t dest, uint32_t src, struct route_entry** rt)` - Find route
7. `ip_fragment(struct sk_buff* skb, uint32_t mtu)` - Fragment packet
8. `ip_defragment(struct sk_buff* skb)` - Reassemble fragments
9. `ip_options_parse(struct sk_buff* skb, struct ip_options* opts)` - Parse options
10. `ip_checksum(void* data, size_t length)` - Compute checksum
11. `ip_verify_checksum(struct ipv4_header* hdr)` - Verify checksum
12. `route_add(uint32_t dest, uint32_t mask, uint32_t gateway, struct net_device* dev)` - Add route
13. `route_del(uint32_t dest, uint32_t mask)` - Delete route
14. `route_lookup(uint32_t dest)` - Lookup route
15. `route_cache_flush(void)` - Flush route cache

**IP Receive Processing**:

1. Validate IP version (must be 4)
2. Verify header length (>= 20 bytes)
3. Verify total length <= packet length
4. Verify header checksum
5. Check destination address
6. If for us: pass to transport layer
7. If not for us: forward (if routing enabled)
8. Handle options if present
9. If fragment: pass to reassembly
10. Update statistics

**IP Fragmentation Algorithm**:

1. Check if packet > MTU
2. If DF flag set: return error
3. Calculate fragment size (multiple of 8)
4. Split packet into fragments
5. Set fragment offset and MF flag
6. Copy IP header to each fragment
7. Recalculate checksum for each fragment
8. Queue fragments for transmission

**IP Reassembly Algorithm**:

1. Check if packet is a fragment
2. Look up reassembly queue by (src, dst, id, protocol)
3. If not found: create new queue
4. Insert fragment in order
5. Check for overlapping fragments (security)
6. If all fragments received: reassemble
7. Timeout incomplete reassembly (60 seconds)
8. Return complete packet or continue waiting

---

#### 3.3.2 TCP Implementation

**Data Structures**:

```c
enum tcp_state {
    TCP_CLOSED,
    TCP_LISTEN,
    TCP_SYN_SENT,
    TCP_SYN_RECEIVED,
    TCP_ESTABLISHED,
    TCP_FIN_WAIT_1,
    TCP_FIN_WAIT_2,
    TCP_CLOSE_WAIT,
    TCP_CLOSING,
    TCP_LAST_ACK,
    TCP_TIME_WAIT
};

struct tcp_header {
    uint16_t src_port;
    uint16_t dst_port;
    uint32_t seq_num;
    uint32_t ack_num;
    uint8_t data_offset;            /* (4 bits) + reserved (4 bits) */
    uint8_t flags;                  /* URG, ACK, PSH, RST, SYN, FIN */
    uint16_t window;
    uint16_t checksum;
    uint16_t urgent_ptr;
    /* Options follow */
};

struct tcp_options {
    bool mss_present;
    uint16_t mss;
    bool wscale_present;
    uint8_t wscale;
    bool sack_permitted;
    bool timestamps_present;
    uint32_t tsval;
    uint32_t tsecr;
};

struct tcp_sock {
    /* Socket base */
    struct socket* sock;

    /* Addresses */
    uint32_t local_addr;
    uint32_t remote_addr;
    uint16_t local_port;
    uint16_t remote_port;

    /* State */
    enum tcp_state state;

    /* Sequence numbers */
    uint32_t snd_una;               /* First unacknowledged byte */
    uint32_t snd_nxt;               /* Next byte to send */
    uint32_t snd_wnd;               /* Send window */
    uint32_t snd_wl1;               /* Segment seq for last window update */
    uint32_t snd_wl2;               /* Segment ack for last window update */
    uint32_t iss;                   /* Initial send sequence */

    uint32_t rcv_nxt;               /* Next byte expected */
    uint32_t rcv_wnd;               /* Receive window */
    uint32_t irs;                   /* Initial receive sequence */

    /* Congestion control */
    uint32_t cwnd;                  /* Congestion window */
    uint32_t ssthresh;              /* Slow start threshold */
    uint32_t rto;                   /* Retransmission timeout (ms) */
    uint32_t srtt;                  /* Smoothed RTT (us) */
    uint32_t rttvar;                /* RTT variance (us) */

    /* Options */
    uint16_t mss;                   /* Max segment size */
    uint8_t snd_wscale;            /* Send window scale */
    uint8_t rcv_wscale;            /* Receive window scale */
    bool sack_enabled;
    bool timestamps_enabled;

    /* Buffers */
    struct sk_buff_head send_queue; /* Outgoing data */
    struct sk_buff_head retransmit_queue; /* Unacked segments */
    struct sk_buff_head receive_queue; /* Incoming data */
    struct sk_buff_head ooo_queue;  /* Out-of-order segments */

    /* Timers */
    struct timer retransmit_timer;
    struct timer delayed_ack_timer;
    struct timer keepalive_timer;
    struct timer time_wait_timer;

    /* Statistics */
    uint64_t bytes_sent;
    uint64_t bytes_received;
    uint64_t retransmits;

    /* Waiters */
    struct wait_queue* accept_queue;
    struct wait_queue* read_wait;
    struct wait_queue* write_wait;

    /* Connection queue (for listen sockets) */
    struct tcp_sock* pending_connections;
    struct tcp_sock* established_connections;
    uint32_t backlog;
    uint32_t max_backlog;

    struct tcp_sock* next;
    struct tcp_sock* prev;
    spinlock_t lock;
};

/* TCP flag definitions */
#define TCP_FLAG_FIN    0x01
#define TCP_FLAG_SYN    0x02
#define TCP_FLAG_RST    0x04
#define TCP_FLAG_PSH    0x08
#define TCP_FLAG_ACK    0x10
#define TCP_FLAG_URG    0x20
```

**Functions to Implement**:

1. `tcp_init(void)` - Initialize TCP
2. `tcp_rcv(struct sk_buff* skb)` - Receive TCP segment
3. `tcp_transmit_skb(struct tcp_sock* sk, struct sk_buff* skb, bool clone)` - Transmit
4. `tcp_send_syn(struct tcp_sock* sk)` - Send SYN
5. `tcp_send_synack(struct tcp_sock* sk)` - Send SYN-ACK
6. `tcp_send_ack(struct tcp_sock* sk)` - Send ACK
7. `tcp_send_fin(struct tcp_sock* sk)` - Send FIN
8. `tcp_send_rst(struct tcp_sock* sk)` - Send RST
9. `tcp_connect(struct tcp_sock* sk, uint32_t addr, uint16_t port)` - Active open
10. `tcp_listen(struct tcp_sock* sk, int backlog)` - Passive open
11. `tcp_accept(struct tcp_sock* sk, struct tcp_sock** newsk)` - Accept connection
12. `tcp_close(struct tcp_sock* sk)` - Close connection
13. `tcp_sendmsg(struct tcp_sock* sk, void* data, size_t len)` - Send data
14. `tcp_recvmsg(struct tcp_sock* sk, void* buf, size_t len)` - Receive data
15. `tcp_retransmit(struct tcp_sock* sk)` - Retransmit unacked
16. `tcp_fast_retransmit(struct tcp_sock* sk)` - Fast retransmit
17. `tcp_congestion_control(struct tcp_sock* sk, bool loss)` - Adjust cwnd
18. `tcp_parse_options(struct sk_buff* skb, struct tcp_options* opts)` - Parse options
19. `tcp_checksum(struct sk_buff* skb)` - Compute checksum
20. `tcp_state_machine(struct tcp_sock* sk, struct sk_buff* skb)` - Process state

**TCP State Machine**:

```
State transitions:

CLOSED:
  - connect() -> send SYN, go to SYN_SENT
  - listen() -> go to LISTEN

LISTEN:
  - recv SYN -> send SYN-ACK, go to SYN_RECEIVED
  - close() -> go to CLOSED

SYN_SENT:
  - recv SYN-ACK -> send ACK, go to ESTABLISHED
  - recv SYN -> send SYN-ACK, go to SYN_RECEIVED (simultaneous open)
  - timeout -> retransmit SYN or go to CLOSED

SYN_RECEIVED:
  - recv ACK -> go to ESTABLISHED
  - recv RST -> go to LISTEN or CLOSED
  - close() -> send FIN, go to FIN_WAIT_1

ESTABLISHED:
  - recv FIN -> send ACK, go to CLOSE_WAIT
  - close() -> send FIN, go to FIN_WAIT_1

FIN_WAIT_1:
  - recv ACK -> go to FIN_WAIT_2
  - recv FIN -> send ACK, go to CLOSING
  - recv FIN-ACK -> send ACK, go to TIME_WAIT

FIN_WAIT_2:
  - recv FIN -> send ACK, go to TIME_WAIT

CLOSING:
  - recv ACK -> go to TIME_WAIT

CLOSE_WAIT:
  - close() -> send FIN, go to LAST_ACK

LAST_ACK:
  - recv ACK -> go to CLOSED

TIME_WAIT:
  - 2*MSL timeout -> go to CLOSED
```

**Congestion Control (Reno)**:

1. **Slow Start**: cwnd starts at 1 MSS, doubles each RTT
2. **Congestion Avoidance**: cwnd increases by 1 MSS per RTT
3. **Fast Retransmit**: On 3 duplicate ACKs, retransmit immediately
4. **Fast Recovery**: ssthresh = cwnd/2, cwnd = ssthresh + 3 MSS
5. **Timeout**: ssthresh = cwnd/2, cwnd = 1 MSS, start slow start

**Retransmission Timer**:

- Initial RTO = 1 second
- On ACK: update SRTT and RTTVAR
  - SRTT = (1-α) _ SRTT + α _ RTT (α = 1/8)
  - RTTVAR = (1-β) _ RTTVAR + β _ |SRTT - RTT| (β = 1/4)
  - RTO = SRTT + 4 \* RTTVAR
- On timeout: RTO = 2 \* RTO (exponential backoff)
- Minimum RTO = 200ms, Maximum RTO = 120 seconds

**Testing Requirements**:

- Test connection establishment (3-way handshake)
- Test data transfer (small and large)
- Test connection termination (4-way handshake)
- Test simultaneous open/close
- Test retransmission
- Test congestion control
- Test window scaling
- Test MSS negotiation
- Test out-of-order segments
- Test duplicate segments
- Test RST handling
- Test all state transitions

---

### 3.4 Socket API - Complete Implementation

#### Requirements

- **BSD Socket API**: Complete socket interface
- **Socket Types**: Stream (TCP), datagram (UDP), raw
- **Blocking/Non-blocking**: Both modes supported
- **Multiplexing**: select(), poll(), epoll()
- **Socket Options**: SO_REUSEADDR, SO_KEEPALIVE, etc.
- **Ancillary Data**: sendmsg/recvmsg with control data

#### Implementation Details

**Data Structures**:

```c
enum socket_type {
    SOCK_STREAM = 1,
    SOCK_DGRAM = 2,
    SOCK_RAW = 3
};

enum socket_state {
    SS_UNCONNECTED,
    SS_CONNECTING,
    SS_CONNECTED,
    SS_DISCONNECTING
};

struct socket {
    int fd;
    int domain;                     /* AF_INET, AF_INET6 */
    enum socket_type type;
    int protocol;
    enum socket_state state;
    uint32_t flags;                 /* O_NONBLOCK, etc. */

    /* Protocol-specific socket */
    void* sk;                       /* tcp_sock, udp_sock, etc. */

    /* Operations */
    struct socket_ops* ops;

    /* Buffers */
    struct sk_buff_head recv_queue;
    struct sk_buff_head send_queue;

    /* Options */
    int rcvbuf_size;
    int sndbuf_size;
    int rcvlowat;
    int sndlowat;
    struct timeval rcvtimeo;
    struct timeval sndtimeo;
    bool reuseaddr;
    bool keepalive;
    bool nodelay;                   /* TCP_NODELAY */

    /* Wait queues */
    struct wait_queue wait_read;
    struct wait_queue wait_write;
    struct wait_queue wait_except;

    /* Error */
    int error;

    /* Reference count */
    atomic_t refcount;

    spinlock_t lock;
};

struct socket_ops {
    int (*bind)(struct socket* sock, struct sockaddr* addr, socklen_t len);
    int (*connect)(struct socket* sock, struct sockaddr* addr, socklen_t len);
    int (*listen)(struct socket* sock, int backlog);
    int (*accept)(struct socket* sock, struct socket** newsock, struct sockaddr* addr, socklen_t* len);
    int (*send)(struct socket* sock, void* buf, size_t len, int flags);
    int (*recv)(struct socket* sock, void* buf, size_t len, int flags);
    int (*sendto)(struct socket* sock, void* buf, size_t len, int flags, struct sockaddr* addr, socklen_t len);
    int (*recvfrom)(struct socket* sock, void* buf, size_t len, int flags, struct sockaddr* addr, socklen_t* len);
    int (*shutdown)(struct socket* sock, int how);
    int (*close)(struct socket* sock);
    int (*getsockopt)(struct socket* sock, int level, int optname, void* optval, socklen_t* optlen);
    int (*setsockopt)(struct socket* sock, int level, int optname, const void* optval, socklen_t optlen);
    int (*poll)(struct socket* sock, struct poll_table* wait);
};

struct sockaddr_in {
    uint16_t sin_family;            /* AF_INET */
    uint16_t sin_port;              /* Port (network byte order) */
    uint32_t sin_addr;              /* IP address */
    uint8_t sin_zero[8];
};
```

**Functions to Implement**:

1. `socket(int domain, int type, int protocol)` - Create socket
2. `bind(int sockfd, struct sockaddr* addr, socklen_t len)` - Bind address
3. `listen(int sockfd, int backlog)` - Start listening
4. `accept(int sockfd, struct sockaddr* addr, socklen_t* len)` - Accept connection
5. `connect(int sockfd, struct sockaddr* addr, socklen_t len)` - Connect to peer
6. `send(int sockfd, void* buf, size_t len, int flags)` - Send data
7. `recv(int sockfd, void* buf, size_t len, int flags)` - Receive data
8. `sendto(int sockfd, void* buf, size_t len, int flags, struct sockaddr* addr, socklen_t len)` - Send datagram
9. `recvfrom(int sockfd, void* buf, size_t len, int flags, struct sockaddr* addr, socklen_t* len)` - Receive datagram
10. `sendmsg(int sockfd, struct msghdr* msg, int flags)` - Send with ancillary
11. `recvmsg(int sockfd, struct msghdr* msg, int flags)` - Receive with ancillary
12. `shutdown(int sockfd, int how)` - Shutdown connection
13. `close(int sockfd)` - Close socket
14. `getsockopt(int sockfd, int level, int optname, void* optval, socklen_t* optlen)` - Get option
15. `setsockopt(int sockfd, int level, int optname, void* optval, socklen_t optlen)` - Set option
16. `getsockname(int sockfd, struct sockaddr* addr, socklen_t* len)` - Get local address
17. `getpeername(int sockfd, struct sockaddr* addr, socklen_t* len)` - Get peer address
18. `select(int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval* timeout)` - Multiplex
19. `poll(struct pollfd* fds, nfds_t nfds, int timeout)` - Multiplex
20. `socketpair(int domain, int type, int protocol, int sv[2])` - Create socket pair

**Error Handling**:

- Return appropriate errno values
- Handle interrupted system calls (EINTR)
- Handle non-blocking operations (EAGAIN, EWOULDBLOCK)
- Handle connection errors (ECONNREFUSED, ECONNRESET)
- Handle address errors (EADDRINUSE, EADDRNOTAVAIL)

**Testing Requirements**:

- Test all socket syscalls
- Test TCP client/server
- Test UDP client/server
- Test non-blocking operations
- Test select/poll
- Test socket options
- Test error conditions
- Test concurrent connections

---

_Phase 3 continues with DHCP, DNS, ARP implementations..._

### 3.5 Network Services - Complete Implementation

#### 3.5.1 DHCP Client

**Data Structures**:

```c
enum dhcp_state {
    DHCP_INIT,
    DHCP_SELECTING,
    DHCP_REQUESTING,
    DHCP_BOUND,
    DHCP_RENEWING,
    DHCP_REBINDING
};

struct dhcp_lease {
    uint32_t client_ip;
    uint32_t server_ip;
    uint32_t gateway;
    uint32_t netmask;
    uint32_t dns_servers[4];
    uint32_t lease_time;
    uint32_t renewal_time;
    uint32_t rebind_time;
    uint64_t obtained_at;
    char domain[256];
};

struct dhcp_client {
    struct net_device* dev;
    enum dhcp_state state;
    struct dhcp_lease lease;
    uint32_t xid;
    struct timer renewal_timer;
    struct timer rebind_timer;
    struct timer lease_timer;
    spinlock_t lock;
};
```

**Functions to Implement**:

1. `dhcp_client_init(struct net_device* dev)` - Initialize DHCP client
2. `dhcp_discover(struct dhcp_client* client)` - Send DISCOVER
3. `dhcp_request(struct dhcp_client* client, uint32_t server_ip)` - Send REQUEST
4. `dhcp_release(struct dhcp_client* client)` - Release lease
5. `dhcp_renew(struct dhcp_client* client)` - Renew lease
6. `dhcp_process_offer(struct dhcp_client* client, struct sk_buff* skb)` - Process OFFER
7. `dhcp_process_ack(struct dhcp_client* client, struct sk_buff* skb)` - Process ACK
8. `dhcp_apply_lease(struct dhcp_client* client)` - Apply configuration

#### 3.5.2 DNS Resolver

**Data Structures**:

```c
struct dns_header {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
};

struct dns_cache_entry {
    char name[256];
    uint16_t type;
    uint32_t ttl;
    uint64_t expires;
    union {
        uint32_t ipv4;
        uint8_t ipv6[16];
        char cname[256];
    } data;
    struct dns_cache_entry* next;
};

struct dns_resolver {
    uint32_t servers[4];
    uint32_t server_count;
    struct dns_cache_entry* cache;
    uint32_t cache_size;
    uint16_t next_id;
    struct socket* socket;
    mutex_t lock;
};
```

**Functions to Implement**:

1. `dns_init(void)` - Initialize DNS resolver
2. `dns_resolve(const char* name, uint32_t* addr)` - Resolve hostname to IPv4
3. `dns_resolve6(const char* name, uint8_t* addr)` - Resolve hostname to IPv6
4. `dns_reverse_lookup(uint32_t addr, char* name)` - Reverse DNS
5. `dns_send_query(uint16_t id, const char* name, uint16_t type)` - Send query
6. `dns_process_response(struct sk_buff* skb)` - Process response
7. `dns_cache_add(const char* name, uint16_t type, void* data, uint32_t ttl)` - Add to cache
8. `dns_cache_lookup(const char* name, uint16_t type)` - Lookup in cache
9. `dns_cache_expire(void)` - Expire old entries

#### 3.5.3 ARP Protocol

**Data Structures**:

```c
struct arp_header {
    uint16_t hw_type;
    uint16_t proto_type;
    uint8_t hw_len;
    uint8_t proto_len;
    uint16_t opcode;
    uint8_t sender_hw[6];
    uint32_t sender_ip;
    uint8_t target_hw[6];
    uint32_t target_ip;
};

struct arp_entry {
    uint32_t ip_addr;
    uint8_t hw_addr[6];
    uint64_t expires;
    enum { ARP_INCOMPLETE, ARP_REACHABLE, ARP_STALE, ARP_DELAY, ARP_PROBE } state;
    struct sk_buff_head pending;
    struct net_device* dev;
    struct arp_entry* next;
};

struct arp_table {
    struct arp_entry* entries;
    uint32_t entry_count;
    rwlock_t lock;
};
```

**Functions to Implement**:

1. `arp_init(void)` - Initialize ARP
2. `arp_lookup(uint32_t ip_addr, uint8_t* hw_addr)` - Lookup MAC for IP
3. `arp_resolve(uint32_t ip_addr, struct net_device* dev, struct sk_buff* skb)` - Resolve
4. `arp_send_request(struct net_device* dev, uint32_t target_ip)` - Send request
5. `arp_send_reply(struct net_device* dev, uint32_t target_ip, uint8_t* target_hw)` - Send reply
6. `arp_rcv(struct sk_buff* skb, struct net_device* dev)` - Receive ARP packet
7. `arp_add_entry(uint32_t ip, uint8_t* hw, struct net_device* dev)` - Add entry
8. `arp_delete_entry(uint32_t ip)` - Delete entry
9. `arp_expire_entries(void)` - Expire old entries

---

## Phase 4: Security Subsystem (Months 10-12)

### Timeline Overview

| Week  | Milestone              | Deliverables                           |
| ----- | ---------------------- | -------------------------------------- |
| 1-2   | User/Group Management  | UID/GID, password, permissions         |
| 3-4   | Access Control Lists   | POSIX ACLs, extended attributes        |
| 5-6   | Capability System      | Fine-grained privileges                |
| 7-8   | Secure Boot            | Chain of trust, signature verification |
| 9-10  | Cryptographic Services | AES, SHA, RSA, ECDSA                   |
| 11-12 | Security Auditing      | Audit log, intrusion detection         |

### 4.1 User and Group Management - Complete Implementation

#### Requirements

- **User Database**: Comprehensive user account management
- **Group Database**: Group membership and management
- **Password Hashing**: Secure password storage (Argon2, bcrypt)
- **Session Management**: Login sessions, sudo/su
- **PAM-like Framework**: Pluggable authentication modules
- **Account Policies**: Password expiry, lockout, complexity

#### Implementation Details

**Data Structures**:

```c
struct user_account {
    uid_t uid;
    gid_t primary_gid;
    char username[64];
    char password_hash[128];
    char salt[32];
    char full_name[256];
    char home_dir[256];
    char shell[256];

    /* Account status */
    bool enabled;
    bool locked;
    uint64_t created_at;
    uint64_t last_login;
    uint32_t failed_logins;
    uint64_t lockout_until;

    /* Password policy */
    uint64_t password_changed;
    uint32_t password_max_age;
    uint32_t password_min_age;
    uint32_t password_warn_days;

    /* Supplementary groups */
    gid_t groups[32];
    uint32_t group_count;

    struct user_account* next;
};

struct group_entry {
    gid_t gid;
    char name[64];
    uid_t* members;
    uint32_t member_count;
    struct group_entry* next;
};

struct user_session {
    uint32_t session_id;
    uid_t uid;
    pid_t login_pid;
    char tty[32];
    uint64_t login_time;
    uint32_t remote_addr;
    bool active;
    struct user_session* next;
};

struct user_database {
    struct user_account* users;
    struct group_entry* groups;
    struct user_session* sessions;
    uint32_t user_count;
    uint32_t group_count;
    uid_t next_uid;
    gid_t next_gid;
    rwlock_t lock;
};
```

**Functions to Implement**:

1. `user_db_init(void)` - Initialize user database
2. `user_create(const char* username, const char* password, uid_t uid, gid_t gid)` - Create user
3. `user_delete(uid_t uid)` - Delete user
4. `user_modify(uid_t uid, struct user_account* changes)` - Modify user
5. `user_authenticate(const char* username, const char* password)` - Authenticate
6. `user_set_password(uid_t uid, const char* password)` - Set password
7. `user_check_password_policy(const char* password)` - Validate password
8. `user_get_by_uid(uid_t uid)` - Get user by UID
9. `user_get_by_name(const char* username)` - Get user by name
10. `group_create(const char* name, gid_t gid)` - Create group
11. `group_delete(gid_t gid)` - Delete group
12. `group_add_member(gid_t gid, uid_t uid)` - Add member to group
13. `group_remove_member(gid_t gid, uid_t uid)` - Remove member
14. `session_create(uid_t uid, pid_t pid, const char* tty)` - Create session
15. `session_destroy(uint32_t session_id)` - Destroy session
16. `session_get_current(void)` - Get current session

**Password Hashing (Argon2)**:

```c
struct argon2_params {
    uint32_t time_cost;       /* Number of iterations */
    uint32_t memory_cost;     /* Memory in KiB */
    uint32_t parallelism;     /* Number of threads */
    uint8_t salt[16];
    uint32_t hash_length;
};

int argon2_hash(const char* password, size_t password_len,
                struct argon2_params* params, uint8_t* hash);
int argon2_verify(const char* password, size_t password_len,
                  const uint8_t* hash, struct argon2_params* params);
```

**Testing Requirements**:

- Test user creation/deletion
- Test authentication (success/failure)
- Test password hashing strength
- Test account lockout
- Test group membership
- Test session management
- Test password policy enforcement

---

### 4.2 Access Control Lists - Complete Implementation

#### Requirements

- **POSIX ACLs**: Full POSIX.1e ACL support
- **Extended Attributes**: User, trusted, security namespaces
- **Default ACLs**: Inheritance for directories
- **ACL Masks**: Effective permissions calculation
- **ACL Caching**: Performance optimization

#### Implementation Details

**Data Structures**:

```c
enum acl_tag {
    ACL_USER_OBJ,       /* Owner */
    ACL_USER,           /* Named user */
    ACL_GROUP_OBJ,      /* Owning group */
    ACL_GROUP,          /* Named group */
    ACL_MASK,           /* Mask */
    ACL_OTHER           /* Others */
};

struct acl_entry {
    enum acl_tag tag;
    uid_t uid;          /* For ACL_USER */
    gid_t gid;          /* For ACL_GROUP */
    uint16_t permissions; /* rwx */
    struct acl_entry* next;
};

struct acl {
    uint32_t count;
    struct acl_entry* entries;
    uint16_t mask;
    bool has_mask;
};

struct xattr {
    char name[256];
    void* value;
    size_t size;
    struct xattr* next;
};

struct xattr_list {
    struct xattr* user;     /* user.* namespace */
    struct xattr* trusted;  /* trusted.* namespace */
    struct xattr* security; /* security.* namespace */
    struct xattr* system;   /* system.* namespace */
    rwlock_t lock;
};
```

**Functions to Implement**:

1. `acl_init(struct acl* acl)` - Initialize ACL
2. `acl_add_entry(struct acl* acl, enum acl_tag tag, uint32_t id, uint16_t perms)` - Add entry
3. `acl_remove_entry(struct acl* acl, enum acl_tag tag, uint32_t id)` - Remove entry
4. `acl_check_permission(struct acl* acl, uid_t uid, gid_t gid, uint16_t requested)` - Check access
5. `acl_calculate_mask(struct acl* acl)` - Calculate mask
6. `acl_apply_default(struct acl* parent_acl, struct acl* child_acl)` - Apply default ACL
7. `acl_to_disk(struct acl* acl, void* buffer, size_t* size)` - Serialize
8. `acl_from_disk(struct acl* acl, void* buffer, size_t size)` - Deserialize
9. `xattr_get(struct inode* inode, const char* name, void* value, size_t size)` - Get xattr
10. `xattr_set(struct inode* inode, const char* name, void* value, size_t size, int flags)` - Set xattr
11. `xattr_remove(struct inode* inode, const char* name)` - Remove xattr
12. `xattr_list(struct inode* inode, char* list, size_t size)` - List xattrs

**Permission Check Algorithm**:

1. If process is root (uid=0): allow all
2. If file owner matches: use ACL_USER_OBJ permissions
3. If named user entry exists: use entry permissions & mask
4. If group matches owning group: check ACL_GROUP_OBJ & mask
5. If named group entry exists: check entry permissions & mask
6. Use ACL_OTHER permissions
7. Apply mask to all except USER_OBJ and OTHER

---

### 4.3 Capability System - Complete Implementation

#### Requirements

- **POSIX Capabilities**: Fine-grained privilege separation
- **Capability Sets**: Permitted, effective, inheritable, bounding
- **Capability Inheritance**: exec() handling
- **Capability-aware Programs**: Support for cap_setuid, etc.
- **Capability Database**: Store capabilities in filesystem

#### Implementation Details

**Data Structures**:

```c
/* Capability bits */
#define CAP_CHOWN           0
#define CAP_DAC_OVERRIDE    1
#define CAP_DAC_READ_SEARCH 2
#define CAP_FOWNER          3
#define CAP_FSETID          4
#define CAP_KILL            5
#define CAP_SETGID          6
#define CAP_SETUID          7
#define CAP_SETPCAP         8
#define CAP_LINUX_IMMUTABLE 9
#define CAP_NET_BIND_SERVICE 10
#define CAP_NET_BROADCAST   11
#define CAP_NET_ADMIN       12
#define CAP_NET_RAW         13
#define CAP_IPC_LOCK        14
#define CAP_IPC_OWNER       15
#define CAP_SYS_MODULE      16
#define CAP_SYS_RAWIO       17
#define CAP_SYS_CHROOT      18
#define CAP_SYS_PTRACE      19
#define CAP_SYS_PACCT       20
#define CAP_SYS_ADMIN       21
#define CAP_SYS_BOOT        22
#define CAP_SYS_NICE        23
#define CAP_SYS_RESOURCE    24
#define CAP_SYS_TIME        25
#define CAP_SYS_TTY_CONFIG  26
#define CAP_MKNOD           27
#define CAP_LEASE           28
#define CAP_AUDIT_WRITE     29
#define CAP_AUDIT_CONTROL   30
#define CAP_SETFCAP         31
#define CAP_MAX             32

typedef uint64_t cap_t[2];  /* 64 capabilities per set */

struct capability_set {
    cap_t permitted;    /* Caps that can be used */
    cap_t effective;    /* Caps currently active */
    cap_t inheritable;  /* Caps passed to exec'd process */
    cap_t bounding;     /* Maximum permitted caps */
    cap_t ambient;      /* Retained across unprivileged exec */
};

struct file_capabilities {
    uint32_t magic;
    uint32_t version;
    cap_t permitted;
    cap_t inheritable;
    bool effective;     /* Set-UID style effective bit */
};
```

**Functions to Implement**:

1. `cap_init(struct capability_set* caps)` - Initialize capability set
2. `cap_set(cap_t caps, int cap)` - Set capability
3. `cap_clear(cap_t caps, int cap)` - Clear capability
4. `cap_check(cap_t caps, int cap)` - Check capability
5. `cap_capable(struct process* proc, int cap)` - Check if process has cap
6. `cap_raise(struct process* proc, int cap)` - Raise capability
7. `cap_drop(struct process* proc, int cap)` - Drop capability
8. `cap_get_proc(struct process* proc, struct capability_set* caps)` - Get process caps
9. `cap_set_proc(struct process* proc, struct capability_set* caps)` - Set process caps
10. `cap_get_file(struct inode* inode, struct file_capabilities* fcaps)` - Get file caps
11. `cap_set_file(struct inode* inode, struct file_capabilities* fcaps)` - Set file caps
12. `cap_transform_on_exec(struct process* proc, struct file_capabilities* fcaps)` - Transform on exec

**Capability Transformation on exec()**:

```
P'(permitted) = (P(inheritable) & F(inheritable)) | (F(permitted) & cap_bset)
P'(effective) = F(effective) ? P'(permitted) : P(ambient)
P'(inheritable) = P(inheritable)
P'(ambient) = (P(ambient) & P'(permitted) & P'(inheritable))
```

---

### 4.4 Cryptographic Services - Complete Implementation

#### Requirements

- **Symmetric Encryption**: AES-128/192/256, ChaCha20
- **Hash Functions**: SHA-256, SHA-512, SHA-3, BLAKE2
- **Asymmetric Encryption**: RSA, ECDSA, Ed25519
- **Key Management**: Key generation, storage, derivation
- **Random Number Generation**: Hardware RNG, CSPRNG
- **TLS Support**: TLS 1.2/1.3 for secure communications

#### Implementation Details

**Data Structures**:

```c
/* AES Context */
struct aes_context {
    uint8_t round_keys[240];    /* Expanded key */
    uint32_t nr;                /* Number of rounds */
};

/* SHA-256 Context */
struct sha256_context {
    uint32_t state[8];
    uint64_t bit_count;
    uint8_t buffer[64];
    uint32_t buffer_len;
};

/* RSA Key */
struct rsa_key {
    uint8_t* n;         /* Modulus */
    size_t n_len;
    uint8_t* e;         /* Public exponent */
    size_t e_len;
    uint8_t* d;         /* Private exponent */
    size_t d_len;
    uint8_t* p;         /* Prime 1 */
    size_t p_len;
    uint8_t* q;         /* Prime 2 */
    size_t q_len;
    bool has_private;
};

/* ECC Point */
struct ecc_point {
    uint8_t x[66];      /* Up to P-521 */
    uint8_t y[66];
    size_t coord_len;
};

/* ECDSA Key */
struct ecdsa_key {
    int curve;          /* SECP256R1, SECP384R1, SECP521R1 */
    struct ecc_point public_key;
    uint8_t private_key[66];
    bool has_private;
};

/* Crypto operation */
struct crypto_op {
    enum {
        CRYPTO_ENCRYPT,
        CRYPTO_DECRYPT,
        CRYPTO_SIGN,
        CRYPTO_VERIFY,
        CRYPTO_HASH,
        CRYPTO_MAC
    } type;
    enum {
        ALG_AES_CBC,
        ALG_AES_GCM,
        ALG_CHACHA20_POLY1305,
        ALG_RSA_OAEP,
        ALG_RSA_PSS,
        ALG_ECDSA_SHA256,
        ALG_ED25519,
        ALG_SHA256,
        ALG_SHA512,
        ALG_HMAC_SHA256
    } algorithm;
    void* key;
    size_t key_len;
    uint8_t* iv;
    size_t iv_len;
    uint8_t* aad;
    size_t aad_len;
};
```

**Functions to Implement**:

**AES**:

1. `aes_init(struct aes_context* ctx, const uint8_t* key, size_t key_len)` - Initialize
2. `aes_encrypt_block(struct aes_context* ctx, const uint8_t* in, uint8_t* out)` - Encrypt block
3. `aes_decrypt_block(struct aes_context* ctx, const uint8_t* in, uint8_t* out)` - Decrypt block
4. `aes_cbc_encrypt(struct aes_context* ctx, const uint8_t* iv, const uint8_t* in, uint8_t* out, size_t len)` - CBC encrypt
5. `aes_cbc_decrypt(struct aes_context* ctx, const uint8_t* iv, const uint8_t* in, uint8_t* out, size_t len)` - CBC decrypt
6. `aes_gcm_encrypt(struct aes_context* ctx, const uint8_t* iv, const uint8_t* aad, size_t aad_len, const uint8_t* in, uint8_t* out, size_t len, uint8_t* tag)` - GCM encrypt
7. `aes_gcm_decrypt(struct aes_context* ctx, const uint8_t* iv, const uint8_t* aad, size_t aad_len, const uint8_t* in, uint8_t* out, size_t len, const uint8_t* tag)` - GCM decrypt

**SHA-256**:

1. `sha256_init(struct sha256_context* ctx)` - Initialize
2. `sha256_update(struct sha256_context* ctx, const uint8_t* data, size_t len)` - Update
3. `sha256_final(struct sha256_context* ctx, uint8_t* hash)` - Finalize
4. `sha256(const uint8_t* data, size_t len, uint8_t* hash)` - One-shot hash

**RSA**:

1. `rsa_generate_key(struct rsa_key* key, size_t bits)` - Generate key pair
2. `rsa_encrypt(struct rsa_key* key, const uint8_t* in, size_t in_len, uint8_t* out, size_t* out_len)` - Encrypt
3. `rsa_decrypt(struct rsa_key* key, const uint8_t* in, size_t in_len, uint8_t* out, size_t* out_len)` - Decrypt
4. `rsa_sign(struct rsa_key* key, const uint8_t* hash, size_t hash_len, uint8_t* sig, size_t* sig_len)` - Sign
5. `rsa_verify(struct rsa_key* key, const uint8_t* hash, size_t hash_len, const uint8_t* sig, size_t sig_len)` - Verify

**ECDSA**:

1. `ecdsa_generate_key(struct ecdsa_key* key, int curve)` - Generate key
2. `ecdsa_sign(struct ecdsa_key* key, const uint8_t* hash, size_t hash_len, uint8_t* sig, size_t* sig_len)` - Sign
3. `ecdsa_verify(struct ecdsa_key* key, const uint8_t* hash, size_t hash_len, const uint8_t* sig, size_t sig_len)` - Verify

**Random Number Generation**:

1. `rng_init(void)` - Initialize RNG
2. `rng_get_bytes(uint8_t* buf, size_t len)` - Get random bytes
3. `rng_seed(const uint8_t* seed, size_t len)` - Add entropy
4. `rng_reseed(void)` - Force reseed

---

## Phase 5: User Interface (Months 13-15)

### Timeline Overview

| Week  | Milestone            | Deliverables                         |
| ----- | -------------------- | ------------------------------------ |
| 1-2   | Framebuffer Graphics | VESA, mode setting, double buffering |
| 3-4   | Window System Core   | Window manager, compositing          |
| 5-6   | Input Subsystem      | Keyboard, mouse, touch               |
| 7-8   | Widget Toolkit       | Buttons, text, forms                 |
| 9-10  | Terminal Emulator    | VT100, UTF-8, colors                 |
| 11-12 | Desktop Environment  | Launcher, file manager               |

### 5.1 Framebuffer Graphics - Complete Implementation

#### Requirements

- **VESA VBE Support**: Mode enumeration and setting
- **Linear Framebuffer**: Direct pixel access
- **Double Buffering**: Prevent tearing
- **Hardware Acceleration**: If available
- **Color Formats**: 16/24/32-bit color depths
- **Screen Resolution**: Multiple resolution support

#### Implementation Details

**Data Structures**:

```c
struct framebuffer {
    uintptr_t physical_addr;
    void* virtual_addr;
    uint32_t width;
    uint32_t height;
    uint32_t pitch;                 /* Bytes per scanline */
    uint8_t bpp;                    /* Bits per pixel */
    uint8_t red_pos;
    uint8_t red_size;
    uint8_t green_pos;
    uint8_t green_size;
    uint8_t blue_pos;
    uint8_t blue_size;
    uint32_t size;                  /* Total buffer size */

    /* Double buffering */
    void* back_buffer;
    bool double_buffered;

    /* Dirty regions */
    struct rect* dirty_rects;
    uint32_t dirty_count;

    spinlock_t lock;
};

struct vbe_mode_info {
    uint16_t mode_number;
    uint16_t width;
    uint16_t height;
    uint8_t bpp;
    uintptr_t framebuffer_addr;
    uint16_t pitch;
    uint8_t memory_model;
    uint8_t red_mask_size;
    uint8_t red_field_pos;
    uint8_t green_mask_size;
    uint8_t green_field_pos;
    uint8_t blue_mask_size;
    uint8_t blue_field_pos;
};

struct rect {
    int32_t x, y;
    uint32_t width, height;
};

struct point {
    int32_t x, y;
};

typedef uint32_t color_t;  /* ARGB format */
```

**Functions to Implement**:

1. `fb_init(struct framebuffer* fb)` - Initialize framebuffer
2. `fb_set_mode(uint32_t width, uint32_t height, uint8_t bpp)` - Set video mode
3. `fb_get_modes(struct vbe_mode_info* modes, uint32_t* count)` - Get available modes
4. `fb_put_pixel(struct framebuffer* fb, int x, int y, color_t color)` - Draw pixel
5. `fb_get_pixel(struct framebuffer* fb, int x, int y)` - Get pixel
6. `fb_fill_rect(struct framebuffer* fb, struct rect* r, color_t color)` - Fill rectangle
7. `fb_draw_rect(struct framebuffer* fb, struct rect* r, color_t color)` - Draw rectangle outline
8. `fb_draw_line(struct framebuffer* fb, int x1, int y1, int x2, int y2, color_t color)` - Draw line
9. `fb_blit(struct framebuffer* fb, int x, int y, void* src, uint32_t w, uint32_t h)` - Blit bitmap
10. `fb_scroll(struct framebuffer* fb, int dx, int dy)` - Scroll content
11. `fb_clear(struct framebuffer* fb, color_t color)` - Clear screen
12. `fb_swap_buffers(struct framebuffer* fb)` - Swap double buffer
13. `fb_mark_dirty(struct framebuffer* fb, struct rect* r)` - Mark dirty region
14. `fb_flush_dirty(struct framebuffer* fb)` - Flush dirty regions

**Drawing Algorithms**:

**Bresenham's Line Algorithm**:

```c
void fb_draw_line(struct framebuffer* fb, int x0, int y0, int x1, int y1, color_t color) {
    int dx = abs(x1 - x0);
    int dy = abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1;
    int sy = y0 < y1 ? 1 : -1;
    int err = dx - dy;

    while (1) {
        fb_put_pixel(fb, x0, y0, color);
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 > -dy) { err -= dy; x0 += sx; }
        if (e2 < dx) { err += dx; y0 += sy; }
    }
}
```

---

### 5.2 Window System - Complete Implementation

#### Requirements

- **Window Manager**: Create, destroy, move, resize windows
- **Compositing**: Alpha blending, shadows, effects
- **Z-Order**: Window stacking, raise/lower
- **Focus Management**: Input focus, activation
- **Decorations**: Title bars, borders, buttons
- **Event Dispatch**: Mouse, keyboard to windows

#### Implementation Details

**Data Structures**:

```c
struct window {
    uint32_t id;
    char title[256];

    /* Geometry */
    int32_t x, y;
    uint32_t width, height;
    uint32_t min_width, min_height;
    uint32_t max_width, max_height;

    /* Content */
    void* surface;              /* Window content bitmap */
    uint32_t surface_pitch;

    /* State */
    bool visible;
    bool focused;
    bool minimized;
    bool maximized;
    bool fullscreen;
    bool decorated;
    uint8_t opacity;            /* 0-255 */

    /* Hierarchy */
    struct window* parent;
    struct window* children;
    struct window* next_sibling;
    struct window* prev_sibling;

    /* Z-order */
    struct window* above;
    struct window* below;

    /* Owner process */
    pid_t owner_pid;

    /* Event queue */
    struct event_queue* events;

    /* Callbacks */
    void (*on_expose)(struct window*);
    void (*on_resize)(struct window*, uint32_t, uint32_t);
    void (*on_key)(struct window*, struct key_event*);
    void (*on_mouse)(struct window*, struct mouse_event*);
    void (*on_close)(struct window*);

    spinlock_t lock;
};

struct compositor {
    struct framebuffer* fb;
    struct window* root;        /* Desktop window */
    struct window* windows;     /* All windows */
    struct window* top;         /* Topmost window */
    struct window* focused;     /* Focused window */

    /* Cursor */
    int32_t cursor_x, cursor_y;
    void* cursor_bitmap;
    void* cursor_save;          /* Background under cursor */

    /* Damage tracking */
    struct region* damage;

    mutex_t lock;
};

struct event {
    uint32_t type;
    uint64_t timestamp;
    union {
        struct key_event key;
        struct mouse_event mouse;
        struct resize_event resize;
        struct focus_event focus;
    } data;
};
```

**Functions to Implement**:

1. `wm_init(void)` - Initialize window manager
2. `window_create(struct window* parent, int x, int y, uint32_t w, uint32_t h, const char* title)` - Create window
3. `window_destroy(struct window* win)` - Destroy window
4. `window_show(struct window* win)` - Show window
5. `window_hide(struct window* win)` - Hide window
6. `window_move(struct window* win, int x, int y)` - Move window
7. `window_resize(struct window* win, uint32_t w, uint32_t h)` - Resize window
8. `window_raise(struct window* win)` - Raise to top
9. `window_lower(struct window* win)` - Lower to bottom
10. `window_focus(struct window* win)` - Set focus
11. `window_set_title(struct window* win, const char* title)` - Set title
12. `window_invalidate(struct window* win, struct rect* r)` - Mark for redraw
13. `compositor_init(struct framebuffer* fb)` - Initialize compositor
14. `compositor_add_window(struct window* win)` - Add window
15. `compositor_remove_window(struct window* win)` - Remove window
16. `compositor_compose(void)` - Composite all windows
17. `compositor_dispatch_event(struct event* e)` - Dispatch event

**Compositing Algorithm**:

1. Clear back buffer (or use damage regions)
2. For each window from bottom to top:
   a. If window intersects damaged region:
   b. Draw window decorations (if decorated)
   c. Alpha-blend window content to back buffer
   d. Apply effects (shadows, transparency)
3. Draw cursor
4. Swap buffers

---

### 5.3 Input Subsystem - Complete Implementation

#### Requirements

- **Keyboard Driver**: PS/2, USB HID
- **Mouse Driver**: PS/2, USB HID
- **Input Event Queue**: Unified input events
- **Keyboard Layout**: Multiple layouts, modifiers
- **Repeat Rate**: Key repeat handling
- **Pointer Acceleration**: Mouse acceleration curves

#### Implementation Details

**Data Structures**:

```c
struct key_event {
    uint32_t keycode;           /* Hardware scan code */
    uint32_t keysym;            /* Logical key symbol */
    uint32_t unicode;           /* Unicode character */
    uint32_t modifiers;         /* Shift, Ctrl, Alt, etc. */
    bool pressed;               /* true=press, false=release */
    bool repeat;                /* Auto-repeat */
};

struct mouse_event {
    int32_t x, y;               /* Absolute position */
    int32_t dx, dy;             /* Relative movement */
    int32_t scroll_x, scroll_y; /* Scroll wheel */
    uint32_t buttons;           /* Button mask */
};

struct keyboard_state {
    bool keys[256];             /* Key pressed state */
    uint32_t modifiers;         /* Active modifiers */
    uint8_t leds;               /* Cap/Num/Scroll lock */
    struct keyboard_layout* layout;

    /* Repeat */
    uint32_t repeat_key;
    uint64_t repeat_start;
    uint32_t repeat_delay;      /* Initial delay (ms) */
    uint32_t repeat_rate;       /* Repeat rate (ms) */
};

struct mouse_state {
    int32_t x, y;
    uint32_t buttons;

    /* Acceleration */
    float threshold;
    float acceleration;
    float max_speed;
};

/* Modifier bits */
#define MOD_SHIFT       0x0001
#define MOD_CTRL        0x0002
#define MOD_ALT         0x0004
#define MOD_META        0x0008
#define MOD_CAPSLOCK    0x0010
#define MOD_NUMLOCK     0x0020
#define MOD_SCROLLLOCK  0x0040
```

**Functions to Implement**:

1. `input_init(void)` - Initialize input subsystem
2. `keyboard_init(void)` - Initialize keyboard
3. `keyboard_handle_scancode(uint8_t scancode)` - Process scancode
4. `keyboard_set_layout(const char* layout)` - Set keyboard layout
5. `keyboard_set_repeat(uint32_t delay, uint32_t rate)` - Set repeat rate
6. `keyboard_set_leds(uint8_t leds)` - Set LED state
7. `mouse_init(void)` - Initialize mouse
8. `mouse_handle_packet(uint8_t* packet, size_t len)` - Process mouse packet
9. `mouse_set_acceleration(float threshold, float accel)` - Set acceleration
10. `input_poll(struct event* e, uint32_t timeout)` - Poll for event
11. `input_push_event(struct event* e)` - Push event to queue
12. `input_get_keyboard_state(struct keyboard_state* state)` - Get keyboard state
13. `input_get_mouse_state(struct mouse_state* state)` - Get mouse state

---

## Phase 6: Lisp Integration (Months 16-18)

### Timeline Overview

| Week  | Milestone               | Deliverables              |
| ----- | ----------------------- | ------------------------- |
| 1-2   | Kernel Lisp Integration | Kernel services from Lisp |
| 3-4   | System Call Interface   | Lisp FFI for syscalls     |
| 5-6   | Driver Development Kit  | Write drivers in Lisp     |
| 7-8   | Shell and REPL          | Interactive Lisp shell    |
| 9-10  | Package System          | Package management        |
| 11-12 | Standard Library        | Core Lisp libraries       |

### 6.1 Kernel Lisp Integration

#### Requirements

- **Lisp Evaluator in Kernel**: Safe subset for kernel code
- **Memory Management**: Lisp GC integration with kernel VMM
- **System Services**: Expose kernel services to Lisp
- **Error Handling**: Lisp conditions in kernel context
- **Performance**: JIT compilation for hot paths

#### Implementation Details

**Data Structures**:

```c
/* Kernel Lisp context */
struct kernel_lisp {
    struct vm_context vm;
    struct environment* kernel_env;
    struct gc_context* gc;

    /* Kernel bindings */
    struct symbol_table* kernel_symbols;

    /* Memory limits */
    size_t heap_limit;
    size_t stack_limit;

    /* Error handling */
    struct condition* last_error;
    jmp_buf error_handler;

    spinlock_t lock;
};

/* Kernel-callable Lisp function */
struct kernel_lisp_function {
    const char* name;
    lisp_value (*handler)(struct kernel_lisp* kl, lisp_value args);
    int min_args;
    int max_args;
    const char* doc;
};
```

**Kernel Services to Expose**:

```lisp
;; Process management
(kernel:spawn program args env)
(kernel:kill pid signal)
(kernel:wait pid)
(kernel:getpid)
(kernel:getppid)

;; Memory management
(kernel:mmap addr size prot flags)
(kernel:munmap addr size)
(kernel:mprotect addr size prot)

;; File operations
(kernel:open path flags mode)
(kernel:close fd)
(kernel:read fd buf count)
(kernel:write fd buf count)
(kernel:lseek fd offset whence)
(kernel:stat path)

;; Network operations
(kernel:socket domain type protocol)
(kernel:bind sockfd addr)
(kernel:listen sockfd backlog)
(kernel:accept sockfd)
(kernel:connect sockfd addr)
(kernel:send sockfd buf flags)
(kernel:recv sockfd buf flags)

;; Device I/O
(kernel:ioctl fd cmd arg)
(kernel:inb port)
(kernel:outb port value)

;; Time
(kernel:time)
(kernel:sleep seconds)
(kernel:nanosleep seconds nanoseconds)
```

**Functions to Implement**:

1. `kernel_lisp_init(struct kernel_lisp* kl)` - Initialize kernel Lisp
2. `kernel_lisp_eval(struct kernel_lisp* kl, const char* expr)` - Evaluate expression
3. `kernel_lisp_call(struct kernel_lisp* kl, const char* func, lisp_value args)` - Call function
4. `kernel_lisp_register_function(struct kernel_lisp* kl, struct kernel_lisp_function* func)` - Register function
5. `kernel_lisp_gc(struct kernel_lisp* kl)` - Run garbage collection
6. `kernel_lisp_load_file(struct kernel_lisp* kl, const char* path)` - Load Lisp file

---

_Document continues with Phases 7-12..._
