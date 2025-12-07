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

**Detailed API Specification**:

#### 1. `btree_init`

```c
int btree_init(struct btree* tree, uint32_t node_size, uint32_t key_size, uint32_t val_size);
```

**Description**:
Initializes a new B+ Tree structure. accurate calculation of `max_keys` based on `node_size` and key/value sizes is critical here to ensure nodes fit within disk blocks.
**Parameters**:

- `tree`: Pointer to the `btree` structure to initialize.
- `node_size`: Size of a tree node in bytes (typically 4096 to match page size).
- `key_size`: Size of the key in bytes.
- `val_size`: Size of the value in bytes (for leaf nodes).
  **Returns**: `0` on success, or `-EINVAL` if sizes are invalid.
  **Side Effects**: Allocates memory for the root node cache but does not write to disk until the first insert/flush.

#### 2. `btree_insert`

```c
int btree_insert(struct btree* tree, uint32_t key, uint64_t value);
```

**Description**:
Inserts a key-value pair into the B+ tree. This function handles the full "root-to-leaf" traversal, identifying the correct leaf node. If the leaf is full, it triggers a `btree_split_leaf` operation. If the split propagates up to the root, it creates a new root node, increasing tree height.
**Parameters**:

- `tree`: Active B+ Tree.
- `key`: Unique 32-bit integer key (inode number).
- `value`: 64-bit value (block pointer or struct offset).
  **Returns**: `0` on success, `-EEXIST` if key exists, `-ENOMEM` if node allocation fails.

#### 3. `btree_delete`

```c
int btree_delete(struct btree* tree, uint32_t key);
```

**Description**:
Removes a key from the tree. If deletion causes a node to fall below `min_keys` (underflow), this function initiates rebalancing via `btree_borrow_from_sibling` or `btree_merge_leaf`.
**Returns**: `0` on success, `-ENOENT` if key not found.

#### 4. `btree_search`

```c
int btree_search(struct btree* tree, uint32_t key, uint64_t* value_out);
```

**Description**:
Traverses the tree to find the value associated with `key`. Uses binary search within each internal node to find the child pointer, and binary search within the leaf node to find the exact key.
**Performance**: O(log_m N) where m is branching factor.

#### 5. `btree_range_query`

```c
int btree_range_query(struct btree* tree, uint32_t start_key, uint32_t end_key, uint64_t* results, size_t max_results);
```

**Description**:
Finds the leaf node containing `start_key`, then traverses the linked list of leaf nodes (`next_leaf_id`) to collect all keys up to `end_key`.
**Use Case**: Directory listings, extent lookups.

#### 6. `btree_split_leaf`

```c
int btree_split_leaf(struct btree* tree, struct btree_node* node, struct btree_node** new_node_out);
```

**Description**:
Splits a full leaf node into two.

1. Allocates a new node `new_node`.
2. Moves the upper 50% of keys/values from `node` to `new_node`.
3. Updates linked list: `node->next` becomes `new_node`, `new_node->next` becomes old `node->next`.
4. Helper Function returns the split key (first key of `new_node`) to be inserted into parent.

#### 7. `btree_split_internal`

```c
int btree_split_internal(struct btree* tree, struct btree_node* node, struct btree_node** new_node_out, uint32_t* middle_key_out);
```

**Description**:
Splits a full internal node.

1. Allocates `new_node`.
2. Moves upper 50% of keys and children to `new_node`.
3. "Push up" the middle key (it is removed from the node and returned to be inserted in parent).
4. Updates parent pointers.

#### 8. `btree_merge_leaf`

```c
int btree_merge_leaf(struct btree* tree, struct btree_node* left, struct btree_node* right);
```

**Description**:
Merges `right` node into `left` node when they are both underfull.

1. Copies all keys/values from `right` to `left`.
2. Updates `left->next` to `right->next`.
3. Marks `right` node as free/deleted in the node allocator.
4. Updates parent to remove the separator key.

#### 9. `btree_merge_internal`

```c
int btree_merge_internal(struct btree* tree, struct btree_node* left, struct btree_node* right, uint32_t separator_key);
```

**Description**:
Merges two internal nodes. Includes pulling down the `separator_key` from the parent into the merged node.

#### 10. `btree_borrow_from_sibling`

```c
int btree_borrow_from_sibling(struct btree* tree, struct btree_node* node, struct btree_node* sibling, bool is_left_sibling);
```

**Description**:
Moves one key (and corresponding child/value) from a wealthy sibling to an underflowing node to avoid merging. Updates the parent's separator key to reflect the new boundary.

#### 11. `btree_rebalance`

```c
int btree_rebalance(struct btree* tree, struct btree_node* node);
```

**Description**:
The master rebalancing logic. Checks if `node` is underfull. Looks at left and right siblings.

- If `left` has spares -> `borrow_from_sibling(node, left)`.
- If `right` has spares -> `borrow_from_sibling(node, right)`.
- Else -> `merge_leaf` or `merge_internal` with available sibling.

#### 12. `btree_cache_get`

```c
struct btree_node* btree_cache_get(struct btree* tree, uint32_t node_id);
```

**Description**:
Check LRU cache for node. If miss, reads from disk via block device driver. If hit, moves to front of LRU list.

#### 13. `btree_cache_put`

```c
void btree_cache_put(struct btree* tree, struct btree_node* node);
```

**Description**:
Inserts a node into the cache. If cache is full, calls `btree_cache_evict`.

#### 14. `btree_cache_evict`

```c
void btree_cache_evict(struct btree* tree);
```

**Description**:
Finds least recently used node. If `dirty` flag is set, writes to disk. Then frees memory.

#### 15. `btree_flush`

```c
int btree_flush(struct btree* tree);
```

**Description**:
Iterates through all cache nodes. If `dirty`, writes them to disk. Used during `sync` or unmount.

**B+ Tree Algorithms**:

**Detailed Algorithmic Specifications**:

**Insert Algorithm (Iterative)**:

1.  **Search Phase**:
    - Start at `root_node`.
    - While `node` is internal:
      - Binary search keys to find index `i` such that `keys[i] <= key < keys[i+1]`.
      - Push `node` and index `i` onto `traversal_stack` (for parent pointers).
      - Fetch child node `child_ids[i]`. `node = child`.
2.  **Leaf Phase**:
    - Binary search in leaf `node` for insertion position.
    - If key exists -> return Error.
3.  **Insert/Split Phase**:
    - If `node.key_count < max_keys`:
      - Shift keys/values right.
      - Insert new key/value.
      - Mark `node` dirty.
      - Return Success.
    - Else (Leaf Full):
      - Call `btree_split_leaf(node)`. Returns `new_node` and `split_key`.
      - Propagate `split_key` up the stack.
      - While stack not empty:
        - Pop `parent`.
        - If `parent` has space:
          - Insert `split_key` and `new_node` pointer.
          - Return.
        - Else (Internal Node Full):
          - Call `btree_split_internal(parent)`.
          - Update `split_key` and `new_node` for next iteration.
      - If stack empty (Root Split):
        - Allocate `new_root`.
        - `new_root` points to old `root` and `new_node`.
        - Update `tree->root_id`.

**Delete Algorithm**:

1.  **Search Phase**: Same as insert, track path in stack.
2.  **Leaf Phase**:
    - Remove key/value. Shift remaining left.
    - If `key_count >= min_keys`: Return Success.
    - Else (Underflow):
      - Peek `parent` from stack.
      - Identify left/right siblings using `parent` child pointers.
      - Attempt `btree_borrow_from_sibling`. If success, Update `parent` key, Return.
      - Attempt `btree_merge_leaf`. If success, remove separator from `parent`.
      - Recursively handle `parent` underflow (propagate up).
3.  **Collapsing Root**:
    - If `root` is internal and has only 1 child:
      - Make child the new root.
      - Free old root.

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

**Detailed API Specification**:

#### 1. `journal_init`

```c
int journal_init(struct journal* journal, uint64_t log_start_block, uint64_t log_size_blocks);
```

**Description**:
Initializes the write-ahead log system.

1. Reads the log header from `log_start_block`.
2. Validates the log signature and checksum.
3. If header is invalid (fresh disk), initializes a new empty log.
4. If header is valid, scans for the last written sequence number to enable append operations.
   **Parameters**:

- `journal`: Pointer to struct.
- `log_start_block`: LBA where log partition begins.
- `log_size_blocks`: Total size reserved for the log.
  **Returns**: `0` on success, error code otherwise.

#### 2. `journal_start_transaction`

```c
int journal_start_transaction(struct journal* journal, uint32_t* transaction_id_out);
```

**Description**:
Starts a new atomic transaction.

1. Acquires `journal_lock`.
2. Increments `next_transaction_id`.
3. Allocates a new `journal_transaction` structure.
4. Adds to `active_transactions` list.
   **Concurrency**: Thread-safe. Multiple transactions can be active (for non-conflicting ops).

#### 3. `journal_log_entry`

```c
int journal_log_entry(struct journal* journal, uint32_t transaction_id, enum journal_entry_type type, void* data, size_t size);
```

**Description**:
Writes an operation to the in-memory log buffer.

1. Validates `transaction_id`.
2. Marshals the data (opcode, target LBA, payload) into a `journal_entry` packet.
3. Calculates CRC32 checksum for the entry.
4. If log buffer full, triggers `journal_flush_buffer`.
   **Performance**: Zero-copy if possible (using scatter-gather).

#### 4. `journal_commit_transaction`

```c
int journal_commit_transaction(struct journal* journal, uint32_t transaction_id);
```

**Description**:
Commits a transaction to durability.

1. Changes transaction state to `COMMITTING`.
2. Writes a `JOURNAL_ENTRY_COMMIT` record to the log buffer.
3. Forces a disk flush (fsync) of the log area.
4. Updates in-memory state to `COMMITTED`.
5. Signals any threads waiting on this transaction.

#### 5. `journal_abort_transaction`

```c
int journal_abort_transaction(struct journal* journal, uint32_t transaction_id);
```

**Description**:
Rolls back an active transaction.

1. Logs a `JOURNAL_ENTRY_ABORT` record (for debugging/audit, strictly optional for correctness as uncommitted txns are ignored on recovery).
2. Discards any in-memory dirty pages associated with this transaction.
3. Frees transaction resources.

#### 6. `journal_checkpoint`

```c
int journal_checkpoint(struct journal* journal);
```

**Description**:
Truncates the log by applying committed changes to the main filesystem.

1. Iterates through `completed_transactions` from oldest to newest.
2. For each modified block, writes the data to its final LBA in the main FS partition.
3. Updates the `log_start_ptr` in the on-disk log header.
4. Discards freed log blocks.

#### 7. `journal_recover`

```c
int journal_recover(struct journal* journal);
```

**Description**:
Replays the log after an unclean shutdown.

1. Reads log from `last_checkpoint`.
2. Scans forward, verifying checksums of all entries.
3. Builds a map of `transaction_id` -> `state` (Checking for COMMIT records).
4. Replays only operations belonging to COMMITTED transactions.
5. Ignores partial or uncommitted transactions.

#### 8. `journal_rotate`

```c
int journal_rotate(struct journal* journal);
```

**Description**:
Handles circular buffer wrap-around. If the log write pointer nears the end of the log area, this function triggers a forced checkpoint to free up space at the beginning.

#### 9. `journal_get_statistics`

```c
int journal_get_statistics(struct journal* journal, struct journal_stats* stats);
```

**Description**:
Returns telemetry: operations per second, average commit latency, log usage percentage.

**Journaling Algorithms (Write-Ahead Log)**:

**Standard Logging Flow**:

1.  **Prepare**: App creates a transaction `TxID`.
2.  **Log**:
    - For every metadata change (inode update, bitmap flip):
    - Create `ENTRY(TxID, LBA, OldData, NewData)`.
    - Append to `LogBuffer`.
3.  **Commit**:
    - App calls commit.
    - System creates `COMMIT(TxID)` entry.
    - **BARRIER**: Disk Cache Flush.
    - Write `LogBuffer` to Log Partition.
    - **BARRIER**: Disk Cache Flush.
    - Transaction is now Durable.
4.  **Apply (Async)**:
    - Background thread writes `NewData` to Main FS LBA.
    - On completion, advanced Log Head pointer.

**Recovery Algorithm (ARIES-style)**:

1.  **Analysis Phase**:
    - Scan log from last checkpoint to end.
    - If `ENTRY` found -> Add to "Dirty Pages Table".
    - If `COMMIT` found -> Add TxID to "Winner Set".
    - If `ABORT` found or End-of-Log without Commit -> Add TxID to "Loser Set".
2.  **Redo Phase**:
    - Replay ALL updates from "Winner Set" in order.
    - Update disk blocks to match logged `NewData`.
3.  **Undo Phase**:
    - (Optional, if using Undo/Redo login). For LFSX (Redo-only), simple discard of "Loser Set" is sufficient since modifications aren't applied to main FS until checkpoint.

**Error Handling**:

- **Log Full**: Block new transactions until Checkpoint completes.
- **Checksum Mismatch**: Truncate log at point of failure (assume power loss during write). Treat remainder as invalid.
- **Write Failure**: Mark filesystem read-only immediately (panic).

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

**Detailed API Specification**:

#### 1. `transaction_begin`

```c
int transaction_begin(enum isolation_level level, uint32_t* transaction_id_out);
```

**Description**:
Initiates a defined scope for atomic operations.

1. allocates new `trx` struct.
2. sets `start_ts` to global monotonic clock.
3. If `level == SERIALIZABLE`, acquires global shared lock (or range locks).
   **Returns**: Transaction ID handle.

#### 2. `transaction_commit`

```c
int transaction_commit(uint32_t transaction_id);
```

**Description**:
Finalizes the transaction.

1. **Validation Phase** (for SSI/Snapshot isolation): Checks if read set overlaps with any concurrent committed write sets.
2. If validation fails -> `transaction_abort` -> return `-ECONFLICT`.
3. If success, calls `journal_commit_transaction`.
4. Updates global `last_committed_ts`.
5. Releases all locks.

#### 3. `transaction_abort`

```c
int transaction_abort(uint32_t transaction_id);
```

**Description**:
Reverts all changes.

1. Walks `changes` list.
2. Restores old values in memory (or simply discards new versions in MVCC).
3. Releases all locks.
4. Marks transaction object as `ABORTED`.

#### 4. `transaction_read`

```c
int transaction_read(uint32_t transaction_id, uint32_t key, void* value_out, size_t size);
```

**Description**:
Reads data respecting isolation level.

- **READ_COMMITTED**: Reads latest version where `commit_ts <= current_ts`.
- **REPEATABLE_READ**: Reads version where `commit_ts <= trx.start_ts`.
- **SERIALIZABLE**: Reads latest + Acquires Shared Lock on `key`.

#### 5. `transaction_write`

```c
int transaction_write(uint32_t transaction_id, uint32_t key, const void* value, size_t size);
```

**Description**:
Writes data.

1. Acquires Exclusive Lock on `key`. If busy, wait or deadlock check.
2. Creates a new version of the data object.
3. Tag with `transaction_id`.
4. Append to `trx.changes` list.

#### 6. `transaction_detect_deadlock`

```c
int transaction_detect_deadlock(void);
```

**Description**:
Runs periodically or on lock contention.

1. Constructs "Waits-For" Directed Graph using lock queues.
2. Uses DFS (Depth First Search) to find cycles.
3. If cycle A->B->A is found:
   - Selects victim (usually valid B).
   - Calls `transaction_abort(B)`.
   - Wake up A.

**MVCC Implementation Details**:

1.  **Version Storage**:
    - Each data page/object has a linked list of versions.
    - `Head -> [Ver3 (Active, Tx105)] -> [Ver2 (Committed, Ts=50)] -> [Ver1 (Committed, Ts=40)]`.
2.  **Visibility Rules (Snapshot Isolation)**:
    - Tx110 starts at Ts=60.
    - It sees Ver2 (because 50 <= 60).
    - It ignores Ver3 (Tx105 not committed or committed after 60).
3.  **Garbage Collection (Vacuum)**:
    - Background thread tracks "Oldest Active Transaction Start Timestamp" (MinStartTs).
    - Any version with `EndTs < MinStartTs` is unreachable by any active transaction.
    - It can be physically deleted.

**Deadlock Detection Algorithm**:

1.  **State**: Global `WaitGraph` G = (V, E).
2.  **Event**: Tx1 wants lock L held by Tx2.
    - Add Edge Tx1 -> Tx2.
    - Run `DetectCycle(Tx1)`.
3.  **DetectCycle(Node N)**:
    - Stack = [N]. Visited = {N}.
    - DFS traversal.
    - If edge points to node in Stack -> CYCLE DETECTED.
4.  **Resolution**:
    - Pick transaction with fewer locks/log-entries as victim.
    - Abort victim. Return error to caller.

**Error Handling**:

- **Lock Timeout**: If lock wait > 5s, auto-abort.
- **Validation Failure**: Optimistic concurrency control failure (Write skew). return Error.
- **Chain Logic**: If nested parent aborts, all children abort.

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

**Detailed API Specification**:

#### 1. `netdev_alloc`

```c
struct net_device* netdev_alloc(const char* name, size_t priv_size);
```

**Description**:
Allocates a new `net_device` structure and associated private data.

1. Allocates `sizeof(struct net_device) + priv_size` from kernel heap (zeroed).
2. Sets `priv` pointer to the end of the `net_device` struct.
3. Initializes spinlocks (`tx_lock`, `rx_lock`).
4. Sets default name (e.g., "eth%d").
   **Parameters**:

- `name`: Name template (e.g., "eth%d").
- `priv_size`: Size of driver-specific private data.

#### 2. `netdev_register`

```c
int netdev_register(struct net_device* dev);
```

**Description**:
Registers the device with the core network stack.

1. Assigns a unique interface index (ifindex).
2. Resolves the name template (e.g., "eth0").
3. Inserts into the global `net_device_list`.
4. Exposes the device to userspace (via sysfs equivalent).
5. Triggers a `NETDEV_REGISTER` event to listeners (DHCP client, etc.).

#### 3. `netdev_open`

```c
int netdev_open(struct net_device* dev);
```

**Description**:
Brings the interface UP.

1. Calls driver's `ops->open()`.
2. Allocates RX ring buffers if dynamic.
3. Enables hardware interrupts.
4. Sets `IFF_UP` flag.
5. Starts the TX queue.

#### 4. `netdev_xmit`

```c
int netdev_xmit(struct sk_buff* skb, struct net_device* dev);
```

**Description**:
Transmits a packet.

1. Acquires `tx_lock`.
2. Checks if TX queue is stopped (flow control).
3. Calls driver's `ops->start_xmit(skb, dev)`.
4. If driver returns `NETDEV_TX_BUSY`:
   - Requeues packet.
   - Stops queue.
   - Returns status.
5. Updates `tx_packets` and `tx_bytes` stats.

#### 5. `netdev_rx`

```c
int netdev_rx(struct net_device* dev, struct sk_buff* skb);
```

**Description**:
Called by driver interrupt handler to push received packet up the stack.

1. Sets `skb->dev`.
2. Identifies protocol (`eth_type_trans`).
3. If NAPI enabled, queues to softirq backlog.
4. Otherwise, calls `netif_receive_skb(skb)` immediately to pass to IP stack.

#### 6. `ring_buffer_init`

```c
int ring_buffer_init(struct net_ring* ring, uint32_t size, uint32_t desc_size);
```

**Description**:
Initializes a circular DMA ring buffer.

1. Allocates physically contiguous memory for descriptors (`dma_alloc_coherent`).
2. Allocates `sk_buff*` array for software management.
3. Resets head/tail indices.

#### 7. `netdev_poll` (NAPI)

```c
int netdev_poll(struct net_device* dev, int budget);
```

**Description**:
Polled in softirq context to process RX packets.

1. Reads up to `budget` packets from RX ring.
2. For each packet: `netif_receive_skb`.
3. Refills RX ring with new buffers.
4. If work done < budget:
   - Re-enables interrupts.
   - Removes self from poll list.
   - Returns work done.

**Detailed Algorithmic Specifications**:

**DMA Management Algorithm**:

1.  **Setup**:
    - Driver requests `pci_alloc_consistent` for descriptor rings.
    - Writes physical address base to NIC `TDBA/RDBA` registers.
2.  **TX Flow**:
    - Driver gets `skb->data` physical address (`dma_map_single`).
    - Writes address and length to current TX Descriptor.
    - Sets "End of Packet" bit.
    - Increments Tail index (Modulo RingSize).
    - Writes Tail to NIC `TDT` (Tail Descriptor Tail) register to trigger DMA.
3.  **RX Flow**:
    - Driver pre-allocates empty `skb`s.
    - Maps them and fills RX Descriptors.
    - Hardware DMAs packet data -> memory.
    - Hardware updates Head index (or write-back descriptor status).

**Interrupt Handling Logic**:

1.  **Hardware Interrupt**:
    - CPU jumps to ISR.
    - Masks interrupts on NIC.
    - Reads `ICR` (Interrupt Cause Register).
2.  **Dispatch**:
    - If `ICR & TX_DONE`: Clean transmitted buffers. Wake queue if stopped.
    - If `ICR & RX_READY`:
      - (Legacy) Read packet, pass to stack.
      - (NAPI) Disable RX IRQ, schedule SOFTIRQ, return.
    - If `ICR & LINK_CHANGE`: Check PHY status, update flags.
3.  **NAPI SoftIRQ**:
    - Runs `netdev_poll`.
    - If RX ring empty, re-enable RX IRQ.

**Error Handling**:

- **TX Timeout**: Watchdog timer fires if TX Queue active but no completion for 5s. Resets NIC hardware.
- **RX Overflow**: Increase RingIO size or drop packets. Increment `rx_dropped` counter.
- **DMA Error**: Detecting PCI bus error -> Full device reset.

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

**Detailed API Specification**:

#### 1. `ip_send`

```c
int ip_send(struct sk_buff* skb, uint32_t dest, uint8_t protocol);
```

**Description**:
Prepares and transmits an IP packet.

1. Allocates SKB head room for IP Header.
2. Performs Route Lookup (`ip_route_output`) to find outgoing interface and next-hop gateway.
3. Fills IPv4 Header (Version=4, IHL=5, TTL=64, ID=AtomicInc).
4. Calculates Header Checksum.
5. If packet length > MTU: calls `ip_fragment`.
6. Passes to `netdev_xmit`.

#### 2. `ip_rcv`

```c
int ip_rcv(struct sk_buff* skb, struct net_device* dev);
```

**Description**:
Main IP receive hook.

1. Validates checksum, version, and length.
2. Checks Pre-Routing Netfilter hooks.
3. If dest == local_addr: Passes to `ip_local_deliver`.
4. If dest != local_addr and forwarding enabled: Passes to `ip_forward`.
5. If fragment (MF bit set or Offset > 0): buffer in reassembly queue.

#### 3. `ip_fragment`

```c
int ip_fragment(struct sk_buff* skb, uint32_t mtu);
```

**Description**:
Splits a large packet into MTU-sized chunks.

1. Calculates number of fragments needed.
2. For each fragment:
   - Allocates new SKB.
   - Copies IP header.
   - Copies data slice.
   - Sets Fragment Offset field.
   - Sets MF (More Fragments) bit (except last).
   - Recalculates Checksum.
3. Queues all fragments for transmission.

#### 4. `ip_route_output`

```c
int ip_route_output(uint32_t dest, uint32_t src, struct route_entry** rt);
```

**Description**:
Finds the best path to destination.

1. Scans `routing_table` for longest prefix match (LPM) against `dest`.
2. Validates cache entries (expiration).
3. If no match found, returns `-ENETUNREACH` or uses Default Gateway.
4. Returns route entry containing outgoing device and next hop.

#### 5. `ip_defragment`

```c
struct sk_buff* ip_defragment(struct sk_buff* skb);
```

**Description**:
Reassembles fragments into a full packet.

1. Hashes (Src, Dst, ID, Proto) to find reassembly queue (`ipq`).
2. Inserts current fragment into sorted linked list (by offset).
3. Checks for overlapping fragments (Teardrop attack protection).
4. If all hole descriptors are filled:
   - Allocates large SKB.
   - Copies data from all fragments.
   - Returns full packet.
   - Frees fragment SKBs.
5. Otherwise, returns NULL (waiting for more).

#### 6. `route_add` / `route_del`

```c
int route_add(uint32_t dest, uint32_t mask, uint32_t gateway, struct net_device* dev);
int route_del(uint32_t dest, uint32_t mask);
```

**Description**:
Manipulates the generic routing table (FIB - Forwarding Information Base).

**Detailed Algorithmic Specifications**:

**IP Forwarding Algorithm**:

1.  **TTL Check**: Decrement TTL. If 0, drop and send ICMP Time Exceeded.
2.  **Lookup**: `ip_route_lookup(packet.dst)`.
3.  **Redirect**: If next-hop is on same subnet as source, send ICMP Redirect.
4.  **MTU Check**: If len > route.mtu:
    - If DF (Don't Fragment) set: Drop, send ICMP Frag Needed (Path MTU Discovery).
    - Else: `ip_fragment()`.
5.  **Output**: `netdev_xmit(packet, route.dev)`.

**Reassembly Timeout Logic**:

- Global Timer runs every 1 second.
- Scans all active Reassembly Queues.
- If `(current_time - q.creation_time) > IP_FRAG_TIME (60s)`:
  - Drop all fragments in queue.
  - Send ICMP Time Exceeded to sender (if 1st fragment was received).
  - Free Queue.

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

**Detailed API Specification**:

#### 1. `tcp_connect`

```c
int tcp_connect(struct tcp_sock* sk, uint32_t addr, uint16_t port);
```

**Description**:
Performs "Active Open".

1. Selects ephemeral local port if not bound.
2. Initializes Sequence Number (`iss` = SecureRandom).
3. Sets state `TCP_SYN_SENT`.
4. Constructs SYN packet (SEQ=ISS).
5. Starts Retransmission Timer.
6. Calls `ip_send`.

#### 2. `tcp_listen`

```c
int tcp_listen(struct tcp_sock* sk, int backlog);
```

**Description**:
Performs "Passive Open".

1. Allocates accept queue and SYN queue (half-open).
2. Sets state `TCP_LISTEN`.
3. Registers socket in global Listening Hash Table.

#### 3. `tcp_rcv`

```c
int tcp_rcv(struct sk_buff* skb);
```

**Description**:
Main TCP Input Engine.

1. Validates Checksum.
2. Finds socket using tuple (SrcIP, DstIP, SrcPort, DstPort).
3. Locks socket.
4. Calls `tcp_state_machine`.

#### 4. `tcp_sendmsg`

```c
int tcp_sendmsg(struct tcp_sock* sk, void* data, size_t len);
```

**Description**:
Queues data for transmission.

1. Breaks data into MSS-sized chunks.
2. Appends to `send_queue`.
3. If Nagle Algorithm permits and CWND allows:
   - Calls `tcp_transmit_skb`.
4. Otherwise, waits for ACK or Window Update.

**Detailed TCP State Machine & Logic**:

**1. CLOSED State**:

- Packet In: If RST, ignore. If ACK, send RST. If SYN, ignore (or RST).
- Event `connect()`: Send SYN, `snd_nxt`++, enter `SYN_SENT`.

**2. LISTEN State**:

- Packet In (SYN):
  - Create `request_sock` (mini-socket).
  - Send SYN-ACK (SEQ=ISS, ACK=RCV.SEQ+1).
  - Add to SYN Queue.
- Packet In (ACK):
  - Check SYN Queue. If match found:
  - Create full `tcp_sock`.
  - Move from SYN Queue to Accept Queue.
  - Wake up `accept()`.
  - Enter `ESTABLISHED`.

**3. SYN_SENT State**:

- Packet In (SYN-ACK):
  - Check Ack matches `snd_nxt`.
  - Send ACK.
  - Initialize Congestion Control (`cwnd=MSS`).
  - Enter `ESTABLISHED`.
- Packet In (SYN): Simultaneous Open. Send SYN-ACK, Enter `SYN_RCVD`.

**4. ESTABLISHED State**:

- **Data Receiver**:
  - Check Sequence Number (must be in window).
  - If expected (`seq == rcv_nxt`):
    - Copy to Receive Buffer.
    - `rcv_nxt += len`.
    - Send Delayed ACK (or immediate if "quickack" mode).
  - If gap (`seq > rcv_nxt`):
    - Queue in Out-Of-Order (OOO) queue.
    - Send SACK (Selective ACK) if negotiated.
    - Send Duplicate ACK (Trigger Fast Retransmit on sender).
- **Data Sender (on ACK)**:
  - Slow Start: `cwnd += MSS` (Exponential).
  - Congestion Avoidance: `cwnd += MSS*MSS/cwnd` (Linear).
  - Remove ACKed segments from `retransmit_queue`.
  - Reset Retransmission Timer.

**5. FIN Processing (Active Close)**:

- App calls `close()`.
- Send FIN. `snd_nxt`++.
- Enter `FIN_WAIT_1`.
- Receive ACK of FIN: Enter `FIN_WAIT_2`.
- Receive FIN from peer: Send ACK. Enter `TIME_WAIT`.

**6. FIN Processing (Passive Close)**:

- Receive FIN in `ESTABLISHED`.
- Send ACK. `rcv_nxt`++.
- Wake up App with EOF.
- Enter `CLOSE_WAIT`.
- App calls `close()`.
- Send FIN. Enter `LAST_ACK`.
- Receive ACK of FIN: Enter `CLOSED`.

**Congestion Control Algorithms (CUBIC/Reno)**:

**Fast Retransmit**:

- Trigger: Receive 3 Duplicate ACKs.
- Action: Retransmit oldest unacked segment IMMEDIATELY (bypass timer).
- Set `ssthresh = cwnd / 2`.
- Set `cwnd = ssthresh + 3*MSS`. (Fast Recovery).

**Retransmission Timeout (RTO)**:

- Trigger: Timer expires before ACK.
- Action:
  - Retransmit oldest segment.
  - Set `ssthresh = cwnd / 2`.
  - Set `cwnd = 1 MSS` (Reset to Slow Start).
  - Backoff RTO (Exponential backoff 2x, 4x...).

**Round Trip Time (RTT) Estimation**:

- Measure `MRTT` (sample RTT).
- `SRTT` (Smoothed) = 0.875 _ SRTT + 0.125 _ MRTT
- `RTTVAR` (Variance) = 0.75 _ RTTVAR + 0.25 _ |SRTT - MRTT|
- `RTO` = SRTT + 4 \* RTTVAR.

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

**Detailed API Specification**:

#### 1. `aes_encrypt_block`

```c
void aes_encrypt_block(struct aes_context* ctx, const uint8_t* in, uint8_t* out);
```

**Description**:
Encrypts a single 128-bit block using AES.

1. **Key Expansion**: (Performed in `aes_init`). Generates 11/13/15 round keys from seed key using Rijndael Key Schedule.
2. **Initial Round**: `AddRoundKey(State, RoundKey[0])`.
3. **Main Rounds** (Nr-1 iterations):
   - `SubBytes`: Nonlinear substitution using S-Box.
   - `ShiftRows`: Cyclic shift of rows 1, 2, 3 by 1, 2, 3 bytes.
   - `MixColumns`: Matrix multiplication over GF(2^8).
   - `AddRoundKey`: XOR with RoundKey[i].
4. **Final Round**:
   - `SubBytes`
   - `ShiftRows`
   - `AddRoundKey`
     **Side Channel Defense**: Use bitsliced implementation or constant-time S-Box lookups to prevent cache timing attacks.

#### 2. `sha256_update`

```c
void sha256_update(struct sha256_context* ctx, const uint8_t* data, size_t len);
```

**Description**:
Processes input data in 512-bit blocks.

1. Fills internal buffer. When 64 bytes full, triggers `sha256_transform`.
2. **Transform Logic**:
   - Expand 16 words into 64 words message schedule (W[0..63]).
   - Initialize working variables (a..h) from State.
   - **Main Loop** (64 rounds):
     - `S1 = RotR(e, 6) ^ RotR(e, 11) ^ RotR(e, 25)`
     - `ch = (e & f) ^ (~e & g)`
     - `temp1 = h + S1 + ch + K[i] + W[i]`
     - `S0 = RotR(a, 2) ^ RotR(a, 13) ^ RotR(a, 22)`
     - `maj = (a & b) ^ (a & c) ^ (b & c)`
     - `temp2 = S0 + maj`
     - Shift variables: `h=g, g=f, f=e, e=d+temp1, d=c, c=b, b=a, a=temp1+temp2`
   - Add working variables back to State.

#### 3. `rsa_generate_key`

```c
int rsa_generate_key(struct rsa_key* key, size_t bits);
```

**Description**:
Generates RSA keypair.

1. **Prime Generation**: Use Miller-Rabin Primality Test to find two large primes `p` and `q` each of `bits/2`.
2. check `gcd((p-1), e) == 1` and `gcd((q-1), e) == 1` (where e=65537).
3. Compute `n = p * q`.
4. Compute `phi = (p-1) * (q-1)`.
5. Compute `d = e^-1 mod phi` (Modular Inverse).
6. Securely clear `p`, `q`, `phi` from stack.

#### 4. `aes_gcm_encrypt`

```c
int aes_gcm_encrypt(struct aes_context* ctx, const uint8_t* iv, const uint8_t* aad, size_t aad_len, const uint8_t* in, uint8_t* out, size_t len, uint8_t* tag);
```

**Description**:
Authenticated Encryption.

1. **CTR Mode**: Encrypts `in` to `out` using AES-CTR with counter starting from IV||1.
2. **GHASH**: Computes Message Authentication Code.
   - Hash `AAD` (padded).
   - Hash `Ciphertext` (padded).
   - Hash `Lengths` (64-bit aad_len || 64-bit in_len).
   - Polynomial multiplication in GF(2^128).
3. **Tag Generation**: XOR GHASH output with `AES(IV||0)`.

**Testing Requirements**:

- Test against NIST vectors (Kat - Known Answer Tests)
- Test Monte Carlo simulations
- Test boundary conditions (empty input, large input)
- Test invalid keys/IVs
- Valgrind analysis for constant-time compliance

---

### 4.5 Secure Boot - Complete Implementation

#### Requirements

- **Measured Boot**: Measure boot stages into TPM PCRs
- **Verified Boot**: Verify signatures of boot stages
- **TPM Integration**: Drivers for TPM 1.2/2.0
- **Key Hierarchy**: Root of Trust (SRK) managing OS signing keys

#### Implementation Details

**Data Structures**:

```c
struct tpm_pcr_bank {
    uint32_t pcr_index;
    uint8_t digest[32]; /* SHA-256 */
};

struct boot_manifest {
    uint32_t magic;
    uint32_t version;
    uint8_t kernel_hash[32];
    uint8_t ramdisk_hash[32];
    uint8_t config_hash[32];
    uint8_t signature[256]; /* RSA-2048 */
};
```

**Functions to Implement**:

1. `tpm_init(void)` - Initialize TPM driver (TIS/CRB interface)
2. `tpm_extend(uint32_t pcr, const uint8_t* digest)` - Extend PCR
3. `tpm_quote(uint32_t pcr_mask, const uint8_t* nonce, uint8_t* signature)` - Get Quote
4. `secure_boot_verify(void* image, size_t size, uint8_t* expected_hash)` - Verify Hash
5. `secure_boot_check_signature(struct boot_manifest* manifest)` - Verify RSA Signature

**Secure Boot Sequence**:

1. **BIOS/UEFI**: Measures Bootloader into PCR[0]. Verifies Bootloader Signature.
2. **Bootloader**:
   - Initialize TPM driver.
   - Measure Kernel into PCR[4].
   - Measure Ramdisk into PCR[5].
   - Measure Kernel cmdline into PCR[8].
   - Load `manifest.bin`.
   - Verify `manifest->signature` using embedded Public Key.
   - Hash loaded Kernel. Verify match with `manifest->kernel_hash`.
   - If Match: Jump to Kernel.
   - If Mismatch: Halt or Recovery Mode.

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

**Detailed API Specification**:

#### 1. `fb_put_pixel`

```c
void fb_put_pixel(struct framebuffer* fb, int x, int y, color_t color);
```

**Description**:
Draws a single pixel.

1. Checks bounds `(0 <= x < width)` and `(0 <= y < height)`.
2. Calculates offset: `offset = y * pitch + x * (bpp/8)`.
3. Writes pixel value to `fb->back_buffer` (if double buffered) or `fb->virtual_addr`.

#### 2. `fb_draw_line` (Bresenham's)

```c
void fb_draw_line(struct framebuffer* fb, int x0, int y0, int x1, int y1, color_t color);
```

**Description**:
Draws a line using integer-only arithmetic.

1. `dx = abs(x1-x0)`, `dy = -abs(y1-y0)`.
2. `sx = x0 < x1 ? 1 : -1`, `sy = y0 < y1 ? 1 : -1`.
3. `err = dx + dy`.
4. Loop:
   - `put_pixel(x0, y0)`.
   - If `x0 == x1 && y0 == y1` break.
   - `e2 = 2 * err`.
   - If `e2 >= dy`: `err += dy`, `x0 += sx`.
   - If `e2 <= dx`: `err += dx`, `y0 += sy`.

#### 3. `fb_fill_rect`

```c
void fb_fill_rect(struct framebuffer* fb, struct rect* r, color_t color);
```

**Description**:
Optimized rectangle fill.

1. Clips rectangle to screen bounds.
2. For each scanline `y` from `r.y` to `r.y + r.height`:
   - Calculate pointer to start of line in buffer.
   - Use `memset` (8/16bpp) or `memset32` (32bpp) to fill `r.width` pixels.
     **Optimization**: Uses AVX/SSE `rep stosd` equivalent for fast fills.

#### 4. `fb_swap_buffers`

```c
void fb_swap_buffers(struct framebuffer* fb);
```

**Description**:
Flips the back buffer to the front.

1. Acquires lock.
2. `memcpy(front_buffer, back_buffer, size)`.
3. Clears dirty region list (if implementing dirty rectangles).
4. Waits for VSync (if supported by VBE/EDID info).

**Detailed Algorithmic Specifications**:

**Dirty Rectangle Tracking**:

1.  **Mark**: On drawing op, union the modified `rect` with existing `dirty_region`.
2.  **Merge**: If `dirty_count > threshold` or regions overlap significantly, merge into a bounding box to reduce overhead.
3.  **Flush**: In `swap_buffers`, only copy the pixels contained in the `dirty_region_list` from Back to Front buffer.

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

**Detailed API Specification**:

#### 1. `window_create`

```c
struct window* window_create(struct window* parent, int x, int y, uint32_t w, uint32_t h, const char* title);
```

**Description**:
Creates a new window object.

1. Allocates `window` struct.
2. Allocates per-window surface (`malloc(w * h * 4)`).
3. Adds to global window list (default Z-order: Top).
4. Initializes event queue.

#### 2. `compositor_compose`

```c
void compositor_compose(void);
```

**Description**:
Renders the desktop scene.

1. **Background**: Clears back buffer to background color/wallpaper.
2. **Window Loop**: Iterates `window_list` from Bottom (Tail) to Top (Head).
3. **Clipping**: If window is obscured by opaque window above it, skip obscured regions (advanced) or just Painter's Algorithm.
4. **Resampling**: If `win->surface` resolution differs from window size (scaling).
5. **Blending**:
   - `FinalR = (SrcR * Alpha + DstR * (255 - Alpha)) / 255`.
   - `FinalG = (SrcG * Alpha + DstG * (255 - Alpha)) / 255`.
   - `FinalB = (SrcB * Alpha + DstB * (255 - Alpha)) / 255`.
6. **Cursor**: Blit cursor sprite at `cursor_x, cursor_y` (Hardware Sprite if available, else Software).
7. **Damage Reset**: Clears damage regions.

#### 3. `compositor_dispatch_event`

```c
void compositor_dispatch_event(struct event* e);
```

**Description**:
Routes input to appropriate window.

1. **Mouse**:
   - Hit testing: Iterate Z-order Top-to-Bottom.
   - `if (mx >= win.x && mx < win.x + win.w && ...)` -> Target Found.
   - Transform coordinates global -> local.
   - Push to `win->events`.
2. **Keyboard**:
   - Push to `focused_window->events`.
3. **Global**: Handle Alt-Tab, global shortcuts.

**Compositing Algorithm Details**:

**Alpha Blending Formula**:
For each pixel `P(x,y)`:
$$ C*{out} = \frac{C*{src} \times \alpha + C\_{dst} \times (255 - \alpha)}{255} $$
Where:

- $C_{src}$ is the pixel color of the window being drawn.
- $C_{dst}$ is the current background color at that position.
- $\alpha$ is the window opacity \* per-pixel alpha channel.

**Damage Tracking**:
To avoid full redraws:

1.  Subtract `ObscuredRegion` (by windows above) from `WindowRect`.
2.  Intersect result with `DamagedRegion`.
3.  Only composite the resulting implementation set.

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

**Detailed API Specification**:

#### 1. `keyboard_handle_scancode`

```c
void keyboard_handle_scancode(uint8_t scancode);
```

**Description**:
State machine for PS/2 Scancode Set 2 translation.

1. **State Tracking**: `E0_PREFIX`, `KEY_RELEASE`.
2. **Translation**: `Scancode -> KeySym` using layout table.
3. **Modifier Handling**:
   - Updates `shift_pressed`, `ctrl_pressed` flags.
   - Toggles `caps_lock`, `num_lock` LED states.
4. **Repeat Logic**:
   - If key is held, `repeat_timer` fires synthetic key events.
5. **Event Emission**:
   - Creates `key_event` struct.
   - Pushes to Global Input Queue.

#### 2. `mouse_handle_packet`

```c
void mouse_handle_packet(uint8_t* packet, size_t len);
```

**Description**:
Processes raw mouse bytes (usually 3 or 4 bytes per PS/2 or USB HID packet).

1. Decodes X/Y displacement and Button flags.
2. **Acceleration**:
   - `velocity = sqrt(dx*dx + dy*dy)`.
   - `factor = (velocity > threshold) ? acceleration_curve(velocity) : 1.0`.
   - `dx *= factor`.
3. **Clipping**: Clamps cursor position to screen bounds `[0, width], [0, height]`.
4. **Drawing**: Marks old cursor region as dirty; Updates `cursor_x/y`; Marks new region dirty.
5. **Event**: Generates `mouse_event` (Move/Click) and pushes to queue.

#### 3. `input_poll`

```c
int input_poll(struct event* e, uint32_t timeout);
```

**Description**:
Retrieves the next event for the window manager or focused application.

1. Checks `global_event_queue`.
2. If empty, sleeps process until interrupt fires or timeout expires.
3. If event available, copies to `e` and returns 1.

**Detailed Algorithmic Specifications**:

**Pointer Acceleration Curve**:
$$ M*{out} = M*{in} \times (1 + \alpha \times \frac{|V| - T}{T}) $$
Where:

- $M_{in}$ is raw movement.
- $V$ is velocity.
- $T$ is threshold.
- $\alpha$ is acceleration factor.
  The curve acts to keep fine movements 1:1 for precision while amplifying fast movements for screen traversal.

**Keyboard Layout Mapping**:

- Uses a multi-layer table `KeyMap[Layout][ModifierState][Scancode]`.
- Example: `KeyMap[US][SHIFT][0x1E (A)] = 'A'`.
- Example: `KeyMap[US][NONE][0x1E (A)] = 'a'`.

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

**Detailed API Specification**:

#### 1. `kernel_lisp_init`

```c
int kernel_lisp_init(struct kernel_lisp* kl, size_t heap_size, size_t stack_size);
```

**Description**:
Initializes a new isolated Lisp environment within the kernel.

1. Allocates continuous physical memory pages for the Heap (`kmalloc_pages`).
2. Maps pages to a secure virtual address range.
3. Initializes the Garbage Collector context (`gc_init`).
4. Creates the Root Environment (Global Symbol Table).
5. Registers core primitives (`defun`, `if`, etc.) and Kernel Bindings.

#### 2. `kernel_lisp_eval`

```c
lisp_value kernel_lisp_eval(struct kernel_lisp* kl, const char* expr_str);
```

**Description**:
Evaluates a string as Lisp code in the kernel context.

1. **Reads**: Parses `expr_str` into an AST (`lisp_read`).
2. **Compiles** (Optional): Converts AST to Bytecode (`lisp_compile`).
3. **Executes**: Runs the code on the VM (`vm_execute` or `eval`).
4. **Result**: Returns a tagged pointer to the result.
5. **Errors**: Captures `setjmp/longjmp` exceptions and returns condition object.

#### 3. `kernel_lisp_register_function`

```c
void kernel_lisp_register_function(struct kernel_lisp* kl, const char* name, int min_args, int max_args, lisp_value (*func)(struct kernel_lisp*, lisp_value), const char* doc);
```

**Description**:
Exposes a C kernel function to Lisp.

1. Interns the symbol `name` in the global environment.
2. Creates a `FUNCTION` object pointing to the C wrapper.
3. Binds symbol to function.
4. Stores arity metadata for runtime checking.

#### 4. `kernel_lisp_gc`

```c
void kernel_lisp_gc(struct kernel_lisp* kl);
```

**Description**:
Triggers a Garbage Collection cycle.

1. **Stop-The-World**: Pauses all threads executed in this Lisp Context (using `thread_suspend`).
2. **Mark Phase**:
   - Roots: VM Registers, Stack Frames, Global Symbol Table, C Handle Table.
   - Algorithm: Tricolor Mark-and-Sweep.
3. **Sweep Phase**:
   - Sweeps unused objects back to free list.
   - Compacts heap if fragmentation > threshold.
4. **Resume**: Resumes paused threads.

**Integration Algorithms**:

**Handle Management (C <-> Lisp Bridge)**:

- **Challenge**: C functions holding Lisp objects must prevent GC from collecting them.
- **Solution**: "Handle Scope" pattern.
  1.  On C entry: `scope = handle_scope_open()`.
  2.  Use `handle_new(obj)` to wrap Lisp pointers.
  3.  `handle_new` adds pointer to thread-local "Root Set".
  4.  On exit: `handle_scope_close(scope)` pops all handles from Root Set.

**Signal/Interrupt Safe Execution**:

- Lisp VM checks `atomic_flag interrupt_pending` at every backward jump or function call.
- If set:
  _ Saves VM state.
  _ Calls `process_interrupt_handler`. \* Restores VM state.
  This ensures Kernel Lisp code can be interrupted by Hardware IRQs without corruption.

---

_Document continues with Phases 7-12..._

---

## Phase 7: Device Drivers (Months 19-21)

### Timeline Overview

| Week  | Milestone           | Deliverables             |
| ----- | ------------------- | ------------------------ |
| 1-2   | USB Host Controller | UHCI, OHCI, EHCI, xHCI   |
| 3-4   | USB Device Classes  | HID, Mass Storage, Audio |
| 5-6   | Graphics Drivers    | VESA, Intel, AMD basic   |
| 7-8   | Audio Drivers       | AC'97, HDA               |
| 9-10  | Power Management    | ACPI, suspend/resume     |
| 11-12 | Peripheral Drivers  | RTC, CMOS, Serial ports  |

### 7.1 USB Host Controller Driver - Complete Implementation

#### Requirements

- **UHCI/OHCI/EHCI/xHCI**: Support all USB host controller interfaces
- **Hub Support**: USB hub detection and device enumeration
- **Power Management**: Port power control, suspend/resume
- **Transfer Types**: Control, bulk, interrupt, isochronous
- **Bandwidth Management**: Allocate bandwidth for periodic transfers
- **Hot-plug**: Handle device insertion/removal

#### Implementation Details

**Data Structures**:

```c
enum usb_speed {
    USB_SPEED_LOW = 0,      /* 1.5 Mbps */
    USB_SPEED_FULL = 1,     /* 12 Mbps */
    USB_SPEED_HIGH = 2,     /* 480 Mbps */
    USB_SPEED_SUPER = 3     /* 5 Gbps */
};

enum usb_transfer_type {
    USB_TRANSFER_CONTROL = 0,
    USB_TRANSFER_ISOCHRONOUS = 1,
    USB_TRANSFER_BULK = 2,
    USB_TRANSFER_INTERRUPT = 3
};

struct usb_endpoint {
    uint8_t address;
    enum usb_transfer_type type;
    uint16_t max_packet_size;
    uint8_t interval;           /* For interrupt/isochronous */
    bool toggle;                /* Data toggle bit */
};

struct usb_interface {
    uint8_t interface_number;
    uint8_t alternate_setting;
    uint8_t class_code;
    uint8_t subclass_code;
    uint8_t protocol;
    struct usb_endpoint* endpoints;
    uint8_t endpoint_count;
    void* driver;               /* Bound driver */
    struct usb_interface* next;
};

struct usb_device {
    uint8_t address;
    enum usb_speed speed;
    uint16_t vendor_id;
    uint16_t product_id;
    uint16_t device_version;
    uint8_t class_code;
    uint8_t subclass_code;
    uint8_t protocol;
    char manufacturer[64];
    char product[64];
    char serial_number[64];

    /* Configuration */
    uint8_t configuration;
    struct usb_interface* interfaces;
    uint8_t interface_count;

    /* Control endpoint */
    struct usb_endpoint ep0;

    /* Parent hub */
    struct usb_device* parent;
    uint8_t parent_port;

    /* For hubs */
    bool is_hub;
    uint8_t num_ports;
    struct usb_device* children[16];

    /* Host controller */
    struct usb_hc* hc;

    spinlock_t lock;
};

struct usb_request {
    uint8_t type;               /* Request type */
    uint8_t request;            /* Request code */
    uint16_t value;
    uint16_t index;
    uint16_t length;
};

struct usb_transfer {
    struct usb_device* device;
    struct usb_endpoint* endpoint;
    enum usb_transfer_type type;

    /* Buffer */
    void* buffer;
    size_t length;
    size_t actual_length;

    /* Setup packet (for control) */
    struct usb_request setup;

    /* Status */
    int status;
    bool complete;

    /* Callback */
    void (*callback)(struct usb_transfer*);
    void* context;

    /* Scheduling */
    struct usb_transfer* next;
};

struct usb_hc_ops {
    int (*init)(struct usb_hc* hc);
    int (*shutdown)(struct usb_hc* hc);
    int (*reset)(struct usb_hc* hc);
    int (*submit)(struct usb_hc* hc, struct usb_transfer* transfer);
    int (*cancel)(struct usb_hc* hc, struct usb_transfer* transfer);
    int (*port_status)(struct usb_hc* hc, int port, uint32_t* status);
    int (*port_reset)(struct usb_hc* hc, int port);
    int (*port_enable)(struct usb_hc* hc, int port, bool enable);
};

struct usb_hc {
    char name[32];
    uint32_t type;              /* UHCI, OHCI, EHCI, xHCI */
    uintptr_t base_addr;
    uint32_t irq;

    struct usb_hc_ops* ops;

    /* Root hub */
    uint8_t num_ports;
    struct usb_device* root_hub;

    /* Device management */
    struct usb_device* devices[128];
    uint8_t next_address;

    /* Transfer queues */
    struct usb_transfer* pending;
    struct usb_transfer* active;

    spinlock_t lock;
};
```

**Detailed API Specification**:

#### 1. `usb_init`

```c
int usb_init(void);
```

**Description**:
Initializes the USB Core Subsystem.

1. Scans PCI bus for xHCI/EHCI/UHCI controllers.
2. For each controller found:
   - Maps MMIO bars.
   - Allocates `usb_hc` structure.
   - Calls `hc->ops->init(hc)`.
   - Registers Root Hub.
3. Launches `usb_enumerator_thread`.

#### 2. `usb_control_transfer`

```c
int usb_control_transfer(struct usb_device* dev, struct usb_request* req, void* buf, size_t len);
```

**Description**:
Performs a synchronous Control Transfer on Endpoint 0.

1. Constructs Setup Packet (8 bytes).
2. **Setup Stage**: Sends Setup Packet to device.
3. **Data Stage**: If `len > 0`, sends/receives data payload (IN/OUT based on `req->type`).
4. **Status Stage**: Performs 0-length handshake packet (IN if Data-OUT, OUT if Data-IN).
5. returns bytes transferred or error code (STALL, TIMEOUT).

#### 3. `xhci_enqueue_trb`

```c
int xhci_enqueue_trb(struct xhci_ring* ring, struct xhci_trb* trb);
```

**Description**:
Adds a Transfer Request Block to the hardware ring.

1. Copies TRB content to `ring->ring_base[ring->enqueue_ptr]`.
2. Toggles Cycle Bit matching the Driver's Cycle State (PCS).
3. Increments Enqueue Pointer.
4. Logic for Ring Wrap-Around: If at end, insert Link TRB pointing to start.
5. **Doorbell**: Writes to Doorbell Register to notify xHCI controller.

#### 4. `usb_hub_scan_ports`

```c
int usb_hub_scan_ports(struct usb_device* hub);
```

**Description**:
Polled or Interrupt-driven port scanner.

1. Reads `PORT_STATUS` for all `hub->num_ports`.
2. If `PORT_CONNECT_STATUS_CHANGE` bit set:
   - Debounce port (wait 100ms).
   - Read status again.
   - If User Device: Trigger Enumeration.
   - If Disconnect: Trigger Removal.
3. Clears Change bits.

**Detailed Algorithmic Specifications**:

**USB Device Enumeration State Machine**:

1.  **Detection**:
    - Host detects device presence on Root Hub Port.
    - Port Status indicates `CurrentConnectStatus = 1`.
2.  **Reset**:
    - Host issues `PortReset`. Drive bus to SE0 for 50ms.
    - Device enters Default State (Address 0).
3.  **Get Descriptor (MaxPacketSize)**:
    - Send `GET_DESCRIPTOR_DEVICE` to Addr 0, Len 64.
    - Device replies with first 8 bytes.
    - Host extracts `bMaxPacketSize0` (8, 16, 32, or 64).
    - Host updates Ep0 context.
4.  **Set Address**:
    - Host allocates new unique address `A` (1..127).
    - Send `SET_ADDRESS(A)` to Addr 0.
    - Device acknowledges. Device acts on Addr `A` henceforth.
5.  **Get Full Descriptor**:
    - Send `GET_DESCRIPTOR_DEVICE` to Addr `A` (Full length).
    - Host parses VendorID, ProductID, Class.
6.  **Configuration**:
    - Send `GET_DESCRIPTOR_CONFIGURATION` (Head).
    - Get TotalLength.
    - Fetch User Configuration Blob (Config + Interfaces + Endpoints).
    - Parse Interfaces and match against `usb_driver_list` (Driver Binding).
    - Send `SET_CONFIGURATION`.

---

### 7.2 USB HID Class Driver - Complete Implementation

#### Requirements

- **Keyboard Support**: Full keyboard with all keys
- **Mouse Support**: Relative and absolute positioning
- **Generic HID**: Support for custom HID devices
- **Report Parsing**: Parse HID report descriptors
- **LED Control**: Caps/Num/Scroll lock LEDs

#### Implementation Details

**Data Structures**:

```c
struct hid_report_item {
    uint8_t type;               /* Main, Global, Local */
    uint8_t tag;
    int32_t value;
    uint8_t size;
};

struct hid_field {
    uint32_t usage_page;
    uint32_t usage_min;
    uint32_t usage_max;
    int32_t logical_min;
    int32_t logical_max;
    int32_t physical_min;
    int32_t physical_max;
    uint8_t report_size;        /* Bits per field */
    uint8_t report_count;       /* Number of fields */
    uint32_t flags;             /* Constant, Variable, Relative, etc. */
    struct hid_field* next;
};

struct hid_report {
    uint8_t id;
    uint8_t type;               /* Input, Output, Feature */
    struct hid_field* fields;
    uint16_t size;              /* Total size in bits */
    struct hid_report* next;
};

struct hid_device {
    struct usb_device* usb_dev;
    struct usb_interface* interface;
    struct usb_endpoint* in_endpoint;
    struct usb_endpoint* out_endpoint;

    /* Report descriptor */
    uint8_t* report_desc;
    size_t report_desc_len;
    struct hid_report* reports;

    /* Current state */
    uint8_t* report_buffer;
    size_t report_buffer_len;

    /* Callbacks */
    void (*input_handler)(struct hid_device*, struct hid_report*, void*);

    /* Keyboard state (if keyboard) */
    uint8_t keyboard_leds;
    uint8_t keyboard_modifiers;
    uint8_t keyboard_keys[6];

    /* Mouse state (if mouse) */
    int16_t mouse_x;
    int16_t mouse_y;
    int8_t mouse_wheel;
    uint8_t mouse_buttons;

    spinlock_t lock;
};

/* HID Usage Pages */
#define HID_UP_GENERIC_DESKTOP  0x01
#define HID_UP_KEYBOARD         0x07
#define HID_UP_LED              0x08
#define HID_UP_BUTTON           0x09

/* Desktop Usages */
#define HID_USAGE_POINTER       0x01
#define HID_USAGE_MOUSE         0x02
#define HID_USAGE_KEYBOARD      0x06
#define HID_USAGE_X             0x30
#define HID_USAGE_Y             0x31
#define HID_USAGE_WHEEL         0x38
```

**Functions to Implement**:

1. `hid_probe(struct usb_device* dev, struct usb_interface* intf)` - Probe HID device
2. `hid_disconnect(struct hid_device* hid)` - Disconnect HID device
3. `hid_parse_report_desc(struct hid_device* hid)` - Parse report descriptor
4. `hid_get_report(struct hid_device* hid, uint8_t type, uint8_t id, void* buf, size_t len)` - Get report
5. `hid_set_report(struct hid_device* hid, uint8_t type, uint8_t id, void* buf, size_t len)` - Set report
6. `hid_set_idle(struct hid_device* hid, uint8_t duration, uint8_t report_id)` - Set idle rate
7. `hid_set_protocol(struct hid_device* hid, uint8_t protocol)` - Set protocol (boot/report)
8. `hid_input_report(struct hid_device* hid, uint8_t* data, size_t len)` - Process input report
9. `hid_keyboard_event(struct hid_device* hid, uint8_t* data)` - Process keyboard report
10. `hid_mouse_event(struct hid_device* hid, uint8_t* data)` - Process mouse report
11. `hid_set_leds(struct hid_device* hid, uint8_t leds)` - Set keyboard LEDs

**Report Descriptor Parsing**:

```c
int hid_parse_report_desc(struct hid_device* hid) {
    uint8_t* p = hid->report_desc;
    uint8_t* end = p + hid->report_desc_len;

    /* Parser state */
    uint32_t usage_page = 0;
    uint32_t usage = 0;
    int32_t logical_min = 0;
    int32_t logical_max = 0;
    uint8_t report_size = 0;
    uint8_t report_count = 0;
    uint8_t report_id = 0;

    while (p < end) {
        uint8_t prefix = *p++;
        uint8_t type = (prefix >> 2) & 0x03;
        uint8_t tag = (prefix >> 4) & 0x0F;
        uint8_t size = prefix & 0x03;
        if (size == 3) size = 4;

        int32_t value = 0;
        for (int i = 0; i < size; i++) {
            value |= (*p++) << (i * 8);
        }

        switch (type) {
            case 0: /* Main */
                switch (tag) {
                    case 0x8: /* Input */
                    case 0x9: /* Output */
                    case 0xB: /* Feature */
                        /* Create field with current state */
                        break;
                }
                break;
            case 1: /* Global */
                switch (tag) {
                    case 0x0: usage_page = value; break;
                    case 0x1: logical_min = value; break;
                    case 0x2: logical_max = value; break;
                    case 0x7: report_size = value; break;
                    case 0x9: report_count = value; break;
                    case 0x8: report_id = value; break;
                }
                break;
            case 2: /* Local */
                switch (tag) {
                    case 0x0: usage = value; break;
                }
                break;
        }
    }
    return 0;
}
```

---

### 7.3 ACPI Power Management - Complete Implementation

#### Requirements

- **ACPI Table Parsing**: RSDP, RSDT/XSDT, FADT, DSDT, SSDT
- **AML Interpreter**: Execute ACPI Machine Language
- **Power States**: S0-S5 system states
- **Device Power**: D0-D3 device states
- **Thermal Management**: Temperature monitoring, cooling
- **Battery Management**: Battery status, charging
- **Button Events**: Power button, sleep button, lid

#### Implementation Details

**Data Structures**:

```c
/* ACPI table header */
struct acpi_header {
    char signature[4];
    uint32_t length;
    uint8_t revision;
    uint8_t checksum;
    char oem_id[6];
    char oem_table_id[8];
    uint32_t oem_revision;
    uint32_t creator_id;
    uint32_t creator_revision;
};

/* Root System Description Pointer */
struct acpi_rsdp {
    char signature[8];
    uint8_t checksum;
    char oem_id[6];
    uint8_t revision;
    uint32_t rsdt_address;
    /* ACPI 2.0+ */
    uint32_t length;
    uint64_t xsdt_address;
    uint8_t extended_checksum;
    uint8_t reserved[3];
};

/* Fixed ACPI Description Table */
struct acpi_fadt {
    struct acpi_header header;
    uint32_t facs_address;
    uint32_t dsdt_address;
    uint8_t reserved1;
    uint8_t preferred_pm_profile;
    uint16_t sci_interrupt;
    uint32_t smi_command;
    uint8_t acpi_enable;
    uint8_t acpi_disable;
    uint8_t s4bios_req;
    uint8_t pstate_control;
    uint32_t pm1a_event_block;
    uint32_t pm1b_event_block;
    uint32_t pm1a_control_block;
    uint32_t pm1b_control_block;
    /* More fields... */
};

/* ACPI namespace object */
struct acpi_object {
    char name[5];               /* 4 chars + null */
    uint8_t type;
    union {
        uint64_t integer;
        struct {
            void* data;
            size_t length;
        } buffer;
        struct {
            char* value;
            size_t length;
        } string;
        struct acpi_object** elements;
        struct {
            uint8_t* code;
            size_t length;
        } method;
    } value;
    struct acpi_object* parent;
    struct acpi_object* children;
    struct acpi_object* next;
};

/* AML interpreter state */
struct aml_state {
    uint8_t* code;
    uint8_t* pc;
    uint8_t* end;
    struct acpi_object* scope;
    struct acpi_object* locals[8];
    struct acpi_object* args[7];
    struct aml_state* return_to;
    uint64_t return_value;
};

/* Power state */
enum acpi_power_state {
    ACPI_STATE_S0,              /* Working */
    ACPI_STATE_S1,              /* Sleeping, CPU stops */
    ACPI_STATE_S2,              /* Sleeping, CPU off */
    ACPI_STATE_S3,              /* Suspend to RAM */
    ACPI_STATE_S4,              /* Suspend to disk */
    ACPI_STATE_S5               /* Soft off */
};

struct acpi_state {
    struct acpi_rsdp* rsdp;
    struct acpi_fadt* fadt;
    struct acpi_header* dsdt;
    struct acpi_object* namespace;

    /* PM registers */
    uint32_t pm1a_event;
    uint32_t pm1b_event;
    uint32_t pm1a_control;
    uint32_t pm1b_control;

    /* Current state */
    enum acpi_power_state power_state;

    /* Event handlers */
    void (*power_button_handler)(void);
    void (*sleep_button_handler)(void);
    void (*lid_handler)(bool open);

    spinlock_t lock;
};
```

**Functions to Implement**:

**Table Parsing**:

1. `acpi_init(void)` - Initialize ACPI
2. `acpi_find_rsdp(void)` - Find RSDP in memory
3. `acpi_parse_rsdt(struct acpi_rsdp* rsdp)` - Parse RSDT/XSDT
4. `acpi_find_table(const char* signature)` - Find table by signature
5. `acpi_validate_checksum(struct acpi_header* table)` - Validate checksum

**AML Interpreter**:

1. `aml_init(struct aml_state* state, uint8_t* code, size_t length)` - Initialize interpreter
2. `aml_execute(struct aml_state* state)` - Execute AML code
3. `aml_evaluate_name(struct aml_state* state, const char* name)` - Evaluate named object
4. `aml_call_method(struct acpi_object* method, struct acpi_object** args)` - Call method
5. `aml_parse_opcode(struct aml_state* state)` - Parse and execute opcode
6. `aml_resolve_name(struct aml_state* state, const char* name)` - Resolve name in namespace

**Power Management**:

1. `acpi_enter_sleep_state(enum acpi_power_state state)` - Enter sleep state
2. `acpi_wake_from_sleep(void)` - Wake from sleep
3. `acpi_get_battery_status(struct acpi_battery_status* status)` - Get battery status
4. `acpi_get_thermal_zone(const char* name, struct acpi_thermal* thermal)` - Get thermal zone
5. `acpi_set_cooling_policy(int policy)` - Set cooling policy
6. `acpi_handle_event(uint32_t event)` - Handle ACPI event

**Power State Transition**:

```c
int acpi_enter_sleep_state(enum acpi_power_state state) {
    uint16_t slp_typa, slp_typb;

    /* Get SLP_TYP values from _Sx method */
    char method_name[5];
    snprintf(method_name, 5, "\\_S%d", state);
    struct acpi_object* sx = aml_resolve_name(NULL, method_name);
    if (!sx) return -ENOENT;

    slp_typa = sx->value.elements[0]->value.integer;
    slp_typb = sx->value.elements[1]->value.integer;

    /* Prepare for sleep */
    acpi_call_method("\\_PTS", state);

    /* Save CPU state */
    save_cpu_state();

    /* Disable interrupts */
    cli();

    /* Set SLP_TYP and SLP_EN */
    outw(acpi_state.pm1a_control, (slp_typa << 10) | (1 << 13));
    if (acpi_state.pm1b_control) {
        outw(acpi_state.pm1b_control, (slp_typb << 10) | (1 << 13));
    }

    /* Should not reach here for S3/S4/S5 */
    /* For S1/S2, wait for wake */
    sti();

    /* Wake handlers */
    acpi_call_method("\\_WAK", state);

    return 0;
}
```

---

---

### 7.4 Graphics Drivers - Complete Implementation

#### Requirements

- **VESA VBE**: Fallback linear framebuffer support.
- **Intel Native**: Hardware acceleration for Intel HD Graphics (Gen 5+).
- **Command Submission**: Ring buffer management for GPU commands.
- **Memory Management**: GTT (Graphics Translation Table) handling.
- **Hardware Cursor**: Hardware-managed cursor plane.

#### Implementation Details

**Data Structures**:

```c
/* VESA VBE Mode Info */
struct vbe_mode_info {
    uint16_t attributes;
    uint8_t window_a;
    uint8_t window_b;
    uint16_t granularity;
    uint16_t window_size;
    uint16_t segment_a;
    uint16_t segment_b;
    uint32_t win_func_ptr;
    uint16_t pitch;
    uint16_t width;
    uint16_t height;
    uint8_t w_char;
    uint8_t y_char;
    uint8_t planes;
    uint8_t bpp;
    uint8_t banks;
    uint8_t memory_model;
    uint8_t bank_size;
    uint8_t image_pages;
    uint8_t reserved1;
    uint8_t red_mask_size;
    uint8_t red_field_position;
    uint8_t green_mask_size;
    uint8_t green_field_position;
    uint8_t blue_mask_size;
    uint8_t blue_field_position;
    uint8_t rsvd_mask_size;
    uint8_t rsvd_field_position;
    uint8_t direct_color_mode_info;
    uint32_t phys_base;
    uint32_t reserved2;
    uint16_t reserved3;
} __attribute__((packed));

/* GPU Command Ring */
struct gpu_ring {
    uint32_t* virtual_base;
    uint32_t length;        /* In bytes */
    uint32_t head;          /* Offset */
    uint32_t tail;          /* Offset */
    uint32_t control_reg;   /* MMIO offset */
    spinlock_t lock;
};

/* Intel GPU Context */
struct intel_gpu {
    struct pci_device* pci;
    void* mmio_base;
    uintptr_t gtt_base;
    struct gpu_ring render_ring;
    struct gpu_ring blitter_ring;
    struct gpu_ring video_ring;

    /* Display Planes */
    struct display_plane* planes;
    int num_planes;

    /* Connectors */
    struct gpu_connector* connectors;
    int num_connectors;
};
```

**Functions to Implement**:

#### 1. `gpu_init_intel`

```c
int gpu_init_intel(struct pci_device* pci);
```

**Description**:
Initializes Intel HD Graphics hardware.

1. Enable Bus Mastering and Memory Access in PCI Command Register.
2. Map MMIO BAR (BAR0) and GTT (often part of BAR0 or separate).
3. Reset GPU Core (set/clear Reset bit in `GT_CTRL`).
4. Initialize GTT (clear all entries to scratch page).
5. Initialize Render Ring Buffer:
   - Allocate 4KB aligned page.
   - Write base address to `RCS_RING_BASE`.
   - Set length in `RCS_RING_LEN`.
   - Enable ring in `RCS_RING_CTL`.

#### 2. `gpu_submit_command`

```c
int gpu_submit_command(struct gpu_ring* ring, uint32_t* batch, size_t count);
```

**Description**:
Writes commands to the circular ring buffer.

1. Acquire ring lock.
2. Check for space: `(tail + size) % len != head`.
3. If full, wait (poll head register).
4. Copy `batch` commands to `ring->virtual_base + tail`.
5. Update `tail = (tail + size * 4) % len`.
6. Write new tail to `RING_TAIL` register.
   - Note: Some HW requires cache flush or specific alignment.

#### 3. `gpu_set_mode`

```c
int gpu_set_mode(struct intel_gpu* gpu, int width, int height, int bpp);
```

**Description**:
Sets the display pipeline (Pipes, Transcoders, Planes).

1. Disable Planes and Pipes.
2. Configure PLLs (Frequencies) for target resolution dot clock.
3. Configure Transcoder timings (HTotal, VTotal, Syncs).
4. Enable Pipe.
5. Configure Plane to scan out from Framebuffer address.
6. Enable Plane.

**Algorithmic Details**:

**Dirty Rectangle Tracking (Software Fallback)**:

1. Maintain a `dirty_region` list for the framebuffer.
2. Drawing primitives (`rect`, `line`, `text`) union their bounds into `dirty_region`.
3. `compositor_thread` wakes up on VSync.
4. If `dirty_region` is not empty:
   - Copy only dirty rects from Back Buffer to Front Buffer (VRAM).
   - Or, issue Blit commands to GPU if available.
   - Clear `dirty_region`.

---

### 7.5 Audio Drivers - Complete Implementation

#### Requirements

- **Intel HDA**: High Definition Audio specification support.
- **AC'97**: Legacy support (optional, but good for VMs).
- **Codec Parsing**: Widget graph traversal.
- **PCM Streams**: Playback and Capture DMA engines.
- **Mixing**: Software mixing of multiple streams.

#### Implementation Details

**Data Structures**:

```c
/* HDA Controller Registers */
#define HDA_GCAP      0x00
#define HDA_VMIN      0x02
#define HDA_VMAJ      0x03
#define HDA_OUTPAY    0x04
#define HDA_INPAY     0x06
#define HDA_GCTL      0x08
#define HDA_WAKEEN    0x0C
#define HDA_STATESTS  0x0E
#define HDA_INTCTL    0x20
#define HDA_INTSTS    0x24
#define HDA_WALCLK    0x30
#define HDA_CORB_BASE 0x40
#define HDA_RIRB_BASE 0x50

/* HDA Stream Descriptor */
struct hda_stream_desc {
    uint32_t ctl_sts;   /* Control / Status */
    uint32_t lpib;      /* Link Position in Buffer */
    uint32_t cbl;       /* Cyclic Buffer Length */
    uint32_t lvi;       /* Last Valid Index */
    uint32_t fmt;       /* Format */
    uint32_t bdl_lower; /* Buffer Descriptor List Base Lower */
    uint32_t bdl_upper; /* Buffer Descriptor List Base Upper */
};

/* Buffer Descriptor List Entry */
struct hda_bdle {
    uint32_t lower_addr;
    uint32_t upper_addr;
    uint32_t length;
    uint32_t flags;     /* IOC: Interrupt On Completion */
} __attribute__((packed));

struct hda_device {
    uintptr_t base_addr;
    uint32_t* corb_base;
    uint64_t* rirb_base;
    uint16_t corb_wp;
    uint16_t rirb_rp;
    int num_output_streams;
    int num_input_streams;
    struct hda_stream_desc* streams;
};
```

**Functions to Implement**:

#### 1. `hda_init`

```c
int hda_init(struct pci_device* pci);
```

**Description**:
Initializes HDA Controller.

1. Map MMIO.
2. Reset Controller: Clear `CRST` bit in `GCTL`, wait, Set `CRST`, wait.
3. Detect capabilities: Read `GCAP` (Number of streams).
4. Initialize CORB (Command Outbound Ring Buffer) and RIRB (Response Inbound Ring Buffer).
   - Allocate DMA buffers.
   - Set `CORBLBASE` / `RIRBLBASE`.
   - Start CORB/RIRB DMA engines.

#### 2. `hda_send_verb`

```c
uint32_t hda_send_verb(struct hda_device* dev, uint32_t codec, uint32_t node, uint32_t payload);
```

**Description**:
Sends a verb to a codec and waits for response.

1. Construct 32-bit command: `(codec << 28) | (node << 20) | payload`.
2. Write to `CORB[write_ptr]`.
3. Increment `write_ptr`.
4. Poll `RIRB` write pointer (hardware update) or interrupt.
5. Read response from `RIRB[read_ptr]`.

#### 3. `hda_setup_stream`

```c
int hda_setup_stream(struct hda_device* dev, int stream_id, struct pcm_buffer* buf);
```

**Description**:
Configures a DMA stream for playback.

1. Reset Stream: Set `SRST` bit in `SDnCTL`.
2. Setup BDL (Buffer Descriptor List):
   - Break linear buffer into physical pages.
   - Create BDLEs for each page.
3. Set Cyclic Buffer Length (`CBL`).
4. Set Stream Format (`FMT`): 48kHz, 16-bit, Stereo (`0x4011`).
5. Enable Stream: Set `RUN` bit.

**Algorithmic Details**:

**Codec Enumeration**:

1. Read `STATESTS` register. Bits 0-15 indicate detected codecs.
2. For each bit set (e.g., bit 0 -> Codec 0):
   - Send `GET_PARAM(Root Node, VENDOR_ID)`.
   - Send `GET_PARAM(Root Node, REVISION_ID)`.
   - Parse Widget Graph:
     - Start at Root Node.
     - Enumerate Audio Function Groups.
     - For each Group, enumerate Widgets (Pin Complex, DAC, ADC, Mixer).
     - Find Path: DAC -> Mixer -> Pin Complex (Headphone/Line Out).
     - Unmute all widgets in path.

## Phase 8: Testing and Quality Assurance (Months 22-24)

### Timeline Overview

| Week  | Milestone              | Deliverables              |
| ----- | ---------------------- | ------------------------- |
| 1-2   | Unit Testing Framework | Test harness, assertions  |
| 3-4   | Kernel Unit Tests      | PMM, VMM, scheduler tests |
| 5-6   | Integration Tests      | Multi-component tests     |
| 7-8   | System Tests           | Full system scenarios     |
| 9-10  | Performance Testing    | Benchmarks, profiling     |
| 11-12 | Stress Testing         | Long-running, edge cases  |

### 8.1 Unit Testing Framework - Complete Implementation

#### Requirements

- **Test Discovery**: Automatic test detection
- **Assertions**: Rich assertion library
- **Fixtures**: Setup/teardown support
- **Mocking**: Mock functions and objects
- **Coverage**: Code coverage reporting
- **Reporting**: Test results and statistics

#### Implementation Details

**Data Structures**:

```c
/* Test result */
enum test_status {
    TEST_PASSED,
    TEST_FAILED,
    TEST_SKIPPED,
    TEST_ERROR
};

struct test_result {
    const char* test_name;
    const char* suite_name;
    enum test_status status;
    const char* message;
    const char* file;
    int line;
    uint64_t duration_us;
    struct test_result* next;
};

/* Test case */
struct test_case {
    const char* name;
    void (*test_fn)(struct test_context*);
    void (*setup)(struct test_context*);
    void (*teardown)(struct test_context*);
    bool skip;
    const char* skip_reason;
    struct test_case* next;
};

/* Test suite */
struct test_suite {
    const char* name;
    struct test_case* tests;
    void (*suite_setup)(void);
    void (*suite_teardown)(void);
    uint32_t test_count;
    uint32_t passed;
    uint32_t failed;
    uint32_t skipped;
    struct test_suite* next;
};

/* Test context */
struct test_context {
    struct test_suite* suite;
    struct test_case* test;
    struct test_result* result;
    void* fixture;              /* Test-specific data */
    jmp_buf fail_jump;
    bool failed;
};

/* Mock function */
struct mock_call {
    const char* function_name;
    void** args;
    size_t arg_count;
    void* return_value;
    struct mock_call* next;
};

struct mock_function {
    const char* name;
    void* original;
    void* mock;
    struct mock_call* expected_calls;
    struct mock_call* actual_calls;
    uint32_t call_count;
};

/* Test statistics */
struct test_stats {
    uint32_t total_suites;
    uint32_t total_tests;
    uint32_t passed;
    uint32_t failed;
    uint32_t skipped;
    uint32_t errors;
    uint64_t total_duration_us;
};
```

**Assertion Macros**:

```c
#define ASSERT_TRUE(expr) \
    do { \
        if (!(expr)) { \
            test_fail(ctx, #expr " is not true", __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_FALSE(expr) \
    do { \
        if (expr) { \
            test_fail(ctx, #expr " is not false", __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_EQ(expected, actual) \
    do { \
        if ((expected) != (actual)) { \
            test_fail_eq(ctx, #expected, #actual, \
                         (int64_t)(expected), (int64_t)(actual), \
                         __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_NE(expected, actual) \
    do { \
        if ((expected) == (actual)) { \
            test_fail_ne(ctx, #expected, #actual, \
                         (int64_t)(expected), __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_NULL(ptr) \
    ASSERT_EQ(NULL, ptr)

#define ASSERT_NOT_NULL(ptr) \
    ASSERT_NE(NULL, ptr)

#define ASSERT_STR_EQ(expected, actual) \
    do { \
        if (strcmp(expected, actual) != 0) { \
            test_fail_str(ctx, expected, actual, __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_MEM_EQ(expected, actual, size) \
    do { \
        if (memcmp(expected, actual, size) != 0) { \
            test_fail_mem(ctx, expected, actual, size, __FILE__, __LINE__); \
        } \
    } while (0)

#define ASSERT_IN_RANGE(value, min, max) \
    do { \
        if ((value) < (min) || (value) > (max)) { \
            test_fail_range(ctx, #value, value, min, max, __FILE__, __LINE__); \
        } \
    } while (0)

#define TEST(suite, name) \
    static void test_##suite##_##name(struct test_context* ctx); \
    static struct test_case __test_##suite##_##name = { \
        .name = #name, \
        .test_fn = test_##suite##_##name, \
    }; \
    __attribute__((constructor)) static void __register_##suite##_##name(void) { \
        test_register_case(#suite, &__test_##suite##_##name); \
    } \
    static void test_##suite##_##name(struct test_context* ctx)
```

**Functions to Implement**:

1. `test_init(void)` - Initialize testing framework
2. `test_register_suite(struct test_suite* suite)` - Register test suite
3. `test_register_case(const char* suite, struct test_case* test)` - Register test case
4. `test_run_all(void)` - Run all registered tests
5. `test_run_suite(const char* name)` - Run specific suite
6. `test_run_case(struct test_suite* suite, struct test_case* test)` - Run single test
7. `test_fail(struct test_context* ctx, const char* msg, const char* file, int line)` - Record failure
8. `test_skip(struct test_context* ctx, const char* reason)` - Skip test
9. `test_get_stats(struct test_stats* stats)` - Get statistics
10. `test_print_results(void)` - Print test results
11. `mock_create(const char* name, void* mock_fn)` - Create mock function
12. `mock_expect_call(struct mock_function* mock, ...)` - Expect call with args
13. `mock_verify(struct mock_function* mock)` - Verify mock expectations
14. `mock_reset(struct mock_function* mock)` - Reset mock

---

### 8.2 Kernel Unit Tests - Complete Implementation

#### Memory Manager Tests

```c
/* PMM Tests */
TEST(pmm, alloc_single_frame) {
    uintptr_t frame = pmm_alloc_frame();
    ASSERT_NOT_NULL((void*)frame);
    ASSERT_TRUE(frame % PAGE_SIZE == 0);
    pmm_free_frame(frame);
}

TEST(pmm, alloc_multiple_frames) {
    uintptr_t frames[100];
    for (int i = 0; i < 100; i++) {
        frames[i] = pmm_alloc_frame();
        ASSERT_NOT_NULL((void*)frames[i]);
    }
    /* Verify all different */
    for (int i = 0; i < 100; i++) {
        for (int j = i + 1; j < 100; j++) {
            ASSERT_NE(frames[i], frames[j]);
        }
    }
    for (int i = 0; i < 100; i++) {
        pmm_free_frame(frames[i]);
    }
}

TEST(pmm, alloc_contiguous) {
    uintptr_t base = pmm_alloc_frames(16);
    ASSERT_NOT_NULL((void*)base);
    /* Verify contiguous */
    for (int i = 0; i < 16; i++) {
        ASSERT_TRUE(pmm_check_frame(base + i * PAGE_SIZE) == 0);
    }
    pmm_free_frames(base, 16);
}

TEST(pmm, exhaust_memory) {
    size_t count = 0;
    uintptr_t* frames = kmalloc(1000000 * sizeof(uintptr_t));
    while (1) {
        uintptr_t frame = pmm_alloc_frame();
        if (!frame) break;
        frames[count++] = frame;
    }
    ASSERT_TRUE(count > 0);
    for (size_t i = 0; i < count; i++) {
        pmm_free_frame(frames[i]);
    }
    kfree(frames);
}

/* VMM Tests */
TEST(vmm, map_single_page) {
    struct address_space* as = vmm_create_address_space(1);
    ASSERT_NOT_NULL(as);

    uintptr_t virt = 0x40000000;
    uintptr_t phys = pmm_alloc_frame();

    int ret = vmm_map_page(as, virt, phys, PAGE_PRESENT | PAGE_WRITABLE);
    ASSERT_EQ(0, ret);

    uintptr_t mapped = vmm_get_physical(as, virt);
    ASSERT_EQ(phys, mapped);

    vmm_unmap_page(as, virt);
    vmm_destroy_address_space(as);
    pmm_free_frame(phys);
}

TEST(vmm, copy_on_write) {
    struct address_space* src = vmm_create_address_space(1);
    struct address_space* dst = vmm_create_address_space(2);

    uintptr_t virt = 0x40000000;
    uintptr_t phys = pmm_alloc_frame();

    vmm_map_page(src, virt, phys, PAGE_PRESENT | PAGE_WRITABLE);

    /* Write test data */
    vmm_switch_address_space(src);
    *(uint32_t*)virt = 0xDEADBEEF;

    /* COW copy */
    vmm_copy_on_write(src, dst, virt, PAGE_SIZE);

    /* Verify both see same data */
    vmm_switch_address_space(dst);
    ASSERT_EQ(0xDEADBEEF, *(uint32_t*)virt);

    /* Write to copy triggers COW */
    *(uint32_t*)virt = 0x12345678;

    /* Verify source unchanged */
    vmm_switch_address_space(src);
    ASSERT_EQ(0xDEADBEEF, *(uint32_t*)virt);

    vmm_destroy_address_space(src);
    vmm_destroy_address_space(dst);
}
```

#### Scheduler Tests

```c
TEST(scheduler, add_remove_thread) {
    struct thread* t = thread_create(NULL, test_thread_fn);
    ASSERT_NOT_NULL(t);

    int ret = scheduler_add_thread(t);
    ASSERT_EQ(0, ret);

    ret = scheduler_remove_thread(t);
    ASSERT_EQ(0, ret);

    thread_destroy(t);
}

TEST(scheduler, priority_scheduling) {
    struct thread* low = thread_create(NULL, priority_test_fn);
    struct thread* high = thread_create(NULL, priority_test_fn);

    low->priority = PRIORITY_LOW;
    high->priority = PRIORITY_HIGH;

    scheduler_add_thread(low);
    scheduler_add_thread(high);

    /* High priority should run first */
    struct thread* next = scheduler_schedule();
    ASSERT_EQ(high, next);

    scheduler_remove_thread(low);
    scheduler_remove_thread(high);
    thread_destroy(low);
    thread_destroy(high);
}

TEST(scheduler, round_robin) {
    struct thread* t1 = thread_create(NULL, test_fn);
    struct thread* t2 = thread_create(NULL, test_fn);
    struct thread* t3 = thread_create(NULL, test_fn);

    t1->priority = t2->priority = t3->priority = PRIORITY_NORMAL;

    scheduler_add_thread(t1);
    scheduler_add_thread(t2);
    scheduler_add_thread(t3);

    /* Should cycle through threads */
    ASSERT_EQ(t1, scheduler_schedule());
    scheduler_yield();
    ASSERT_EQ(t2, scheduler_schedule());
    scheduler_yield();
    ASSERT_EQ(t3, scheduler_schedule());
    scheduler_yield();
    ASSERT_EQ(t1, scheduler_schedule());

    scheduler_remove_thread(t1);
    scheduler_remove_thread(t2);
    scheduler_remove_thread(t3);
}
```

---

### 8.3 Performance Testing - Complete Implementation

#### Requirements

- **Benchmarks**: Standardized performance tests
- **Profiling**: Function-level timing
- **Memory Profiling**: Allocation tracking
- **CPU Profiling**: Instruction counting
- **I/O Profiling**: Throughput measurement
- **Regression Detection**: Compare against baseline

#### Implementation Details

**Data Structures**:

```c
struct benchmark {
    const char* name;
    const char* description;
    void (*setup)(void);
    void (*run)(struct benchmark_result*);
    void (*teardown)(void);
    uint32_t iterations;
    uint64_t target_ops;        /* Target ops/sec */
};

struct benchmark_result {
    const char* name;
    uint64_t iterations;
    uint64_t total_time_ns;
    uint64_t min_time_ns;
    uint64_t max_time_ns;
    uint64_t avg_time_ns;
    double ops_per_sec;
    double throughput_mbps;
    struct benchmark_result* next;
};

struct profiler_entry {
    const char* function;
    uint64_t call_count;
    uint64_t total_time_ns;
    uint64_t min_time_ns;
    uint64_t max_time_ns;
    uint64_t self_time_ns;
    uint64_t children_time_ns;
    struct profiler_entry* parent;
    struct profiler_entry* children;
    struct profiler_entry* next;
};

struct memory_profile {
    size_t total_allocated;
    size_t total_freed;
    size_t peak_usage;
    size_t current_usage;
    uint64_t alloc_count;
    uint64_t free_count;
    struct allocation_record* live_allocations;
};

struct allocation_record {
    void* address;
    size_t size;
    const char* file;
    int line;
    uint64_t timestamp;
    struct allocation_record* next;
};
```

**Benchmarks**:

```c
/* Memory allocation benchmark */
BENCHMARK(memory, malloc_free_cycle, 1000000) {
    void* p = kmalloc(64);
    kfree(p);
}

/* Context switch benchmark */
BENCHMARK(scheduler, context_switch, 100000) {
    scheduler_yield();
}

/* Page fault benchmark */
BENCHMARK(vmm, page_fault_handling, 10000) {
    /* Touch new page to trigger fault */
    volatile uint8_t* p = (uint8_t*)(0x80000000 + result->iteration * PAGE_SIZE);
    *p = 0;
}

/* File I/O benchmark */
BENCHMARK(filesystem, sequential_read, 1000) {
    char buf[4096];
    int fd = open("/benchmark/file", O_RDONLY);
    while (read(fd, buf, sizeof(buf)) > 0);
    close(fd);
}

/* Network benchmark */
BENCHMARK(network, tcp_throughput, 100) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    connect(sock, &server_addr, sizeof(server_addr));
    char buf[65536];
    for (int i = 0; i < 1000; i++) {
        send(sock, buf, sizeof(buf), 0);
    }
    close(sock);
}
```

**Detailed API Specification**:

#### 1. `benchmark_run`

```c
struct benchmark_result benchmark_run(const char* name);
```

**Description**:
Executes a registered benchmark.

1. **Warmup**: Runs `setup` and performs 10% of iterations to warm instruction cache.
2. **Measurement**:
   - `start = rdtsc()`.
   - Loop `iterations` times calling body.
   - `end = rdtsc()`.
3. **Analysis**:
   - Computes `avg_cycles`, `ops_per_sec`.
   - Stores result in linked list.

#### 2. `profiler_report`

```c
void profiler_report(void);
```

**Description**:
Dumps call graph and timing data.

1. Walks the `profiler_entry` tree.
2. For each node, prints:
   - Function Name
   - Call Count
   - Total/Self/Children Time
   - % of parent time
3. Formats as hierarchical text or JSON for external tools (e.g., FlameGraph).

---

### 8.4 Kernel Fuzzing - Complete Implementation

#### Requirements

- **Coverage-Guided Fuzzing**: Integrate with KCOV-like interface
- **Syzkaller Support**: Expose syscall descriptions
- **Sanitizers**: KASAN (Address), KUBSAN (Undefined Behavior)
- **Fault Injection**: Randomly fail allocations or IO

#### Implementation Details

**Data Structures**:

```c
struct kcov_area {
    uint64_t count;
    uint64_t pcs[KCOV_MAX_PCS];
};

struct kasan_meta {
    uint8_t shadow_byte; /* 0=Good, 0xFA=RedZone, 0xFD=Freed */
};
```

**Functions to Implement**:

1. `kcov_init(void)` - Initialize coverage tracker
2. `kcov_remote_start(uint64_t handle)` - Start remote tracing
3. `kcov_remote_stop(void)` - Stop remote tracing
4. `__sanitizer_cov_trace_pc(void)` - Compiler hook for PC tracing
5. `kasan_init(void)` - Initialize Shadow Memory (1/8th of RAM)
6. `kasan_poison(void* addr, size_t size, uint8_t value)` - Poison memory range
7. `kasan_unpoison(void* addr, size_t size)` - Mark memory as valid
8. `kasan_report(uintptr_t addr, size_t size, bool write)` - Report violation

**Fuzzing Strategy (Syzkaller Integration)**:

1.  **Syscall Description**: Define AST for all 150+ syscalls in Syzkaller's `lang` format.
    - `open(file ptr[in, string], flags flags[open_flags], mode const[0]) fd`
2.  **Executor**: Implement a small userspace agent (`syz-executor`) that:
    - Maps `kcov` file.
    - Executes shared memory bytecode from Fuzzer.
    - Feeds coverage back to Fuzzer.
3.  **Reproducer**: Logic to re-run crashing inputs deterministically.

---

### 8.5 System Integration Scenarios - Detailed

#### Scenario 1: Heavy Network Load (The 'C10K' Test)

- **Setup**: Spawn 100 threads, each opening 100 connections to loopback server.
- **Action**: Send 1KB random data ping-pong.
- **Validation**:
  - No kernel panic (OOM or Deadlock).
  - All data verified via CRC32.
  - Throughput > 1 Gbps loopback.
  - No fd leaks (check `ls /proc/sys/fs/file-nr`).

#### Scenario 2: Filesystem Journal Recovery

- **Setup**: Mount LFSX. Start writing 1GB file.
- **Action**: Hard Reset VM (simulated power loss) at 50% progress.
- **Validation**:
  - Mount LFSX.
  - Journal Replay runs successfully.
  - Filesystem consistent (fsck pass).
  - Data up to last atomic commit is present.

#### Scenario 3: Memory Pressure & Swapping

- **Setup**: Limit RAM to 256MB.
- **Action**: Spawn process allocating 512MB initialized data.
- **Validation**:
  - Swap file usage increases.
  - No OOM Kill of critical services (Init, WindowServer).
  - Process completes verify check of memory.

---

## Phase 9: Documentation (Months 25-26)

### Timeline Overview

| Week | Milestone         | Deliverables                |
| ---- | ----------------- | --------------------------- |
| 1-2  | API Documentation | All public APIs documented  |
| 3-4  | User Guide        | Installation, configuration |
| 5-6  | Developer Guide   | Architecture, contributing  |
| 7-8  | Tutorials         | Step-by-step guides         |

### 9.1 Documentation Standards

#### Requirements

- **API Reference**: Complete documentation of all public APIs
- **Architecture Guide**: System design and internals
- **User Manual**: Installation and usage
- **Developer Guide**: Contributing and development
- **Tutorials**: Learning resources
- **Man Pages**: Unix-style documentation

#### Documentation Format

**API Documentation Template (Doxygen)**:

```c
/**
 * @brief Brief description of the function.
 *
 * Detailed description of the function's behavior, including
 * side effects, thread safety guarantees, and algorithmic complexity.
 *
 * @param[in] param1 Description of input parameter.
 * @param[out] param2 Description of output parameter.
 * @return Description of return value.
 * @retval 0 Success
 * @retval -EINVAL Invalid argument
 *
 * @note Important usage notes.
 * @warning Critical warnings.
 */
return_type function_name(param_type1 param1, param_type2 param2);
```

**Architecture Decision Records (ADR)**:

- **Title**: Short noun phrase.
- **Status**: Proposed, Accepted, Deprecated.
- **Context**: What is the issue?
- **Decision**: The change that we are making.
- **Consequences**: Positive and negative impacts.

#### Man Page Generation

- Tools: `pandoc` or `ronn`.
- Source: Markdown in `docs/man/`.
- Output: Troff format installed to `/usr/share/man/`.

**System Call Documentation**:

````markdown
# syscall_name(2)

## NAME

syscall_name - brief description

## SYNOPSIS

```c
#include <sys/syscall.h>

long syscall_name(arg_type1 arg1, arg_type2 arg2);
```
````

## DESCRIPTION

Full description of the system call.

## ARGUMENTS

- `arg1`: Description
- `arg2`: Description

## RETURN VALUE

On success, returns X. On error, returns -1 and sets errno.

## ERRORS

- `EFAULT`: Invalid memory address
- `EINVAL`: Invalid argument

## CONFORMING TO

POSIX.1-2008, Linux

## BUGS

Known issues or limitations.

## EXAMPLE

```c
if (syscall_name(arg1, arg2) < 0) {
    perror("syscall_name");
    exit(1);
}
```

## SEE ALSO

related_syscall(2), related_function(3)

````

---

*Document continues with Phases 10-12...*


### 9.2 Tutorials

#### Tutorial 1: Writing a Hello World Driver

**Goal**: Create a kernel module that prints to the debug log.

**Steps**:

1.  **Create Driver Structure**:
    ```c
    #include <kernel/driver.h>

    static int hello_init(void) {
        kprintf("Hello, Kernel World!\n");
        return 0;
    }

    static void hello_exit(void) {
        kprintf("Goodbye, Kernel World!\n");
    }

    struct driver hello_driver = {
        .name = "hello",
        .init = hello_init,
        .exit = hello_exit,
    };

    DRIVER_EXPORT(hello_driver);
    ```

2.  **Build System**:
    Add to `drivers/Makefile`:
    ```makefile
    obj-$(CONFIG_HELLO) += hello.o
    ```

3.  **Loading**:
    In Lisp shell:
    ```lisp
    (kernel:load-module "hello")
    ;; Output: Hello, Kernel World!
    ```

#### Tutorial 2: Creating a Lisp GUI Application

**Goal**: Create a window with a button that clicks.

**Code**:
```lisp
(defpackage :my-app
  (:use :cl :ui))

(in-package :my-app)

(defun on-click (btn)
  (ui:alert "Button Clicked!"))

(defun main ()
  (let ((win (ui:window :title "My App" :width 400 :height 300))
        (btn (ui:button :text "Click Me" :on-click #'on-click)))
    (ui:add-child win btn)
    (ui:show win)))
```

---

## Phase 10: Advanced OS Features (Months 25-28)

### Timeline Overview

| Week  | Milestone              | Deliverables              |
| ----- | ---------------------- | ------------------------- |
| 1-4   | SMP Support            | Multi-core boot, per-CPU  |
| 5-8   | Advanced Scheduler     | Load balancing, affinity  |
| 9-12  | Virtualization         | VT-x/SVM enabled VMM      |
| 13-16 | Real-time Extensions   | Preemption, PI mutexes    |

### 10.1 Symmetric Multiprocessing (SMP) - Complete Implementation

#### Requirements

- **Multi-core Boot**: Wake up Application Processors (APs)
- **Per-CPU Data**: CPU-local storage
- **Locking**: Spinlocks, Reader-Writer locks, RCU
- **IPI**: Inter-Processor Interrupts
- **TLB Shootdown**: Coordinated TLB invalidation

#### Implementation Details

**Data Structures**:

```c
/* Per-CPU data area */
struct per_cpu_data {
    struct cpu* self;           /* Pointer to self */
    struct thread* current;     /* Current thread */
    struct thread* idle;        /* Idle thread */
    uint64_t apic_id;           /* Local APIC ID */
    uint32_t cpu_id;            /* Logical CPU ID */
    int preemption_depth;       /* Preemption counter */
    int interrupt_depth;       /* Interrupt nesting */

    /* Scheduler runqueue */
    struct runqueue runqueue;

    /* Statistics */
    uint64_t switches;
    uint64_t interrupts;
    uint64_t timer_ticks;
} __attribute__((aligned(PAGE_SIZE)));

/* CPU Descriptor */
struct cpu {
    struct per_cpu_data* data;
    bool online;
    bool active;
    uint32_t package_id;
    uint32_t core_id;
    struct cpu* next;
};

/* Spinlock */
typedef struct {
    volatile uint32_t value;
    uint32_t flags;
    struct cpu* owner;
#ifdef DEBUG_LOCKS
    const char* name;
    const char* file;
    int line;
#endif
} spinlock_t;

/* RW Lock */
typedef struct {
    volatile uint32_t state;    /* High bit: write, Low bits: readers */
} rwlock_t;

/* IPI Messages */
enum ipi_type {
    IPI_TLB_SHOOTDOWN,
    IPI_RESCHEDULE,
    IPI_CALL_FUNCTION,
    IPI_STOP_CPU
};

struct ipi_message {
    enum ipi_type type;
    void (*func)(void*);
    void* info;
    volatile int target_count;
    volatile int processed_count;
    spinlock_t lock;
};
````

**Detailed API Specification**:

#### 1. `smp_boot_aps`

```c
void smp_boot_aps(void);
```

**Description**:
Boots Application Processors (APs).

1. **Trampoline**: Copies 16-bit trampoline code to low memory (e.g., `0x8000`).
2. **INIT IPI**: Sends INIT IPI to target core APIC ID.
3. **Delay**: Waits 10ms.
4. **SIPI**: Sends Start-Up IPI (SIPI) with vector `0x08` (Start at `0x8000`).
5. **Wait**: Checks `cpu->online` flag.
6. **Retry**: If not online, sends second SIPI.
7. **Synchronization**: AP switches to 64-bit mode, loads IDT/GDT, enables paging, sets `online=true`.

#### 2. `spinlock_acquire`

```c
void spinlock_acquire(spinlock_t* lock);
```

**Description**:
Acquires a spinlock with deadlock detection.

1. Disables interrupts (`cli`).
2. `preempt_disable()`.
3. **Fast Path**: `atomic_compare_exchange(lock->value, 0, 1)`.
4. **Slow Path**:
   - Loops while `lock->value != 0`.
   - `cpu_relax()` (PAUSE instruction).
   - Checks for deadlock timeout (debug mode).
5. Sets `lock->owner = current_cpu`.

#### 3. `send_ipi`

```c
void send_ipi(uint32_t cpu_id, enum ipi_type type);
```

**Description**:
Sends Inter-Processor Interrupt.

1. Locates `paca[cpu_id]`.
2. Allocates `ipi_message` from ring buffer.
3. Sets `msg->type = type`.
4. Barriers: `smp_wmb()`.
5. Writes `APIC_ICR_LOW` with `vector | (apic_id << 56)`.
6. Waits for Delivery Status bit to clear.

**Algorithmic Details**:

**TLB Shootdown Protocol**:

1. Initiator disables preemption.
2. Sets `mm->tlb_flush_pending = mask` (target CPUs).
3. Sends `IPI_TLB_FLUSH` to mask.
4. While `mm->tlb_flush_pending != 0`:
   - `cpu_relax()`.
5. Enters Receiver:
   - `flush_tlb()`.
   - Atomically clears bit in `pending`.
   - Sends ACK (implied by clearing bit).

**Algorithmic Details**:

**IPI Handling**:

1. Sender prepares `ipi_message`.
2. Sender writes to Local APIC ICR (Interrupt Command Register).
3. Sender waits (spin or notify) for completion if synchronous.
4. Receiver gets vector interrupt.
5. Receiver reads message, executes action (e.g., `flush_tlb`).
6. Receiver acknowledges.

### 10.2 Virtualization Support - Complete Implementation

#### Requirements

- **Type-2 Hypervisor**: Run generic VMs
- **Hardware Acceleration**: Use Intel VT-x or AMD-V
- **Nested Paging**: EPT (Intel) or NPT (AMD)
- **I/O Virtualization**: IOMMU support

#### Implementation Details

**Data Structures (Intel VT-x)**:

```c
/* VMCS Region */
struct vmcs {
    uint32_t revision_id;
    uint32_t abort_indicator;
    uint8_t data[PAGE_SIZE - 8];
};

/* EPT Entry */
union ept_entry {
    uint64_t raw;
    struct {
        uint64_t read : 1;
        uint64_t write : 1;
        uint64_t execute : 1;
        uint64_t type : 3;
        uint64_t ignore_pat : 1;
        uint64_t large_page : 1;
        uint64_t accessed : 1;
        uint64_t dirty : 1;
        uint64_t user_mode : 1;
        uint64_t ignored : 1;
        uint64_t pfn : 40;
        uint64_t available : 12;
    } fields;
};

/* VCPU State */
struct vcpu {
    struct vmcs* vmcs;
    uint64_t host_rsp;
    uint64_t guest_regs[16];
    uint64_t cr3_target_count;
    bool launched;
    int state;
};
```

**Functions to Implement**:

**Detailed API Specification**:

#### 1. `vm_run`

```c
int vm_run(struct vcpu* vcpu);
```

**Description**:
Executes the VCPU loop.

1. `vmptrld(vcpu->vmcs_phys)`.
2. Restores GPRs from `vcpu->guest_regs`.
3. `vmlaunch` (first time) or `vmresume` (subsequent).
4. **VM Exit**: Hardware saves state to VMCS, jumps to HOST_RIP.
5. Saves Guest GPRs to `vcpu->guest_regs`.
6. Calls `handle_vmexit(vcpu)`.

#### 2. `handle_vmexit`

```c
int handle_vmexit(struct vcpu* vcpu);
```

**Description**:
Decodes VM Exit Reason and dispatches handler.

- **CPUID**: Emulates CPUID instruction (returns host topology or masked features).
- **IO_INSTRUCTION**: Emulates PIO (calls `io_handler`).
- **EPT_VIOLATION**: Handles MMIO or Paging faults (Shadow Paging/EPT fill).
- **CR_ACCESS**: Emulates MOV CRx (e.g., enable paging).
- **MSR_READ/WRITE**: Emulates Model Specific Registers.
- **HLT**: Yields CPU until interrupt.

#### 3. `vm_map_phys`

```c
int vm_map_phys(struct vm* vm, uint64_t gpa, uint64_t hpa, uint64_t flags);
```

**Description**:
Maps Guest Physical Address to Host Physical Address in EPT/NPT.

1. Walks EPT PML4 -> PDPT -> PD -> PT.
2. Allocates tables if missing.
3. Sets PFN in Leaf Entry with correct permissions (Read/Write/Exec).
4. Invalidates EPT TLB (`invept`).

**Algorithmic Details**:

**Instruction Emulation (PIO)**:

1. Decode `EXIT_QUALIFICATION` to get Port, Size, Direction (In/Out).
2. If `Out`: Read value from Guest RAX/EAX/AX/AL. Write to virtual device state.
3. If `In`: Read from virtual device state. Write to Guest RAX...
4. Advance Guest RIP by `VM_EXIT_INSTRUCTION_LEN`.

**Interrupt Injection**:
Before `vmresume`:

1. Check if Guest RFLAGS.IF is set (Interruptible).
2. Check if pending virtual interrupt exists.
3. Write `VM_ENTRY_INTR_INFO_FIELD` in VMCS with Vector and Type (ExtInt).

### 10.3 Real-time Features - Complete Implementation

#### Requirements

- **Preemptible Kernel**: Minimize interrupt latency
- **Priority Inheritance**: Prevent priority inversion
- **High-Resolution Timers**: Nanosecond precision

#### Implementation Details

**Data Structures**:

```c
struct rt_mutex {
    spinlock_t lock;
    struct thread* owner;
    struct list_head wait_list; /* Threads blocked on this mutex */
    int original_priority;
};

struct hrtimer {
    uint64_t expiry_ns;
    void (*callback)(struct hrtimer*);
    struct rb_node node;        /* Red-black tree node */
};
```

**Priority Inheritance Algorithm**:

1. When Thread A (Low Prio) holds Mutex M.
2. Thread B (High Prio) tries to lock M.
3. B blocks.
4. Kernel checks M->owner (A).
5. If A->priority < B->priority, boost A->priority = B->priority.
6. Make A inheritor of B's priority effectively.
7. Recurse if A is blocked on another mutex.
8. On unlock, restore A's original priority.

---

### 10.4 Containerization - Complete Implementation

#### Requirements

- **Namespaces**: Process ID (PID), Mount (MNT), Network (NET), UTS, IPC, User.
- **Control Groups (cgroups)**: Resource limiting (CPU, Memory, IO).
- **Layered Filesystem**: OverlayFS implementation.

#### Implementation Details

**Data Structures**:

```c
/* Namespace Proxy */
struct nsproxy {
    atomic_t count;
    struct uts_namespace* uts_ns;
    struct ipc_namespace* ipc_ns;
    struct mnt_namespace* mnt_ns;
    struct pid_namespace* pid_ns;
    struct net_namespace* net_ns;
};

/* CGroup Controller */
struct cgroup_subsys_state {
    struct cgroup* cgroup;
    atomic_t refcnt;
    unsigned long flags;
    /* Controller specific data follows */
};

struct css_set {
    atomic_t refcnt;
    struct hlist_node hlist;
    struct list_head tasks; /* Tasks in this cg */
    struct cgroup_subsys_state* subsys[CGROUP_SUBSYS_COUNT];
};
```

**Functions to Implement**:

#### 1. `copy_namespaces`

```c
int copy_namespaces(unsigned long flags, struct task_struct* tsk);
```

**Description**:
Called during `fork()`/`clone()`.

1. If flags set (`CLONE_NEWPID`, etc.), allocate new namespace structures.
2. Initialize new namespace (e.g., PID NS creates new ID allocator).
3. If flags not set, increment refcount on parent's namespaces.
4. Attach `nsproxy` to new task.

#### 2. `cgroup_attach_task`

```c
int cgroup_attach_task(struct cgroup* cgrp, struct task_struct* tsk);
```

**Description**:
Moves a task into a cgroup.

1. Lock `css_set_lock`.
2. Find existing `css_set` linking to `cgrp`.
3. If not exists, allocate new `css_set`.
4. Migrate task to new `css_set`.
5. Call controller APIs (e.g., `cpu_cgroup_attach`) to update accounting.

**Algorithmic Details**:

**PID Namespace Translation**:

1. Global PID: Unique ID in kernel.
2. Virtual PID: ID seen by process.
3. `task_pid_vnr(task)`:
   - Walk up namespace hierarchy from `task->nsproxy->pid_ns`.
   - Find mapping for `task`.
   - Return local ID.

---

### 10.4 Containerization - Complete Implementation

#### Requirements

- **Namespaces**: Process ID (PID), Mount (MNT), Network (NET), UTS, IPC, User.
- **Control Groups (cgroups)**: Resource limiting (CPU, Memory, IO).
- **Layered Filesystem**: OverlayFS implementation.

#### Implementation Details

**Data Structures**:

```c
/* Namespace Proxy */
struct nsproxy {
    atomic_t count;
    struct uts_namespace* uts_ns;
    struct ipc_namespace* ipc_ns;
    struct mnt_namespace* mnt_ns;
    struct pid_namespace* pid_ns;
    struct net_namespace* net_ns;
};

/* CGroup Controller */
struct cgroup_subsys_state {
    struct cgroup* cgroup;
    atomic_t refcnt;
    unsigned long flags;
    /* Controller specific data follows */
};

struct css_set {
    atomic_t refcnt;
    struct hlist_node hlist;
    struct list_head tasks; /* Tasks in this cg */
    struct cgroup_subsys_state* subsys[CGROUP_SUBSYS_COUNT];
};
```

**Functions to Implement**:

#### 1. `copy_namespaces`

```c
int copy_namespaces(unsigned long flags, struct task_struct* tsk);
```

**Description**:
Called during `fork()`/`clone()`.

1. If flags set (`CLONE_NEWPID`, etc.), allocate new namespace structures.
2. Initialize new namespace (e.g., PID NS creates new ID allocator).
3. If flags not set, increment refcount on parent's namespaces.
4. Attach `nsproxy` to new task.

#### 2. `cgroup_attach_task`

```c
int cgroup_attach_task(struct cgroup* cgrp, struct task_struct* tsk);
```

**Description**:
Moves a task into a cgroup.

1. Lock `css_set_lock`.
2. Find existing `css_set` linking to `cgrp`.
3. If not exists, allocate new `css_set`.
4. Migrate task to new `css_set`.
5. Call controller APIs (e.g., `cpu_cgroup_attach`) to update accounting.

**Algorithmic Details**:

**PID Namespace Translation**:

1. Global PID: Unique ID in kernel.
2. Virtual PID: ID seen by process.
3. `task_pid_vnr(task)`:
   - Walk up namespace hierarchy from `task->nsproxy->pid_ns`.
   - Find mapping for `task`.
   - Return local ID.

---

## Phase 11: Deployment & Distribution (Months 29-32)

### Timeline Overview

| Week  | Milestone           | Deliverables              |
| ----- | ------------------- | ------------------------- |
| 1-4   | Package Format      | Spec, builder, signer     |
| 5-8   | Dependency Resolver | SAT solver implementation |
| 9-12  | Network Repository  | HTTP client, repo layout  |
| 13-16 | System Installer    | Partitioning, formatting  |

### 11.1 Package Manager - Complete Implementation

#### Requirements

- **Package Format**: Compressed archive (zstd) with metadata (TOML)
- **Dependency Resolution**: Versioned dependencies
- **Security**: Signed packages (Ed25519)
- **Repositories**: Local and remote (HTTP/HTTPS)
- **Transactions**: Atomic install/update/remove

#### Implementation Details

**Data Structures**:

```c
/* Detailed Package Metadata */
struct package_metadata {
    char* name;
    char* version;          /* SemVer compliant */
    char* description;
    char* maintainer;
    char* license;
    char* arch;            /* x86_64, aarch64 */

    /* Dependencies */
    struct dependency* depends;
    struct dependency* conflicts;
    struct dependency* provides;
    size_t dep_count;

    /* File Manifest */
    struct file_entry* files;
    size_t file_count;

    /* Scripts */
    char* pre_install;
    char* post_install;
    char* pre_remove;
    char* post_remove;
};

struct dependency {
    char* name;
    enum cmp_op { EQ, GT, LT, GE, LE } op;
    char* version;
};

struct repository {
    char* name;
    char* url;
    uint8_t pubkey[32];     /* Ed25519 public key */
    struct package_metadata* cache;
    size_t cache_size;
};

/* Transaction Step */
enum step_type { INSTALL, REMOVE, UPDATE };
struct transaction_step {
    enum step_type type;
    struct package_metadata* pkg;
    struct transaction_step* next;
};
```

**Detailed API Specification**:

#### 1. `pkg_solve_dependencies`

```c
struct transaction* pkg_solve_dependencies(const char* target, struct repository* repo);
```

**Description**:
Resolves dependencies using a SAT solver approach.

1. **Clause Generation**:
   - For every package `P` in repo:
     - `Install(P) -> Install(Dep1) AND Install(Dep2)...`.
     - `Install(P) -> NOT Install(Conflict1)`.
   - Add user goal: `Install(Target)`.
2. **Solver Execution**:
   - Uses CDCL (Conflict-Driven Clause Learning) solver.
   - Finds an assignment of True/False to all `Install(P)` variables.
3. **Transaction Build**:
   - Collects all variables assigned True.
   - Sorts them topologically based on dependency graph.
   - Returns list of `INSTALL` steps.

#### 2. `pkg_verify_signature`

```c
int pkg_verify_signature(const char* path, uint8_t* pubkey);
```

**Description**:
Verifies the Ed25519 signature of a package.

1. Reads the last 64 bytes of the `.apk` file (The signature).
2. Hashes the rest of the file using BLAKE3.
3. Calls `ed25519_verify(signature, hash, pubkey)`.
4. Returns 0 if valid, -1 if invalid.

**Algorithmic Details**:

**Repository Indexing**:

1. Crawl directory of `.apk` files.
2. Extract `metadata.toml` from each.
3. Aggregate into `index.json`.
4. Sign `index.json` with GPG private key.
5. Clients download `index.json` + `index.json.sig`.

**Detailed System Installer Logic**:

**Partitioning (GPT)**:

1. `wipe_disk(device)`: Write zeros to first/last 1MB.
2. `create_gpt_header()`: UUID generation, CRC32.
3. `add_partition(EFI, 512MB, FAT32)`.
4. `add_partition(ROOT, Remaining, LFSX)`.

**Bootstrap Process**:

1. Mount `/mnt/root` and `/mnt/root/boot/efi`.
2. `pkg_install_root("/mnt/root", "base-system")`.
   - Unpacks packages to target chroot.
   - Runs `post_install` scripts inside chroot (using `chroot` or namespace isolation).
3. `config_gen_fstab()`.
4. `install_bootloader()`.

---

## Phase 12: Future Architecture (Months 33-36)

### 12.1 Microkernel Migration Strategy

#### Goals

- **Fault Isolation**: Drivers in userspace.
- **Message Passing**: Fast IPC (L4-style).
- **Capability Security**: Object-capability model.

#### Proposed Architecture (AstraLisp Micro)

1.  **Nano-Kernel**: Handles threading, IPC, and MMU only. (< 10k LOC).
2.  **User-Space Drivers**:
    - `pci_server`: Enumerates bus, grants MMIO access to driver processes.
    - `disk_driver`: Talks to NVMe, exposes block IPC interface.
    - `net_driver`: Talks to NIC, exposes packet IPC interface.
3.  **Lisp Runtime**: runs as a "System Server" managing the object graph.

### 12.2 Distributed AstraLisp

#### Concept

Transform the OS into a Single System Image (SSI) cluster.

#### Implementation

1.  **Remote Object References**: Lisp pointers that refer to objects on another node.
2.  **Network Paging**: Valid pages can be fetched from remote RAM (RDMA).
3.  **Process Migration**: Serializing a running thread (stack + heap closure) and resuming on another node.

### 11.2 System Installer - Complete Implementation

#### Requirements

- **Disk Detection**: Scan available drives (NVMe, SATA, USB)
- **Partitioning**: Create GPT partitions (EFI, Swap, Root)
- **Filesystem Creation**: Format partitions (FAT32, LFSX)
- **Bootstrap**: Copy base system files
- **Bootloader Setup**: Install GRUB or Limine

#### Implementation Details

**Data Structures**:

```c
struct partition_plan {
    char* disk_dev;         /* e.g., /dev/nvme0n1 */
    uint64_t total_size;

    struct {
        uint64_t size;
        const char* type;   /* "EFI System", "Linux Filesystem" */
        const char* mount_point;
    } partitions[4];        /* Simplified for example */
};

struct install_config {
    char hostname[64];
    char username[32];
    char password_hash[128];
    char timezone[32];
    char locale[32];
    struct partition_plan storage;
};
```

**Installation State Machine**:

1. **Welcome**: Language selection.
2. **Hardware Scan**: Detect disks, network.
3. **Partitioning**: UI to define `partition_plan`.
4. **User Setup**: Collect credentials.
5. **Execution**:
   - `part_create_table(disk, "gpt")`
   - `part_add_partition(...)` x N
   - `mkfs.fat -F32 /dev/nvme0n1p1`
   - `mkfs.lfsx /dev/nvme0n1p3`
   - `mount /dev/nvme0n1p3 /mnt/target`
   - `pkg_install_base("/mnt/target")`
   - `bootloader_install("/mnt/target")`
6. **Finish**: Reboot.

---

## Phase 12: Future Architecture (Months 33-36)

### Timeline Overview

| Week  | Milestone           | Deliverables             |
| ----- | ------------------- | ------------------------ |
| 1-8   | Microkernel Core    | IPC-centric kernel API   |
| 9-16  | Userspace Drivers   | Port drivers to userland |
| 17-24 | Distributed IPC     | Network transparent IPC  |
| 25-30 | Capability Hardware | CHERI support research   |

### 12.1 Microkernel Transition

#### Strategy

AstraLisp OS is designed as a hybrid kernel, but the long-term goal is a pure microkernel to maximize stability.

1.  **Extract Drivers**: Move USB, AHCI, Network stacks to userspace processes.
2.  **Define IPC Interfaces**: Strict IDL (Interface Definition Language) for all driver interactions.
3.  **Kernel Minimalism**: Reduce kernel role to: Scheduling, IPC, Basic Memory Mgmt.

**Proposed IPC Message Format**:

```c
struct ipc_header {
    uint64_t source_tid;
    uint64_t dest_port;
    uint32_t message_id;
    uint32_t size;
    uint32_t flags;         /* BLOCKING, ONE_WAY, etc. */
};
```

### 12.2 Distributed OS Features

#### Concept

Treat a cluster of AstraLisp machines as a single system image.

1.  **Global PID Namespace**: Process ID includes Node ID.
2.  **Object Migration**: Move Lisp objects (heap pages) between nodes transparently.
3.  **Distributed GC**: Global mark-sweep across nodes.

```c
/* Distributed Object Reference */
struct global_ref {
    uint32_t node_id;
    uint64_t address;
};
```

This roadmap provides a comprehensive path to a production-grade Operating System.

---

# Appendices

## Appendix A: System Call Reference

This section defines the ABI for all kernel system calls.

| ID  | Name | Signature | Description |
| --- | ---- | --------- | ----------- |
| 0   | `sys_exit` | `void exit(int status)` | Terminate the calling process. |
| 1   | `sys_fork` | `pid_t fork(void)` | Create a child process. |
| 2   | `sys_read` | `ssize_t read(int fd, void *buf, size_t count)` | Read from a file descriptor. |
| 3   | `sys_write` | `ssize_t write(int fd, const void *buf, size_t count)` | Write to a file descriptor. |
| 4   | `sys_open` | `int open(const char *pathname, int flags, mode_t mode)` | Open a file. |
| 5   | `sys_close` | `int close(int fd)` | Close a file descriptor. |
| 6   | `sys_waitpid` | `pid_t waitpid(pid_t pid, int *status, int options)` | Wait for process change. |
| 7   | `sys_creat` | `int creat(const char *pathname, mode_t mode)` | Create a file. |
| 8   | `sys_link` | `int link(const char *oldpath, const char *newpath)` | Make a new name for a file. |
| 9   | `sys_unlink` | `int unlink(const char *pathname)` | Delete a name and possibly the file. |
| 10  | `sys_execve` | `int execve(const char *filename, char *const argv[], char *const envp[])` | Execute program. |
| 11  | `sys_chdir` | `int chdir(const char *path)` | Change working directory. |
| 12  | `sys_time` | `time_t time(time_t *tloc)` | Get time in seconds. |
| 13  | `sys_mknod` | `int mknod(const char *pathname, mode_t mode, dev_t dev)` | Create directory/special file. |
| 14  | `sys_chmod` | `int chmod(const char *pathname, mode_t mode)` | Change permissions of a file. |
| 15  | `sys_lchown` | `int lchown(const char *pathname, uid_t owner, gid_t group)` | Change ownership of a file. |
| 16  | `sys_stat` | `int stat(const char *pathname, struct stat *statbuf)` | Get file status. |
| 17  | `sys_lseek` | `off_t lseek(int fd, off_t offset, int whence)` | Reposition read/write offset. |
| 18  | `sys_getpid` | `pid_t getpid(void)` | Get process identity. |
| 19  | `sys_mount` | `int mount(const char *source, const char *target, const char *filesystemtype, unsigned long mountflags, const void *data)` | Mount filesystem. |
| 20  | `sys_umount` | `int umount(const char *target)` | Unmount filesystem. |
| 21  | `sys_setuid` | `int setuid(uid_t uid)` | Set user identity. |
| 22  | `sys_getuid` | `uid_t getuid(void)` | Get user identity. |
| 23  | `sys_stime` | `int stime(const time_t *t)` | Set system time. |
| 24  | `sys_ptrace` | `long ptrace(enum __ptrace_request request, pid_t pid, void *addr, void *data)` | Process trace. |
| 25  | `sys_alarm` | `unsigned int alarm(unsigned int seconds)` | Schedule an alarm signal. |
| 26  | `sys_fstat` | `int fstat(int fd, struct stat *statbuf)` | Get file status (fd). |
| 27  | `sys_pause` | `int pause(void)` | Wait for signal. |
| 28  | `sys_utime` | `int utime(const char *filename, const struct utimbuf *times)` | Change file last access/mod times. |
| 29  | `sys_access` | `int access(const char *pathname, int mode)` | Check user's permissions for a file. |
| 30  | `sys_nice` | `int nice(int inc)` | Change process priority. |
| 31  | `sys_sync` | `void sync(void)` | Commit buffer cache to disk. |
| 32  | `sys_kill` | `int kill(pid_t pid, int sig)` | Send signal to a process. |
| 33  | `sys_rename` | `int rename(const char *oldpath, const char *newpath)` | Change location/name of a file. |
| 34  | `sys_mkdir` | `int mkdir(const char *pathname, mode_t mode)` | Create a directory. |
| 35  | `sys_rmdir` | `int rmdir(const char *pathname)` | Delete a directory. |
| 36  | `sys_dup` | `int dup(int oldfd)` | Duplicate a file descriptor. |
| 37  | `sys_pipe` | `int pipe(int pipefd[2])` | Create pipe. |
| 38  | `sys_times` | `clock_t times(struct tms *buf)` | Get process times. |
| 39  | `sys_brk` | `int brk(void *addr)` | Change data segment size. |
| 40  | `sys_setgid` | `int setgid(gid_t gid)` | Set group identity. |
| 41  | `sys_getgid` | `gid_t getgid(void)` | Get group identity. |
| 42  | `sys_signal` | `sighandler_t signal(int signum, sighandler_t handler)` | ANSI C signal handling. |
| 43  | `sys_geteuid` | `uid_t geteuid(void)` | Get effective user identity. |
| 44  | `sys_getegid` | `gid_t getegid(void)` | Get effective group identity. |
| 45  | `sys_acct` | `int acct(const char *filename)` | Switch process accounting on/off. |
| 46  | `sys_ioctl` | `int ioctl(int fd, unsigned long request, ...)` | Control device. |
| 47  | `sys_fcntl` | `int fcntl(int fd, int cmd, ...)` | Manipulate file descriptor. |
| 48  | `sys_setpgid` | `int setpgid(pid_t pid, pid_t pgid)` | Set process group ID. |
| 49  | `sys_umask` | `mode_t umask(mode_t mask)` | Set file mode creation mask. |
| 50  | `sys_chroot` | `int chroot(const char *path)` | Change root directory. |
| 51  | `sys_ustat` | `int ustat(dev_t dev, struct ustat *ubuf)` | Get filesystem statistics. |
| 52  | `sys_dup2` | `int dup2(int oldfd, int newfd)` | Duplicate a file descriptor. |
| 53  | `sys_getppid` | `pid_t getppid(void)` | Get parent process ID. |
| 54  | `sys_getpgrp` | `pid_t getpgrp(void)` | Get process group ID. |
| 55  | `sys_setsid` | `pid_t setsid(void)` | Create session and set process group ID. |
| 56  | `sys_sigaction` | `int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)` | Examine and change signal action. |
| 57  | `sys_sgetmask` | `int sgetmask(void)` | Get signal mask (obsolete). |
| 58  | `sys_ssetmask` | `int ssetmask(int newmask)` | Set signal mask (obsolete). |
| 59  | `sys_setreuid` | `int setreuid(uid_t ruid, uid_t euid)` | Set real and/or effective user ID. |
| 60  | `sys_setregid` | `int setregid(gid_t rgid, gid_t egid)` | Set real and/or effective group ID. |
| 61  | `sys_sigsuspend` | `int sigsuspend(const sigset_t *mask)` | Wait for a signal. |
| 62  | `sys_sigpending` | `int sigpending(sigset_t *set)` | Examine pending signals. |
| 63  | `sys_sethostname` | `int sethostname(const char *name, size_t len)` | Set hostname. |
| 64  | `sys_setrlimit` | `int setrlimit(int resource, const struct rlimit *rlim)` | Set resource limits. |
| 65  | `sys_getrlimit` | `int getrlimit(int resource, struct rlimit *rlim)` | Get resource limits. |
| 66  | `sys_getrusage` | `int getrusage(int who, struct rusage *usage)` | Get resource usage. |
| 67  | `sys_gettimeofday` | `int gettimeofday(struct timeval *tv, struct timezone *tz)` | Get time. |
| 68  | `sys_settimeofday` | `int settimeofday(const struct timeval *tv, const struct timezone *tz)` | Set time. |
| 69  | `sys_getgroups` | `int getgroups(int size, gid_t list[])` | Get supplementary group IDs. |
| 70  | `sys_setgroups` | `int setgroups(size_t size, const gid_t *list)` | Set supplementary group IDs. |
| 71  | `sys_select` | `int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout)` | Synchronous I/O multiplexing. |
| 72  | `sys_symlink` | `int symlink(const char *target, const char *linkpath)` | Make a new name for a file. |
| 73  | `sys_lstat` | `int lstat(const char *pathname, struct stat *statbuf)` | Get file status. |
| 74  | `sys_readlink` | `ssize_t readlink(const char *pathname, char *buf, size_t bufsiz)` | Read value of a symbolic link. |
| 75  | `sys_uselib` | `int uselib(const char *library)` | Select shared library. |
| 76  | `sys_swapon` | `int swapon(const char *path, int swapflags)` | Start/stop swapping to file/device. |
| 77  | `sys_reboot` | `int reboot(int magic, int magic2, int cmd, void *arg)` | Reboot or enable/disable Ctrl-Alt-Del. |
| 78  | `sys_readdir` | `int readdir(unsigned int fd, struct old_linux_dirent *dirp, unsigned int count)` | Read directory entry. |
| 79  | `sys_mmap` | `void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)` | Map files or devices into memory. |
| 80  | `sys_munmap` | `int munmap(void *addr, size_t length)` | Unmap files or devices. |
| 81  | `sys_truncate` | `int truncate(const char *path, off_t length)` | Truncate a file to a specified length. |
| 82  | `sys_ftruncate` | `int ftruncate(int fd, off_t length)` | Truncate a file to a specified length. |
| 83  | `sys_fchmod` | `int fchmod(int fd, mode_t mode)` | Change permissions of a file. |
| 84  | `sys_fchown` | `int fchown(int fd, uid_t owner, gid_t group)` | Change ownership of a file. |
| 85  | `sys_getpriority` | `int getpriority(int which, id_t who)` | Get program scheduling priority. |
| 86  | `sys_setpriority` | `int setpriority(int which, id_t who, int prio)` | Set program scheduling priority. |
| 87  | `sys_statfs` | `int statfs(const char *path, struct statfs *buf)` | Get filesystem statistics. |
| 88  | `sys_fstatfs` | `int fstatfs(int fd, struct statfs *buf)` | Get filesystem statistics. |
| 89  | `sys_ioperm` | `int ioperm(unsigned long from, unsigned long num, int turn_on)` | Set port input/output permissions. |
| 90  | `sys_socketcall` | `int socketcall(int call, unsigned long *args)` | Socket system calls (multiplexer). |
| 91  | `sys_syslog` | `int syslog(int type, char *bufp, int len)` | Read and/or clear kernel message ring buffer. |
| 92  | `sys_setitimer` | `int setitimer(int which, const struct itimerval *new_value, struct itimerval *old_value)` | Set value of an interval timer. |
| 93  | `sys_getitimer` | `int getitimer(int which, struct itimerval *curr_value)` | Get value of an interval timer. |
| 94  | `sys_newstat` | `int newstat(const char *pathname, struct stat *statbuf)` | Get file status. |
| 95  | `sys_newlstat` | `int newlstat(const char *pathname, struct stat *statbuf)` | Get file status. |
| 96  | `sys_newfstat` | `int newfstat(int fd, struct stat *statbuf)` | Get file status. |
| 97  | `sys_uname` | `int uname(struct utsname *buf)` | Get name and information about current kernel. |
| 98  | `sys_iopl` | `int iopl(int level)` | Change I/O privilege level. |
| 99  | `sys_vhangup` | `int vhangup(void)` | Virtually hangup the current terminal. |

## Appendix B: Error Codes

List of all standard `errno` values returned by system calls.

| Code | Name | Description |
| ---- | ---- | ----------- |
| 1 | `EPERM` | Operation not permitted |
| 2 | `ENOENT` | No such file or directory |
| 3 | `ESRCH` | No such process |
| 4 | `EINTR` | Interrupted system call |
| 5 | `EIO` | I/O error |
| 6 | `ENXIO` | No such device or address |
| 7 | `E2BIG` | Argument list too long |
| 8 | `ENOEXEC` | Exec format error |
| 9 | `EBADF` | Bad file number |
| 10 | `ECHILD` | No child processes |
| 11 | `EAGAIN` | Try again |
| 12 | `ENOMEM` | Out of memory |
| 13 | `EACCES` | Permission denied |
| 14 | `EFAULT` | Bad address |
| 15 | `ENOTBLK` | Block device required |
| 16 | `EBUSY` | Device or resource busy |
| 17 | `EEXIST` | File exists |
| 18 | `EXDEV` | Cross-device link |
| 19 | `ENODEV` | No such device |
| 20 | `ENOTDIR` | Not a directory |
| 21 | `EISDIR` | Is a directory |
| 22 | `EINVAL` | Invalid argument |
| 23 | `ENFILE` | File table overflow |
| 24 | `EMFILE` | Too many open files |
| 25 | `ENOTTY` | Not a typewriter |
| 26 | `ETXTBSY` | Text file busy |
| 27 | `EFBIG` | File too large |
| 28 | `ENOSPC` | No space left on device |
| 29 | `ESPIPE` | Illegal seek |
| 30 | `EROFS` | Read-only file system |
| 31 | `EMLINK` | Too many links |
| 32 | `EPIPE` | Broken pipe |
| 33 | `EDOM` | Math argument out of domain of func |
| 34 | `ERANGE` | Math result not representable |
| 35 | `EDEADLK` | Resource deadlock would occur |
| 36 | `ENAMETOOLONG` | File name too long |
| 37 | `ENOLCK` | No record locks available |
| 38 | `ENOSYS` | Function not implemented |
| 39 | `ENOTEMPTY` | Directory not empty |
| 40 | `ELOOP` | Too many symbolic links encountered |
| 42 | `ENOMSG` | No message of desired type |
| 43 | `EIDRM` | Identifier removed |
| 44 | `ECHRNG` | Channel number out of range |
| 45 | `EL2NSYNC` | Level 2 not synchronized |
| 46 | `EL3HLT` | Level 3 halted |
| 47 | `EL3RST` | Level 3 reset |
| 48 | `ELNRNG` | Link number out of range |
| 49 | `EUNATCH` | Protocol driver not attached |
| 50 | `ENOCSI` | No CSI structure available |
| 51 | `EL2HLT` | Level 2 halted |
| 52 | `EBADE` | Invalid exchange |
| 53 | `EBADR` | Invalid request descriptor |
| 54 | `EXFULL` | Exchange full |
| 55 | `ENOANO` | No anode |
| 56 | `EBADRQC` | Invalid request code |
| 57 | `EBADSLT` | Invalid slot |
| 59 | `EBFONT` | Bad font file format |
| 60 | `ENOSTR` | Device not a stream |
| 61 | `ENODATA` | No data available |
| 62 | `ETIME` | Timer expired |
| 63 | `ENOSR` | Out of streams resources |
| 64 | `ENONET` | Machine is not on the network |
| 65 | `ENOPKG` | Package not installed |
| 66 | `EREMOTE` | Object is remote |
| 67 | `ENOLINK` | Link has been severed |
| 68 | `EADV` | Advertise error |
| 69 | `ESRMNT` | Srmount error |
| 70 | `ECOMM` | Communication error on send |
| 71 | `EPROTO` | Protocol error |
| 72 | `EMULTIHOP` | Multihop attempted |
| 73 | `EDOTDOT` | RFS specific error |
| 74 | `EBADMSG` | Not a data message |
| 75 | `EOVERFLOW` | Value too large for defined data type |
| 76 | `ENOTUNIQ` | Name not unique on network |
| 77 | `EBADFD` | File descriptor in bad state |
| 78 | `EREMCHG` | Remote address changed |
| 79 | `ELIBACC` | Can not access a needed shared library |
| 80 | `ELIBBAD` | Accessing a corrupted shared library |
| 81 | `ELIBSCN` | .lib section in a.out corrupted |
| 82 | `ELIBMAX` | Attempting to link in too many shared libraries |
| 83 | `ELIBEXEC` | Cannot exec a shared library directly |
| 84 | `EILSEQ` | Illegal byte sequence |
| 85 | `ERESTART` | Interrupted system call should be restarted |
| 86 | `ESTRPIPE` | Streams pipe error |
| 87 | `EUSERS` | Too many users |
| 88 | `ENOTSOCK` | Socket operation on non-socket |
| 89 | `EDESTADDRREQ` | Destination address required |
| 90 | `EMSGSIZE` | Message too long |
| 91 | `EPROTOTYPE` | Protocol wrong type for socket |
| 92 | `ENOPROTOOPT` | Protocol not available |
| 93 | `EPROTONOSUPPORT` | Protocol not supported |
| 94 | `ESOCKTNOSUPPORT` | Socket type not supported |
| 95 | `EOPNOTSUPP` | Operation not supported on transport endpoint |
| 96 | `EPFNOSUPPORT` | Protocol family not supported |
| 97 | `EAFNOSUPPORT` | Address family not supported by protocol |
| 98 | `EADDRINUSE` | Address already in use |
| 99 | `EADDRNOTAVAIL` | Cannot assign requested address |
| 100 | `ENETDOWN` | Network is down |
| 101 | `ENETUNREACH` | Network is unreachable |
| 102 | `ENETRESET` | Network dropped connection because of reset |
| 103 | `ECONNABORTED` | Software caused connection abort |
| 104 | `ECONNRESET` | Connection reset by peer |
| 105 | `ENOBUFS` | No buffer space available |
| 106 | `EISCONN` | Transport endpoint is already connected |
| 107 | `ENOTCONN` | Transport endpoint is not connected |
| 108 | `ESHUTDOWN` | Cannot send after transport endpoint shutdown |
| 109 | `ETOOMANYREFS` | Too many references: cannot splice |
| 110 | `ETIMEDOUT` | Connection timed out |
| 111 | `ECONNREFUSED` | Connection refused |
| 112 | `EHOSTDOWN` | Host is down |
| 113 | `EHOSTUNREACH` | No route to host |
| 114 | `EALREADY` | Operation already in progress |
| 115 | `EINPROGRESS` | Operation now in progress |
| 116 | `ESTALE` | Stale file handle |
| 117 | `EUCLEAN` | Structure needs cleaning |
| 118 | `ENOTNAM` | Not a XENIX named type file |
| 119 | `ENAVAIL` | No XENIX semaphores available |
| 120 | `EISNAM` | Is a named type file |
| 121 | `EREMOTEIO` | Remote I/O error |
| 122 | `EDQUOT` | Quota exceeded |
| 123 | `ENOMEDIUM` | No medium found |
| 124 | `EMEDIUMTYPE` | Wrong medium type |
| 125 | `ECANCELED` | Operation Canceled |
| 126 | `ENOKEY` | Required key not available |
| 127 | `EKEYEXPIRED` | Key has expired |
| 128 | `EKEYREVOKED` | Key has been revoked |
| 129 | `EKEYREJECTED` | Key was rejected by service |
| 130 | `EOWNERDEAD` | Owner died |
| 131 | `ENOTRECOVERABLE` | State not recoverable |

