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

*Document continues with Phase 3-8 specifications...*
