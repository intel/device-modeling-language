# Utility Templates

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [lib/1.2/dml-builtins.dml](lib/1.2/dml-builtins.dml)
- [lib/1.4/dml-builtins.dml](lib/1.4/dml-builtins.dml)
- [lib/1.4/utility.dml](lib/1.4/utility.dml)
- [test/1.4/lib/T_io_memory.dml](test/1.4/lib/T_io_memory.dml)
- [test/1.4/lib/T_io_memory.py](test/1.4/lib/T_io_memory.py)
- [test/1.4/lib/T_map_target_connect.py](test/1.4/lib/T_map_target_connect.py)
- [test/1.4/lib/T_signal_templates.dml](test/1.4/lib/T_signal_templates.dml)
- [test/1.4/lib/T_signal_templates.py](test/1.4/lib/T_signal_templates.py)

</details>



## Purpose and Scope

The `utility.dml` library provides reusable templates for common device modeling patterns in DML 1.4. These templates implement standard behaviors for registers, fields, reset mechanisms, memory-mapped I/O operations, and signal handling. They are designed to be instantiated on registers, fields, banks, ports, and connections to add specific functionality without requiring custom method implementations.

This page provides an overview of all utility templates and their organization. For detailed documentation on specific template categories:
- Core object templates (`device`, `bank`, `register`, etc.) are covered in [Core Templates (dml-builtins)](#4.1)
- Reset mechanism details are in [Reset System](#4.3)
- Register behavior patterns are detailed in [Register and Field Behaviors](#4.4)
- Memory I/O patterns are explained in [Memory-Mapped I/O](#4.5)

**Sources:** [lib/1.4/utility.dml:1-26]()

## Template Categories

The utility library organizes templates into five main categories:

| Category | Purpose | Primary Targets | Common Use Cases |
|----------|---------|-----------------|------------------|
| **Reset Templates** | Define power-on, hard, and soft reset behaviors | Device, registers, fields | Initializing device state, handling reset signals |
| **Register/Field Behaviors** | Implement common read/write patterns | Registers, fields | Read-only, write-only, constant values, unimplemented features |
| **Memory-Mapped I/O** | Route memory transactions to banks and methods | Banks, ports, subdevices | Function-based routing, transaction handling |
| **Signal Templates** | Handle signal interface implementation and tracking | Ports, connections | Device interconnections, interrupt lines |
| **Map Target Templates** | Provide memory-mapped access to connections | Connections | DMA controllers, bus masters |

**Template Organization Diagram**

```mermaid
graph TB
    UTILITY["utility.dml<br/>Standard Template Library"]
    
    RESET["Reset Templates<br/>power_on_reset, hard_reset, soft_reset<br/>poreset, hreset, sreset"]
    REGFIELD["Register/Field Behavior Templates<br/>read_only, write_only, constant, etc.<br/>25+ behavior patterns"]
    IOMEM["Memory-Mapped I/O Templates<br/>function_io_memory<br/>function_mapped_bank<br/>bank_io_memory"]
    SIGNAL["Signal Templates<br/>signal_port<br/>signal_connect"]
    MAPTARGET["Map Target Templates<br/>map_target<br/>map_target_value<br/>map_target_data"]
    
    UTILITY --> RESET
    UTILITY --> REGFIELD
    UTILITY --> IOMEM
    UTILITY --> SIGNAL
    UTILITY --> MAPTARGET
    
    RESET --> DEVICE1["Device Objects"]
    RESET --> REG1["Registers"]
    RESET --> FIELD1["Fields"]
    
    REGFIELD --> REG2["Registers"]
    REGFIELD --> FIELD2["Fields"]
    
    IOMEM --> BANKS["Banks"]
    IOMEM --> PORTS["Ports"]
    IOMEM --> SUBDEV["Subdevices"]
    
    SIGNAL --> PORTS2["Ports"]
    SIGNAL --> CONN["Connections"]
    
    MAPTARGET --> CONN2["Connections"]
```

**Sources:** [lib/1.4/utility.dml:1-26](), [lib/1.4/utility.dml:50-85](), [lib/1.4/utility.dml:336-941](), [lib/1.4/utility.dml:942-1065](), [lib/1.4/utility.dml:1066-1239](), [lib/1.4/utility.dml:1240-1397]()

## Reset Template System

### Template Hierarchy

Reset templates provide a three-level system for device initialization and reset:

```mermaid
graph TB
    subgraph "Top-Level Templates (Create Signal Ports)"
        PORESET["poreset<br/>Creates POWER port<br/>Triggers power_on_reset()"]
        HRESET["hreset<br/>Creates HRESET port<br/>Triggers hard_reset()"]
        SRESET["sreset<br/>Creates SRESET port<br/>Triggers soft_reset()"]
    end
    
    subgraph "Base Behavior Templates"
        POR["power_on_reset<br/>Recursively calls power_on_reset()<br/>on all subobjects"]
        HR["hard_reset<br/>Recursively calls hard_reset()<br/>on all subobjects"]
        SR["soft_reset<br/>Recursively calls soft_reset()<br/>on all subobjects"]
    end
    
    subgraph "Integration Templates"
        POR_INIT["_init_val_power_on_reset<br/>Calls init() after recursion<br/>Restores init_val"]
        HR_INIT["_init_val_hard_reset<br/>Calls init() after recursion<br/>Restores init_val"]
        SR_INIT["_init_val_soft_reset<br/>Calls init() after recursion<br/>Restores init_val"]
    end
    
    subgraph "Customization Templates"
        SOFT_VAL["soft_reset_val<br/>Uses soft_reset_val parameter<br/>instead of init_val"]
        STICKY["sticky<br/>Suppresses soft reset only"]
        NO_RESET["no_reset<br/>Suppresses all resets"]
    end
    
    PORESET --> POR
    HRESET --> HR
    SRESET --> SR
    
    POR --> POR_INIT
    HR --> HR_INIT
    SR --> SR_INIT
    
    POR_INIT -.->|"applied via<br/>'in each init_val'"| REGISTERS["Registers & Fields"]
    HR_INIT -.->|"applied via<br/>'in each init_val'"| REGISTERS
    SR_INIT -.->|"applied via<br/>'in each init_val'"| REGISTERS
    
    SR --> SOFT_VAL
    POR --> STICKY
    HR --> STICKY
    SR --> STICKY
    
    POR --> NO_RESET
    HR --> NO_RESET
    SR --> NO_RESET
```

**Sources:** [lib/1.4/utility.dml:176-215](), [lib/1.4/utility.dml:217-255](), [lib/1.4/utility.dml:277-333](), [lib/1.4/utility.dml:363-369]()

### Key Template Implementations

| Template | Target | Method Provided | Default Behavior |
|----------|--------|-----------------|------------------|
| `power_on_reset` | Any object | `power_on_reset()` | Recurses to all subobjects implementing `power_on_reset` |
| `hard_reset` | Any object | `hard_reset()` | Recurses to all subobjects implementing `hard_reset` |
| `soft_reset` | Any object | `soft_reset()` | Recurses to all subobjects implementing `soft_reset` |
| `poreset` | Device (top-level) | Creates `POWER` port | Implements `signal` interface, calls `dev.power_on_reset()` on `signal_raise` |
| `hreset` | Device (top-level) | Creates `HRESET` port | Implements `signal` interface, calls `dev.hard_reset()` on `signal_raise` |
| `sreset` | Device (top-level) | Creates `SRESET` port | Implements `signal` interface, calls `dev.soft_reset()` on `signal_raise` |

**Sources:** [lib/1.4/utility.dml:176-215](), [lib/1.4/utility.dml:217-255](), [lib/1.4/utility.dml:277-333]()

### Reset Propagation Mechanism

```mermaid
sequenceDiagram
    participant Signal as Signal Source
    participant Port as HRESET Port<br/>(signal interface)
    participant DevReset as dev.hard_reset()
    participant Each as _each_hard_reset<br/>(sequence param)
    participant BankReset as bank.hard_reset()
    participant RegReset as register.hard_reset()
    participant FieldInit as field.init()
    
    Signal->>Port: signal_raise()
    Port->>DevReset: invoke hard_reset()
    DevReset->>Each: iterate over sequence
    Each->>BankReset: obj.hard_reset()
    BankReset->>RegReset: recurse
    RegReset->>FieldInit: init() called by _init_val_hard_reset
    Note over FieldInit: field.val = init_val
```

**Sources:** [lib/1.4/utility.dml:217-255](), [lib/1.4/utility.dml:233-238]()

## Register and Field Behavior Templates

### Behavioral Pattern Categories

The utility library provides 25+ templates implementing common register and field access patterns:

**Read/Write Control Templates**

```mermaid
graph LR
    subgraph "Access Control"
        RO["read_only<br/>Writes logged as spec_viol<br/>Reads return actual value"]
        WO["write_only<br/>Reads logged as spec_viol<br/>Returns 0"]
        IG["ignore_write<br/>Writes silently ignored<br/>No state change"]
        IW["ignore<br/>Read returns 0<br/>Write ignored"]
    end
    
    subgraph "Constant Values"
        CONST["constant<br/>Logs writes as spec_viol<br/>Survives reset"]
        SILENT["silent_constant<br/>No logs<br/>Survives reset"]
        ZEROS["zeros<br/>constant with init_val=0"]
        ONES["ones<br/>constant with init_val=-1"]
        RC["read_constant<br/>Reads return read_val<br/>Writes update storage"]
        RZ["read_zero<br/>Reads return 0<br/>Writes update storage"]
    end
    
    subgraph "Bit Manipulation"
        W1C["write_1_clears<br/>Write 1 to clear bits<br/>new_val = old_val & ~written"]
        W1O["write_1_only<br/>Can only set bits<br/>new_val = old_val | written"]
        W0O["write_0_only<br/>Can only clear bits<br/>new_val = old_val & written"]
        COR["clear_on_read<br/>Read returns value<br/>Then sets to 0"]
    end
```

**Sources:** [lib/1.4/utility.dml:384-473](), [lib/1.4/utility.dml:490-560](), [lib/1.4/utility.dml:591-725]()

**Implementation Status Templates**

| Template | Log Level | Log Type | Use Case |
|----------|-----------|----------|----------|
| `unimpl` | 1 then 3 | `unimpl` | Feature not yet implemented |
| `read_unimpl` | 1 then 3 | `unimpl` | Read access unimplemented |
| `write_unimpl` | 1 then 3 | `unimpl` | Write access unimplemented |
| `silent_unimpl` | None | None | Unimplemented, suppress logs |
| `design_limitation` | 1 then 3 | `unimpl` | Known design limitation |
| `undocumented` | 1 then 3 | `unimpl` | Documentation unclear/missing |
| `reserved` | 2 (once) | `spec_viol` | Hardware reserved field |

**Sources:** [lib/1.4/utility.dml:776-941]()

### Template Composition Example

Templates can be combined to create complex behaviors. The composition follows these patterns:

```mermaid
graph TB
    subgraph "Method Override Chain"
        T1["Template 1<br/>write_field"]
        T2["Template 2<br/>write_field"]
        T3["Template 3<br/>write_field"]
        
        T1 -->|"calls default()"| T2
        T2 -->|"calls default()"| T3
        T3 -->|"calls default()"| BASE["Base Implementation"]
    end
    
    subgraph "Parameter Resolution"
        RANK["Template Rank System"]
        P1["Template A<br/>param init_val = 10"]
        P2["Template B<br/>param init_val = 20"]
        
        RANK --> HIGHEST["Highest Rank Wins"]
        P1 --> HIGHEST
        P2 --> HIGHEST
    end
    
    subgraph "Example: read_only + soft_reset_val"
        REG["register r @ 0x100"]
        RO_TEMP["is read_only<br/>write_field() logs error"]
        SRV_TEMP["is soft_reset_val<br/>soft_reset_val = 0xFF"]
        INIT["is init_val<br/>init_val = 0x00"]
        
        REG --> RO_TEMP
        REG --> SRV_TEMP
        REG --> INIT
        
        RO_TEMP -.-> WRITE["Writes rejected"]
        SRV_TEMP -.-> SOFT["Soft reset → 0xFF"]
        INIT -.-> HARD["Hard reset → 0x00"]
    end
```

**Sources:** [lib/1.4/utility.dml:434-445](), [lib/1.4/utility.dml:363-369]()

## Memory-Mapped I/O Templates

### Function-Based I/O Routing

The `function_io_memory` template enables function-based memory routing where banks are accessed via a function number rather than a single flat address space:

```mermaid
graph TB
    subgraph "Device Structure"
        DEV["device<br/>implement io_memory<br/>is function_io_memory"]
        BANK_A["bank a<br/>is function_mapped_bank<br/>param function = 0xA"]
        BANK_B["bank b<br/>is function_mapped_bank<br/>param function = 0xB"]
        BANK_C["bank c[i < 4]<br/>is function_mapped_bank<br/>param function = 0x10 + i"]
        
        DEV --> BANK_A
        DEV --> BANK_B
        DEV --> BANK_C
    end
    
    subgraph "Transaction Flow"
        TRANS["transaction_t<br/>physical_address:<br/>bits[63:32] = function<br/>bits[31:0] = offset"]
        
        DISPATCH["_dispatch_io_memory()<br/>Extract function number<br/>Look up bank"]
        
        BANK_ACCESS["bank.transaction_access()<br/>Uses offset only"]
        
        TRANS --> DISPATCH
        DISPATCH --> BANK_ACCESS
    end
    
    subgraph "Example Access"
        EX_TRANS["Transaction:<br/>addr = 0x000B_0100<br/>size = 4"]
        EX_ROUTE["Routes to bank b<br/>(function = 0xB)"]
        EX_OFFSET["Accesses offset 0x100<br/>within bank b"]
        
        EX_TRANS --> EX_ROUTE
        EX_ROUTE --> EX_OFFSET
    end
```

**Sources:** [lib/1.4/utility.dml:942-1011](), [test/1.4/lib/T_io_memory.dml:13-22]()

### Bank I/O Memory Template

The `bank_io_memory` template provides direct bank access without function routing:

```mermaid
graph LR
    subgraph "Configuration"
        PORT["port bare<br/>implement io_memory<br/>is bank_io_memory<br/>param bank = a"]
        BANK["bank a<br/>param use_io_memory = true<br/>param mappable = false"]
    end
    
    subgraph "Access Path"
        IFACE["io_memory interface<br/>operation()<br/>get_map()"]
        REDIRECT["Redirects to<br/>bank.transaction_access()"]
        BANK_IMPL["bank a implementation<br/>read/write methods"]
        
        IFACE --> REDIRECT
        REDIRECT --> BANK_IMPL
    end
```

**Sources:** [lib/1.4/utility.dml:1013-1065](), [test/1.4/lib/T_io_memory.dml:23-44]()

### I/O Memory Access Methods

Banks using `io_memory_access` template get specialized transaction handling:

| Method | Transaction Type | Purpose |
|--------|-----------------|---------|
| `get(uint64 offset, uint64 size)` | Inquiry (read-only) | Return value at offset without side effects |
| `set(uint64 offset, uint64 size, uint64 value)` | Inquiry (write) | Store value without side effects |
| `read(uint64 offset, uint64 enabled_bytes, void *aux)` | Normal transaction | Perform actual read with side effects |
| `write(uint64 offset, uint64 value, uint64 enabled_bytes, void *aux)` | Normal transaction | Perform actual write with side effects |

The `transaction_access` method automatically dispatches to the appropriate method based on the transaction's inquiry flag.

**Sources:** [lib/1.4/utility.dml:1013-1065](), [test/1.4/lib/T_io_memory.dml:115-163]()

## Signal Templates

### Signal Port Template

The `signal_port` template implements the `signal` interface for input signal handling:

```mermaid
graph TB
    subgraph "signal_port Template"
        PORT["port signal_input<br/>is signal_port"]
        IMPL["implement signal<br/>signal_raise()<br/>signal_lower()"]
        TRACK["param signal_high : bool<br/>Tracks current level"]
        VALIDATE["Validates transitions<br/>Logs spec_viol on invalid"]
    end
    
    subgraph "State Tracking"
        LOW["signal_high = false"]
        HIGH["signal_high = true"]
        
        LOW -->|"signal_raise()"| HIGH
        HIGH -->|"signal_lower()"| LOW
        HIGH -->|"signal_raise()"| ERROR1["Log: already high"]
        LOW -->|"signal_lower()"| ERROR2["Log: already low"]
    end
    
    subgraph "Usage Example"
        DEV["device"]
        INPUT["port insig<br/>is signal_port"]
        DEV --> INPUT
        
        EXT["External Source"] -->|"signal_raise()"| INPUT
        INPUT -.->|"signal_high = true"| STATE["Internal State"]
    end
```

**Sources:** [lib/1.4/utility.dml:1066-1127](), [test/1.4/lib/T_signal_templates.dml:12-23]()

### Signal Connect Template

The `signal_connect` template manages outgoing signal connections with level tracking:

```mermaid
graph TB
    subgraph "signal_connect Template"
        CONN["connect outsig<br/>is signal_connect"]
        METHODS["set_level(int level)<br/>Raises or lowers signal<br/>Updates signal_high"]
        ATTR["Attribute: outsig<br/>Target object implementing<br/>signal interface"]
        INIT["signal_high_at_init<br/>Optional: raise on creation"]
    end
    
    subgraph "Lifecycle Management"
        CREATE["Object Creation"]
        SET["outsig attribute set"]
        CHKPT["Checkpoint Load"]
        UNREG["outsig = None"]
        
        CREATE -->|"if signal_high_at_init"| RAISE1["Raise signal"]
        SET -->|"if signal_high"| RAISE2["Raise signal"]
        CHKPT -.->|"No raise"| RESTORE["Restore state"]
        UNREG -->|"if signal_high"| LOWER["Lower signal"]
    end
    
    subgraph "State Synchronization"
        PARAM["outsig_signal_high<br/>attribute/parameter"]
        TARGET["Target object<br/>signal_high state"]
        
        PARAM <-->|"Synchronized"| TARGET
    end
```

**Sources:** [lib/1.4/utility.dml:1129-1239](), [test/1.4/lib/T_signal_templates.py:25-99]()

## Map Target Templates

### Map Target Connection

The `map_target` template allows `connect` objects to provide memory-mapped access to target devices:

```mermaid
graph TB
    subgraph "map_target Template Structure"
        CONN["connect x<br/>is map_target"]
        ATTRS["Attributes:<br/>x_target: target object<br/>x_size: access size<br/>x_address: target offset"]
        IFACE["Uses transaction interface<br/>of target object"]
    end
    
    subgraph "Access Methods"
        VALUE["map_target_value<br/>x_value attribute<br/>Read/write uint64"]
        DATA["map_target_data<br/>x_data attribute<br/>Read/write byte array"]
    end
    
    subgraph "Transaction Flow"
        READ["x_value (read)"]
        TRANS_R["Create transaction<br/>addr = x_address<br/>size = x_size"]
        TARGET_R["target.transaction.issue()"]
        
        WRITE["x_value = val (write)"]
        TRANS_W["Create transaction<br/>with value"]
        TARGET_W["target.transaction.issue()"]
        
        READ --> TRANS_R
        TRANS_R --> TARGET_R
        WRITE --> TRANS_W
        TRANS_W --> TARGET_W
    end
    
    subgraph "Example Configuration"
        CONNECT["connect x<br/>is (map_target,<br/>map_target_value)"]
        SET_TARGET["x_target = obj.bank.z"]
        SET_ADDR["x_address = 0x100"]
        SET_SIZE["x_size = 8"]
        
        ACCESS["x_value = 0x42"]
        RESULT["Writes 0x42 to<br/>bank.z @ offset 0x100"]
        
        CONNECT --> SET_TARGET
        SET_TARGET --> SET_ADDR
        SET_ADDR --> SET_SIZE
        SET_SIZE --> ACCESS
        ACCESS --> RESULT
    end
```

**Sources:** [lib/1.4/utility.dml:1240-1397](), [test/1.4/lib/T_map_target_connect.dml:1-29]()

### Map Target Attributes

| Attribute | Type | Purpose | Required |
|-----------|------|---------|----------|
| `x_target` | `conf_object_t *` | Target device/bank implementing `transaction` | Yes |
| `x_size` | `uint64` | Size of memory access in bytes | Yes |
| `x_address` | `uint64` | Offset within target object | Yes |
| `x_value` | `uint64` | Read/write 64-bit value (if `map_target_value`) | No |
| `x_data` | `[d*]` | Read/write byte array (if `map_target_data`) | No |

The `x_` prefix is replaced by the actual connection name. For a connection named `dma`, the attributes would be `dma_target`, `dma_size`, `dma_address`, etc.

**Sources:** [lib/1.4/utility.dml:1240-1325](), [test/1.4/lib/T_map_target_connect.py:9-43]()

### Error Handling and Logging

Map target templates provide comprehensive error logging:

```mermaid
graph TB
    subgraph "Error Conditions"
        NO_TARGET["x_target not set"]
        INVALID_TARGET["x_target invalid type<br/>(not implementing transaction)"]
        TRANS_FAIL["Transaction fails<br/>(access outside registers)"]
    end
    
    subgraph "Logging Behavior"
        NO_TARGET --> LOG1["Log: info<br/>'target not set'<br/>Return None or raise exception"]
        INVALID_TARGET --> LOG2["Raise SimExc_Attribute"]
        TRANS_FAIL --> LOG3["Log: info<br/>'failed to read/write X bytes @ offset'<br/>Log target's spec_viol"]
    end
    
    subgraph "Log Level Configuration"
        DEV_LOGLEVEL["Device log_level parameter<br/>Controls verbosity"]
        LEVEL4["Level 4: logs all accesses<br/>'read/wrote X bytes @ offset'"]
        LEVEL2["Level 2: logs failures only"]
        
        DEV_LOGLEVEL --> LEVEL4
        DEV_LOGLEVEL --> LEVEL2
    end
```

**Sources:** [lib/1.4/utility.dml:1326-1397](), [test/1.4/lib/T_map_target_connect.py:45-65]()

## Template Application Guidelines

### Choosing the Right Template

The following decision tree helps select appropriate templates:

```mermaid
graph TB
    START["Select Template"]
    
    Q1["Is it a register/field?"]
    Q2["What access pattern?"]
    Q3["Is it implemented?"]
    Q4["Is it a reset mechanism?"]
    Q5["Is it memory routing?"]
    Q6["Is it signal handling?"]
    
    START --> Q1
    
    Q1 -->|Yes| Q2
    Q1 -->|No| Q4
    
    Q2 -->|"Read-only"| RO["read_only"]
    Q2 -->|"Write-only"| WO["write_only"]
    Q2 -->|"Constant"| CONST["constant or<br/>silent_constant"]
    Q2 -->|"Bit manipulation"| BITS["write_1_clears<br/>write_1_only<br/>write_0_only"]
    Q2 --> Q3
    
    Q3 -->|No| UNIMP["unimpl<br/>read_unimpl<br/>write_unimpl"]
    Q3 -->|"Reserved"| RES["reserved"]
    Q3 -->|"Unclear docs"| UNDOC["undocumented"]
    
    Q4 -->|Yes| RESET["poreset, hreset, sreset<br/>soft_reset_val<br/>sticky, no_reset"]
    Q4 -->|No| Q5
    
    Q5 -->|Yes| IOMEM["function_io_memory<br/>function_mapped_bank<br/>bank_io_memory"]
    Q5 -->|No| Q6
    
    Q6 -->|Yes| SIG["signal_port<br/>signal_connect"]
    Q6 -->|No| MAPTGT["map_target<br/>map_target_value<br/>map_target_data"]
```

**Sources:** [lib/1.4/utility.dml:1-1397]()

### Common Template Combinations

Frequently used template combinations:

| Combination | Purpose | Example Use Case |
|-------------|---------|------------------|
| `read_only + init_val` | Readable constant initialized at device creation | Hardware version register |
| `constant + soft_reset_val` | Constant with different soft reset value | Configuration register |
| `write_1_clears + sticky` | Status bits cleared by software, persist across soft reset | Interrupt status register |
| `reserved + ignore_write` | Reserved field in implemented register | Future expansion bits |
| `unimpl + read_constant` | Unimplemented feature with fixed return value | Capability register returning 0 |
| `function_io_memory + hreset` | Function-routed banks with hard reset support | Multi-function device |

**Sources:** [lib/1.4/utility.dml:336-941](), [lib/1.4/utility.dml:176-333]()

### Internal Helper Templates

Several templates are internal implementation details and not intended for direct use:

| Template | Purpose | Used By |
|----------|---------|---------|
| `_reg_or_field` | Provides `is_register` parameter | Register/field behavior templates |
| `_simple_write` | Efficient write implementation | `write_1_clears`, `write_1_only`, etc. |
| `_log_unimpl_read` | Logs unimplemented reads | `unimpl`, `read_unimpl` |
| `_log_unimpl_write` | Logs unimplemented writes | `unimpl`, `write_unimpl` |
| `_init_val_power_on_reset` | Integrates `init_val` with `power_on_reset` | Applied by `poreset` |
| `_init_val_hard_reset` | Integrates `init_val` with `hard_reset` | Applied by `hreset` |
| `_init_val_soft_reset` | Integrates `init_val` with `soft_reset` | Applied by `sreset` |

These templates should not be instantiated directly in device models.

**Sources:** [lib/1.4/utility.dml:14-16](), [lib/1.4/utility.dml:890-895](), [lib/1.4/utility.dml:776-798](), [lib/1.4/utility.dml:192-197]()