# Claude Code Integration with Dolphin Smalltalk

## Overview

This repository contains a bridge system that allows Claude Code to communicate with a live Dolphin Smalltalk image over TCP. This enables real-time code execution, inspection, and modification of the Smalltalk environment from within Claude Code conversations.

## Communication Architecture

### TCP Bridge Protocol
- **Endpoint**: The Dolphin server listens on the Windows host IP as seen from WSL (typically `172.x.x.1` or found via `ip route show | grep default`)
- **Helper Script**: `bin/st` provides a command-line interface to send Smalltalk code
- **Protocol**: Simple TCP socket communication using netcat (`nc`)

### Usage Patterns

#### Basic Execution
```bash
# Direct command execution
./bin/st "Object allSubclasses size"

# Multi-line code from stdin
./bin/st <<'ST'
| pairs |
pairs := OrderedCollection new.
Object withAllSubclasses do: [:c | 
  pairs add: (Array with: c name with: c selectors size)].
pairs size
ST

# Execute from file
./bin/st -f scripts/example.st

# Mirror to Transcript (-T flag)
./bin/st -T "Transcript show: 'Hello from Claude'; cr"
```

#### Configuration
Configuration is managed through `config/smalltalk.conf` with the following parameters:
- `SMALLTALK_HOST` - Windows host IP as seen from WSL (find with: `ip route show | grep default | awk '{print $3}'`)
- `SMALLTALK_PORT` - Port where Dolphin TCP server is listening
- `SMALLTALK_TIMEOUT` (default: `8` seconds)

Environment variables can still override these settings. Use `bin/st-config` to manage configuration:
```bash
# Show current config
./bin/st-config show

# Set Windows host IP (find with: ip route show | grep default | awk '{print $3}')
./bin/st-config set SMALLTALK_HOST 172.25.224.1

# Change port
./bin/st-config set SMALLTALK_PORT 8097
```

### Logging and Output
- `var/last.st`: Last Smalltalk code sent
- `var/last.out`: Last raw output received
- `var/st.log`: Append-only log with timestamps, host:port, code, and output

## Legacy Integration History

### Previous ChatGPT/Codex Integration
The repository contains evidence of a previous integration system designed for ChatGPT/Codex:

#### ClaudeIntegration Package Structure
- **ClaudeCodeClient**: Main interface for AI communication
- **ClaudeCodeTcpServer**: TCP server for bidirectional communication
- **ClaudeCodeRequest/Response**: Request/response handling classes
- **AIWorkspace**: Specialized workspace for AI interactions
- **Component Builder**: GUI generation system

#### Communication Pattern
The legacy system used:
1. **JSON-based messaging** with session management
2. **Node.js bridge script** (`claude-bridge-resume.js`) for handling requests
3. **WSL integration** for cross-platform compatibility
4. **Complete JSON response parsing** with timeout handling
5. **Session persistence** and resume capabilities

#### Key Features
- 10-minute timeout for complex operations
- Session ID tracking and history
- GUI component generation from specifications
- Real-time response streaming
- Error handling and recovery

## Current Claude Code Integration

### Simplified Protocol
The current system uses a simpler, more reliable approach:
- Direct TCP communication via `bin/st`
- No intermediate JSON layers
- Immediate response handling
- Built-in logging and debugging

### Best Practices

#### Quoting and Escaping
- Smalltalk uses single quotes for strings, double quotes for comments
- For complex code, prefer `-f <file>` or stdin to avoid shell escaping
- Use doubled single quotes to embed quotes: `'That''s fine'`

#### Method Compilation
```smalltalk
# Direct compilation (preferred)
(Smalltalk at: #SDL3Library)
  compile: 'methodName
    <cdecl: return_type FunctionName param_types>
    ^self invalidCall: _failureCode'
  classified: 'category'.

# For complex methods with pragmas, use file input:
./bin/st -f scripts/compile_method.st
```

#### Error Handling
- Network timeouts are handled by netcat `-w` flag
- Connection failures return empty output
- All communication is logged for debugging

## Development Workflow

### Code Execution
1. Claude Code sends Smalltalk expressions via `bin/st`
2. Dolphin server evaluates and returns results
3. Results are logged and returned to Claude Code
4. Complex operations can be built incrementally

### Package Management
```smalltalk
# File out packages
Package manager packageNamed: 'SDL3' fileOut.

# Load packages
Package manager loadPackageNamed: 'MyPackage'.

# Save image
SessionManager current saveImage.
```

### Debugging
- Use `-T` flag to mirror output to Transcript
- Check `var/st.log` for complete interaction history
- Use `var/last.st` and `var/last.out` for immediate debugging

## Security Considerations

- The TCP server accepts arbitrary Smalltalk code execution
- Network access is restricted and requires elevated permissions
- All code execution is logged for audit purposes
- The system is designed for development environments only

## Integration Examples

### Simple Query
```bash
./bin/st "Object allSubclasses size"
# Returns: 2506
```

### Complex Analysis
```bash
./bin/st -T <<'ST'
| report |
report := String streamContents: [:s |
  Object withAllSubclasses do: [:cls |
    s nextPutAll: cls name; 
      nextPutAll: ' (';
      nextPutAll: cls selectors size printString;
      nextPutAll: ' methods)'; cr
  ]
].
report
ST
```

### Method Compilation
```bash
./bin/st -f scripts/update_sdl3.st
```

This architecture enables Claude Code to provide sophisticated Smalltalk development assistance with real-time image introspection and modification capabilities.
