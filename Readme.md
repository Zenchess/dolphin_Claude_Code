# Dolphin Smalltalk Claude Code Integration Project

## What it is

A bridge between Claude Code and Dolphin Smalltalk that enables real-time Smalltalk code execution in your live image. Very powerful, especially for setting up external interfacing libraries.

### Component Builder Feature

There's also a component builder class (work in progress) that can load `.gui` files. An example GUI lets you call back into Claude from the image using the `sendToClaude:` message and get responses. You can actually work with Claude without the Claude Code GUI open this way, or execute these commands in the workspace. Note: Claude running this way operates with skip dangerous permissions mode on, so there's no way to stop what it's doing until it completes a task.

## Quick Start Instructions

1. **Setup**: Put your image file in this folder and run Claude Code:
   ```bash
   claude
   ```

2. **Configuration**: Claude should read the `CLAUDE.md` file automatically. If it doesn't, tell it to read it.

3. **Start the Dolphin Server**: In Dolphin, load the `ClaudeIntegration` package and execute:
   ```smalltalk
   server := ClaudeCodeTcpServer new. 
   server start
   ```
   
   > 📝 **Note**: Check the TCP port shown in the system transcript - you may need to tell Claude about it.

4. **Communication**: Claude will send commands to the `bin/st` script that communicates with the Dolphin TCP bridge.

### Requirements

- **WSL**: You must run Claude Code in WSL for this to work (though you could modify the project if Claude Code runs in Windows)

---

## Detailed Documentation

### Communication Architecture

#### TCP Bridge Protocol
- **Endpoint**: The Dolphin server listens on the Windows host IP as seen from WSL (typically `172.x.x.1` or found via `ip route show | grep default`)
- **Helper Script**: `bin/st` provides a command-line interface to send Smalltalk code
- **Protocol**: Simple TCP socket communication using netcat (`nc`)

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

### Basic Usage

The `bin/st` script provides a simple interface to send Smalltalk code to the running Dolphin image:

```bash
# Basic command
./bin/st "1 + 2"

# Get help
./bin/st -h
```

### Logging and Output
- `var/last.st`: Last Smalltalk code sent
- `var/last.out`: Last raw output received
- `var/st.log`: Append-only log with timestamps, host:port, code, and output


### Legacy Integration History

This repository contains evidence of a previous integration system designed for ChatGPT/Codex:

#### ClaudeIntegration Package Structure
- **ClaudeCodeClient**: Main interface for AI communication
- **ClaudeCodeTcpServer**: TCP server for bidirectional communication
- **ClaudeCodeRequest/Response**: Request/response handling classes
- **AIWorkspace**: Specialized workspace for AI interactions
- **Component Builder**: GUI generation system

#### Previous Features
- JSON-based messaging with session management
- Node.js bridge script (`claude-bridge-resume.js`) for handling requests
- WSL integration for cross-platform compatibility
- Complete JSON response parsing with timeout handling
- Session persistence and resume capabilities
- 10-minute timeout for complex operations
- Session ID tracking and history
- GUI component generation from specifications
- Real-time response streaming
- Error handling and recovery

### Current Claude Code Integration

#### Simplified Protocol
The current system uses a simpler, more reliable approach:
- Direct TCP communication via `bin/st`
- No intermediate JSON layers
- Immediate response handling
- Built-in logging and debugging

### Security Considerations

- The TCP server accepts arbitrary Smalltalk code execution
- Network access is restricted and requires elevated permissions
- All code execution is logged for audit purposes
- The system is designed for development environments only

---

This architecture enables Claude Code to provide sophisticated Smalltalk development assistance with real-time image introspection and modification capabilities.