| package |
package := Package name: 'ClaudeIntegration'.
package paxVersion: 1;
	preDeclareClassesOnLoad: false;
	basicComment: 'Claude Code CLI Integration for Dolphin Smalltalk
Copyright (c) 2025

This package provides integration with Claude Code CLI tool, allowing Dolphin Smalltalk to communicate with Claude Code through stdin/stdout pipes for enhanced development assistance.

The package includes:
- ClaudeCodeClient: Main interface for Claude Code communication
- ClaudeCodeProcess: Process management for long-running Claude Code sessions
- ClaudeCodeRequest/Response: Request/response handling classes
- ClaudeCodeError: Error handling for Claude Code operations

For documentation and examples, see the class comments for each component.'.

package basicPackageVersion: '1.0'.

package classNames
	add: #AIWorkspace;
	add: #ClaudeCodeClient;
	add: #ClaudeCodeError;
	add: #ClaudeCodeRequest;
	add: #ClaudeCodeResponse;
	add: #ClaudeCodeTcpServer;
	add: #ComponentBuilder;
	yourself.

package methodNames
	add: #ShellView -> #initializeClaudeGUI;
	add: #ShellView -> #loadPreviousSessions;
	add: #ShellView -> #newSession;
	add: #ShellView -> #resumeSession;
	add: #ShellView -> #sendToClaude;
	add: #ShellView -> #updateSessionDisplay;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Core\Object Arts\Dolphin\IDE\Base\Development System'
	'..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Radio\Dolphin Radio Buttons'
	'..\Core\Object Arts\Dolphin\Sockets\Dolphin Sockets'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter'
	'..\Core\Contributions\Burning River\ExternalProcess\ExternalProcess'
	'..\Core\Contributions\svenc\STON\STON-Core').

package!

"Class Definitions"!

Object subclass: #ClaudeCodeClient
	instanceVariableNames: 'lastResponse isConnected claudeExecutablePath workingDirectory process dummy'
	classVariableNames: 'DefaultExecutablePath'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ClaudeCodeRequest
	instanceVariableNames: 'prompt files options'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ClaudeCodeResponse
	instanceVariableNames: 'content exitCode success errorMessage'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ClaudeCodeTcpServer
	instanceVariableNames: 'serverSocket isRunning port serverProcess inputPipe outputPipe process'
	classVariableNames: 'ClaudeResponse CurrentServer CurrentSessionId CurrentShellView DefaultPort SessionHistory'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ComponentBuilder
	instanceVariableNames: 'components commands shell currentX currentY'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Error subclass: #ClaudeCodeError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

SmalltalkWorkspace subclass: #AIWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Loose Methods"!

!ShellView methodsFor!

initializeClaudeGUI
    "Initialize Claude GUI components, register with server, and load sessions"
    ClaudeCodeTcpServer CurrentShellView: self.
    self updateSessionDisplay.
    self loadPreviousSessions!

loadPreviousSessions
    "Load session IDs from our tracked session history"
    | sessionSelector sessionHistory |
    sessionSelector := self viewNamed: #sessionSelector ifNone: [^self].
    
    "Get our tracked session history"
    sessionHistory := ClaudeCodeTcpServer SessionHistory.
    
    "Populate dropdown with tracked sessions"
    sessionHistory isEmpty 
        ifTrue: [sessionSelector list: #('No previous sessions - start a conversation')]
        ifFalse: [sessionSelector list: sessionHistory asArray]!

newSession
    "Start a new Claude session"
    | statusLabel |
    statusLabel := self viewNamed: #statusLabel ifNone: [^self].
    statusLabel text: 'Creating new session...'.
    ClaudeCodeTcpServer CurrentSessionId: nil.
    self updateSessionDisplay.
    statusLabel text: 'New session ready - next message will start fresh conversation'!

resumeSession
    "Resume a selected session from the dropdown"
    | sessionSelector statusLabel selectedSession |
    sessionSelector := self viewNamed: #sessionSelector ifNone: [^self].
    statusLabel := self viewNamed: #statusLabel ifNone: [^self].
    selectedSession := sessionSelector text.
    selectedSession isEmpty ifTrue: [
        statusLabel text: 'Please select a session to resume'.
        ^self
    ].
    ClaudeCodeTcpServer CurrentSessionId: selectedSession.
    self updateSessionDisplay.
    statusLabel text: 'Resumed session: ', selectedSession!

sendToClaude
    "GUI command handler - get text from messageInput and send to Claude"
    | messageInput responseArea statusLabel messageText response |
    
    "Get the GUI components"
    messageInput := self viewNamed: #messageInput ifNone: [^self].
    responseArea := self viewNamed: #responseArea ifNone: [^self].
    statusLabel := self viewNamed: #statusLabel ifNone: [^self].
    
    "Get the message text"
    messageText := messageInput text .
    messageText isEmpty ifTrue: [
        statusLabel text: 'Please enter a message'.
        ^self
    ].
    
    "Update status"
    statusLabel text: 'Sending to Claude...'.
    
    "Send to Claude using our working CLASS method"
    response := ClaudeCodeTcpServer  sendToClaudeWithCompleteJSON: messageText.
    
    "Update the response area"
    responseArea text: response.
    
    "Update session display"
    self updateSessionDisplay.
    
    "Update status"
    statusLabel text: 'Response received'.
    
    "Clear the input"
    messageInput text: ''.!

updateSessionDisplay
    "Update the session ID display in the GUI"
    | sessionDisplay currentSessionId |
    sessionDisplay := self viewNamed: #sessionDisplay ifNone: [^self].
    currentSessionId := ClaudeCodeTcpServer CurrentSessionId.
    sessionDisplay text: (currentSessionId ifNil: ['None'] ifNotNil: [currentSessionId asString])! !

!ShellView categoriesForMethods!
initializeClaudeGUI!Claude Integration!public! !
loadPreviousSessions!Claude Integration!public! !
newSession!Claude Integration!public! !
resumeSession!Claude Integration!public! !
sendToClaude!Claude Integration!public! !
updateSessionDisplay!Claude Integration!public! !
!

"End of package definition"!

"Source Globals"!

"Classes"!

ClaudeCodeClient guid: (GUID fromString: '{a1b2c3d4-e5f6-7890-abcd-ef1234567890}')!

ClaudeCodeClient comment: 'ClaudeCodeClient provides a high-level interface for communicating with Claude Code CLI tool from within Dolphin Smalltalk.

This class manages Claude Code process communication. It uses ExternalProcess for process management and handles file-based communication to avoid process spawning issues.

Example usage:
	client := ClaudeCodeClient new.
	response := client sendPrompt: ''Explain this Smalltalk code''.
	response content.

Key features:
- Simple Claude Code CLI integration
- Automatic process lifecycle handling  
- Error handling and recovery
- Support for different Claude Code executable paths
- Working directory management for project context'!

!ClaudeCodeClient categoriesForClass!Claude Integration! !

!ClaudeCodeClient methodsFor!

claudeExecutablePath
	"Answer the path to the Claude Code executable"
	
	^claudeExecutablePath ifNil: [self class defaultExecutablePath]!

claudeExecutablePath: aString
	"Set the path to the Claude Code executable"
	
	claudeExecutablePath := aString!

convertWindowsPathToWSL: windowsPath
	"Convert C:\path\file to /mnt/c/path/file for WSL access"
	
	| wslPath |
	wslPath := windowsPath copyReplacing: $\ withObject: $/.
	wslPath := wslPath copyReplaceAll: 'C:/' with: '/mnt/c/'.
	^wslPath!

executeClaudeWithPrompt: promptText  |  promptFile outputFile stream result | promptFile := 'C:\smalltalk\claude\prompt.txt'. outputFile := 'C:\smalltalk\claude\out.txt'. (File exists: outputFile) ifTrue: [File delete: outputFile]. (FileStream write: promptFile text: true) nextPutAll: promptText; close. (ExternalProcess new commandLine: 'C:\smalltalk\claude\claude-call-interactive.bat') executeSync. stream := FileStream read: outputFile text: true. result := stream contents. stream close. ^result!

fileTest  |  path | path := 'C:\test.txt'. ^path!

initialize
	"Initialize a new ClaudeCodeClient instance"
	
	super initialize.
	isConnected := false.
	workingDirectory := '.'!

lastResponse
	"Answer the last response received from Claude Code"
	
	^lastResponse!

sendPrompt: promptString
	"Send a prompt to Claude Code and return the response"
	
	| request |
	request := ClaudeCodeRequest new
		prompt: promptString;
		yourself.
		
	lastResponse := self sendRequest: request.
	^lastResponse!

sendRequest: aClaudeCodeRequest
	"Send a request to Claude Code and return the response"
	
	| commandText response |
	
	[
		"Build the command text"
		commandText := aClaudeCodeRequest prompt.
		
		"Execute Claude Code with the prompt"
		response := self executeClaudeWithPrompt: commandText.
		
		lastResponse := ClaudeCodeResponse new
			content: response;
			success: true;
			yourself.
			
	] on: Error do: [:error |
		lastResponse := ClaudeCodeResponse new
			errorMessage: error messageText;
			yourself.
	].
	
	^lastResponse!

simpleTest ^123!

testMethod ^42!

workingDirectory
	"Answer the working directory for Claude Code operations"
	
	^workingDirectory!

workingDirectory: aString
	"Set the working directory for Claude Code operations"
	
	workingDirectory := aString! !

!ClaudeCodeClient categoriesForMethods!
claudeExecutablePath!public! !
claudeExecutablePath:!public! !
convertWindowsPathToWSL:!public! !
executeClaudeWithPrompt:!AI Integration!claude integration!public! !
fileTest!public!testing! !
initialize!public! !
lastResponse!public! !
sendPrompt:!public! !
sendRequest:!public! !
simpleTest!public!testing! !
testMethod!public!testing! !
workingDirectory!public! !
workingDirectory:!public! !
!

!ClaudeCodeClient class methodsFor!

defaultExecutablePath
	"Answer the default path to the Claude Code executable"
	
	^DefaultExecutablePath ifNil: ['claude']!

defaultExecutablePath: aString
	"Set the default path to the Claude Code executable"
	
	DefaultExecutablePath := aString!

new
	"Create a new ClaudeCodeClient instance"
	
	^super new initialize! !

!ClaudeCodeClient class categoriesForMethods!
defaultExecutablePath!public! !
defaultExecutablePath:!public! !
new!public! !
!

ClaudeCodeRequest guid: (GUID fromString: '{b2c3d4e5-f6a7-8901-bcde-f23456789012}')!

ClaudeCodeRequest comment: 'ClaudeCodeRequest represents a request to be sent to Claude Code CLI.

A request contains:
- prompt: The main prompt/question to send to Claude
- files: Optional collection of file paths to include for context
- options: Optional dictionary of additional options

Example usage:
	request := ClaudeCodeRequest new
		prompt: ''Explain this code'';
		files: #(''myfile.st'');
		yourself.
'!

!ClaudeCodeRequest categoriesForClass!Claude Integration! !

!ClaudeCodeRequest methodsFor!

files
	"Answer the collection of file paths to include with this request"
	
	^files ifNil: [files := OrderedCollection new]!

files: aCollection
	"Set the collection of file paths to include with this request"
	
	files := aCollection!

initialize
	"Initialize a new ClaudeCodeRequest"
	
	super initialize.
	options := Dictionary new!

options
	"Answer the options dictionary for this request"
	
	^options!

options: aDictionary
	"Set the options dictionary for this request"
	
	options := aDictionary!

prompt
	"Answer the prompt text for this request"
	
	^prompt!

prompt: aString
	"Set the prompt text for this request"
	
	prompt := aString! !

!ClaudeCodeRequest categoriesForMethods!
files!public! !
files:!public! !
initialize!public! !
options!public! !
options:!public! !
prompt!public! !
prompt:!public! !
!

!ClaudeCodeRequest class methodsFor!

new
	"Create a new ClaudeCodeRequest"
	
	^super new initialize!

withPrompt: aString
	"Create a new request with the specified prompt"
	
	^self new
		prompt: aString;
		yourself! !

!ClaudeCodeRequest class categoriesForMethods!
new!public! !
withPrompt:!public! !
!

ClaudeCodeResponse guid: (GUID fromString: '{c3d4e5f6-a7b8-9012-cdef-345678901234}')!

ClaudeCodeResponse comment: 'ClaudeCodeResponse represents a response received from Claude Code CLI.

A response contains:
- content: The main response text from Claude
- exitCode: The process exit code (0 for success)
- success: Boolean indicating if the request was successful
- errorMessage: Error message if the request failed

Example usage:
	response := client sendPrompt: ''some prompt''.
	response isSuccess ifTrue: [
		Transcript show: response content
	] ifFalse: [
		Transcript show: ''Error: '', response errorMessage
	].
'!

!ClaudeCodeResponse categoriesForClass!Claude Integration! !

!ClaudeCodeResponse methodsFor!

content
	"Answer the response content from Claude"
	
	^content!

content: aString
	"Set the response content from Claude"
	
	content := aString!

errorMessage
	"Answer the error message if the request failed"
	
	^errorMessage!

errorMessage: aString
	"Set the error message"
	
	errorMessage := aString.
	success := false!

exitCode
	"Answer the process exit code"
	
	^exitCode ifNil: [0]!

exitCode: anInteger
	"Set the process exit code"
	
	exitCode := anInteger.
	success := (exitCode = 0)!

initialize
	"Initialize a new ClaudeCodeResponse"
	
	super initialize.
	success := false.
	content := ''!

isSuccess
	"Answer whether the request was successful"
	
	^success ifNil: [false]!

success: aBoolean
	"Set whether the request was successful"
	
	success := aBoolean! !

!ClaudeCodeResponse categoriesForMethods!
content!public! !
content:!public! !
errorMessage!public! !
errorMessage:!public! !
exitCode!public! !
exitCode:!public! !
initialize!public! !
isSuccess!public! !
success:!public! !
!

!ClaudeCodeResponse class methodsFor!

new
	"Create a new ClaudeCodeResponse"
	
	^super new initialize!

withContent: aString
	"Create a successful response with the specified content"
	
	^self new
		content: aString;
		success: true;
		yourself!

withError: aString
	"Create an error response with the specified error message"
	
	^self new
		errorMessage: aString;
		yourself! !

!ClaudeCodeResponse class categoriesForMethods!
new!public! !
withContent:!public! !
withError:!public! !
!

ClaudeCodeTcpServer guid: (GUID fromString: '{e5f6a7b8-c9d0-1234-5678-90abcdef1234}')!

ClaudeCodeTcpServer comment: 'ClaudeCodeTcpServer provides a TCP server that allows Claude Code to execute Smalltalk expressions directly in the live Dolphin image.

The server listens on localhost:8080 (by default) and accepts simple text connections. Claude Code can send raw Smalltalk expressions and receive the results back as strings.

Example usage:
	server := ClaudeCodeTcpServer new.
	server start.
	"Claude Code can now connect and send Smalltalk code"
	server stop.

Protocol:
	Claude Code sends: "Object allSubclasses size"
	Server responds: "1247"

This gives Claude Code full access to inspect, modify, and interact with the live Smalltalk system.'!

!ClaudeCodeTcpServer categoriesForClass!Claude Integration! !

!ClaudeCodeTcpServer methodsFor!

checkClaudeResponse
	"Check if Claude response is complete (contains ###END###)"
	| response |
	response := self class ClaudeResponse.
	^(response findString: '###END###') > 0!

defaultPort
	"Answer the default port for the Claude Code server"
	
	^self class defaultPort!

evaluateCode: codeString
	"Evaluate code with comprehensive error reporting"
	| result  err|
	
	result := [
		Compiler evaluate: codeString
	] on: Error do: [:error |
		
	 
		err := error class name asString , ': ', error printString.
		
	]
	 on: CompilerErrorNotification do: [:error |
	
      err := error class name asString , ': ', error  printString.
      err
  ] 
	on: Exception do: [:error | 
	
	err := error class name asString, ': ', error printString. 
	err].
	
	"Always return a string representation"
	err ifNotNil: [^err].

	result ifNil: [^'nil'] ifNotNil: [^result printString]
!

extractResultFromBridgeResponse: jsonResponse
	"Extract the result text from the bridge JSON response"
	| lines resultLine |
	
	"Split response into lines and find the result line"
	lines := jsonResponse subStrings: Character cr.
	resultLine := lines detect: [:line | line contains: 'result'] ifNone: ['No result found'].
	
	"For now, just return the last line that contains text"
	lines do: [:line | 
		(line contains: '###END###') ifTrue: [
			"Extract just the result part before ###END###"
			| endPos |
			endPos := line findString: '###END###'.
			endPos > 1 ifTrue: [^(line leftString: endPos - 1) withBlanksTrimmed]
		]
	].
	
	^'Could not extract result from: ', jsonResponse!

getClaudeResponse
	"Get the completed Claude response, removing ###END### marker"
	| string size |
	string := self class ClaudeResponse.
	size := string size.
	^string leftString: size - 9!

handleClient: aSocket
	"Handle communication with a connected Claude Code client - runs in forked process"
	
	| readStream writeStream codeString result endTime dataFound chunks data |
	
	[
		Transcript show: 'Client connected'; cr.
		aSocket isText: true.
		
		readStream := aSocket readStream.
		writeStream := aSocket writeStream.
		
		"Wait for data to arrive with timeout"
		endTime := Time millisecondClockValue + 5000.
		dataFound := false.
		[Time millisecondClockValue < endTime and: [dataFound not]] whileTrue: [
			readStream hasInput ifTrue: [
				Transcript show: 'Data available'; cr.
				dataFound := true].
			dataFound ifFalse: [(Delay forMilliseconds: 10) wait]].
		
		readStream hasInput ifFalse: [
			Transcript show: 'Timeout waiting for data'; cr.
			^self
		].
		
		"Read everything until connection closes - canonical Dolphin pattern"
		[
			codeString := readStream upToEnd.
		] on: Stream endOfStreamSignal do: [:ex |
			Transcript show: 'Connection closed - normal'; cr.
			codeString ifNil: [codeString := '']
		].
		Transcript show: 'Raw length: ', codeString size printString; cr.
		codeString := codeString trimBlanks.
		
		"Convert the injected '< /dev/null |' back to pipe characters"
		codeString := codeString copyReplaceAll: ' < /dev/null | ' with: ' | '.
		codeString := codeString copyReplaceAll: ' < /dev/null |' with: ' |'.
		codeString := codeString copyReplaceAll: '< /dev/null | ' with: '| '.
		codeString := codeString copyReplaceAll: '< /dev/null |' with: '|'.
		
		codeString isEmpty
			ifTrue: [ Transcript show: 'No code received'; cr ]
			ifFalse: [
				Transcript show: 'Received: ', codeString; cr.
				result := self evaluateCode: codeString.
				Transcript show: 'Sending: ', result; cr.
				writeStream nextPutAll: result; cr; flush.
			].
	] ensure: [
		aSocket close.
		Transcript show: 'Client disconnected'; cr.
	]!

initialize
	"Initialize a new ClaudeCodeTcpServer"
	
	super initialize.
	port := self defaultPort.
	isRunning := false!

inputPipe
	^inputPipe!

isRunning
	"Answer whether the server is currently running"
	
	^isRunning!

outputPipe
	^outputPipe!

port
	"Answer the port the server listens on"
	
	^port!

port: anInteger
	"Set the port for the server to listen on"
	
	port := anInteger!

safeReadLineFrom: aReadStream withTimeout: timeoutMs
    "Safely read a line with timeout - returns nil if no data"
    | startTime |
    startTime := Time millisecondClockValue.
    
    [(Time millisecondClockValue - startTime) < timeoutMs] whileTrue: [
        aReadStream atEnd ifFalse: [
            [^aReadStream nextLine] on: Error do: [:ex | ^nil]
        ].
        Processor sleep: 10.
    ].
    ^nil!

sendToClaude: messageText
    "Send message using our new working JSON method"
    ^ClaudeCodeTcpServer sendToClaudeWithCompleteJSON: messageText!

sendToClaudeBlocking: messageText
	"Send message to Claude and wait for response - uses Processor yield"
	| enhancedPrompt startTime timeout |
	self class ClaudeResponse: ''.
	
	"Include the END marker instruction in the prompt"
	enhancedPrompt := messageText , ' (Please end your response with ###END###)'.
	
	inputPipe writeStream
		nextPutAll: enhancedPrompt;
		cr;
		flush.
	
	"Wait for response while yielding control"
	startTime := Time millisecondClockValue.
	timeout := 30000.  "30 second timeout"
	
	[(self class ClaudeResponse findString: '###END###') = 0 and: [
		(Time millisecondClockValue - startTime) < timeout
	]] whileTrue: [
		Processor yield.  "Yield control to keep UI responsive"
		Processor sleep: 100.
	].
	
	"Check if we timed out"
	(Time millisecondClockValue - startTime) >= timeout ifTrue: [
		^'ERROR: Claude response timed out after 30 seconds'
	].
	
	"Return cleaned response"
	^self getClaudeResponse!

sendToClaudeWithBridge: messageText
	"Send message to Claude using the working resume bridge"
	| enhancedPrompt response process |
	
	"Include the END marker instruction in the prompt"
	enhancedPrompt := messageText , ' (Please end your response with ###END###)'.
	
	"Execute the working bridge script"
	process := ExternalProcess new
		commandLine: 'node claude-bridge-resume.js ', enhancedPrompt printString;
		executeSync.
	
	"Get the response"
	response := process outputText.
	
	"Extract just the result text from the JSON response"
	^self extractResultFromBridgeResponse: response!

sendToClaudeWithCallback: messageText callback: aBlock
	"Send message to Claude and execute callback when response arrives - NON-BLOCKING"
	| enhancedPrompt |
	self class ClaudeResponse: ''.
	
	"Include the END marker instruction in the prompt"
	enhancedPrompt := messageText , ' (Please end your response with ###END###)'.
	
	inputPipe writeStream
		nextPutAll: enhancedPrompt;
		cr;
		flush.
	
	"Start background process to wait for response"
	[
		"Wait in background thread - this won't block UI"
		[self class ClaudeResponse findString: '###END###'] whileFalse: [
			Processor sleep: 100
		].
		"Execute callback with cleaned response"
		aBlock value: self getClaudeResponse
	] fork.
	
	^'Message sent - callback will be executed when response arrives'!

sendToClaudeWithSessionManagement: messageText
    |size string answer enhancedPrompt sessionData sessionId|
    "Send message to Claude with session management using non-blocking pattern"
    self class ClaudeResponse: ''.
    
    "Include the END marker instruction in the prompt"
    enhancedPrompt := messageText , ' (Please end your response with ###END###)'.
    
    inputPipe writeStream
        nextPutAll: enhancedPrompt;
        cr;
        flush.

    "Wait for response with ###END### marker"
    [(self class ClaudeResponse findString: '###END###') = 0] whileTrue: [ 
        Processor sleep: 100.  "Increased sleep to reduce CPU load"
    ].
    
    string := self class ClaudeResponse.
    size := string size. 
                
    answer := string leftString: size - 9.
    
    "Try to parse JSON to extract session ID for future use"
    [ sessionData := STON fromString: string.
      sessionId := sessionData at: #session_id ifAbsent: [ nil ].
      sessionId ifNotNil: [ 
          "Store session ID for future use"
          self class CurrentSessionId: sessionId 
      ]
    ] on: Error do: [ :ex | 
        "If JSON parsing fails, ignore and continue"
    ].
    
    ^answer!

sendToClaudeWithSessionManagementOneShot: messageText
    "Send message using one-shot bridge execution with proper pipe reading"
    | process result sessionData sessionId startTime timeout outputPipe line |
    
    "Clear previous response"
    self class ClaudeResponse: ''.
    result := ''.
    
    "Set timeout (30 seconds)"
    timeout := 30000.
    startTime := Time millisecondClockValue.
    
    "Execute bridge as one-shot process with message as argument"
    process := ExternalProcess new
        commandLine: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js ', messageText printString.
    outputPipe := process stdoutPipe.
    [process executeSync] fork.
    
    "Wait for process to become alive"
    [process isAlive] whileFalse: [
        (Time millisecondClockValue - startTime) >= timeout ifTrue: [
            ^'ERROR: Process failed to start within timeout'
        ].
        Processor sleep: 10.
    ].
    
    "Read output lines as they arrive with timeout"
    [process isTerminated not and: [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
        [outputPipe readStream atEnd] whileFalse: [
            line := outputPipe readStream nextLine.
            line ifNotNil: [
                result := result , line , String cr.
                "Try to extract session ID from JSON lines"
                [ sessionData := STON fromString: line.
                  sessionId := sessionData at: #session_id ifAbsent: [ nil ].
                  sessionId ifNotNil: [ 
                      self class CurrentSessionId: sessionId.
                      Transcript show: 'Session ID: ', sessionId; cr
                  ]
                ] on: Error do: [ :ex | "Ignore non-JSON lines" ]
            ]
        ].
        Processor sleep: 50.
    ].
    
    "Check for timeout"
    (Time millisecondClockValue - startTime) >= timeout ifTrue: [
        process terminate.
        ^'ERROR: Bridge process timed out after 30 seconds'
    ].
    
    "Store response and extract clean answer"
    self class ClaudeResponse: result.
    (result findString: '###END###') > 0 
        ifTrue: [ | endPos |
            endPos := result findString: '###END###'.
            ^(result leftString: endPos - 1) withBlanksTrimmed
        ]
        ifFalse: [ ^result withBlanksTrimmed ]!

sendToClaudeWithSessionManagementSafe: messageText
    "Send message using cmd + node command with SAFE non-blocking reads"
    | bridgeProcess result sessionData sessionId startTime timeout outputPipe inputPipe line |
    
    "Clear previous response"
    self class ClaudeResponse: ''.
    result := ''.
    
    "Set timeout (30 seconds)"
    timeout := 30000.
    startTime := Time millisecondClockValue.
    
    "Start cmd process first"
    bridgeProcess := ExternalProcess new
        commandLine: 'cmd'.
    outputPipe := bridgeProcess stdoutPipe.
    inputPipe := bridgeProcess stdinPipe.
    [bridgeProcess executeSync] fork.
    
    "Wait for cmd to become alive"
    [bridgeProcess isAlive] whileFalse: [
        (Time millisecondClockValue - startTime) >= timeout ifTrue: [
            ^'ERROR: CMD failed to start within timeout'
        ].
        Processor sleep: 100.
    ].
    
    "Send the actual node bridge command"
    inputPipe writeStream
        nextPutAll: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js ', messageText printString;
        cr;
        flush.
    
    "Read output lines using SAFE method"
    [bridgeProcess isAlive and: [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
        line := self safeReadLineFrom: outputPipe readStream withTimeout: 1000.
        line ifNotNil: [
            "Skip Windows command prompt output"
            ((line beginsWith: 'Microsoft') not and: 
             [(line beginsWith: '(c) Microsoft') not and:
              [(line beginsWith: 'C:') not and: [line notEmpty]]]) ifTrue: [
                result := result , line , String cr.
                "Try to extract session ID from JSON lines"
                [ sessionData := STON fromString: line.
                  sessionId := sessionData at: #session_id ifAbsent: [ nil ].
                  sessionId ifNotNil: [ 
                      self class CurrentSessionId: sessionId.
                      Transcript show: 'Session ID: ', sessionId; cr
                  ]
                ] on: Error do: [ :ex | "Ignore non-JSON lines" ].
                "Check if we got the end marker"
                (line findString: '###END###') > 0 ifTrue: [
                    "Send exit to terminate cmd"
                    inputPipe writeStream nextPutAll: 'exit'; cr; flush.
                    "Response complete"
                    ^(result copyReplaceAll: '###END###' with: '') withBlanksTrimmed
                ]
            ]
        ].
        Processor sleep: 100.
    ].
    
    "Cleanup - send exit to cmd"
    inputPipe writeStream nextPutAll: 'exit'; cr; flush.
    
    "Check for timeout"
    (Time millisecondClockValue - startTime) >= timeout ifTrue: [
        ^'ERROR: Bridge process timed out after 30 seconds'
    ].
    
    "Store response and return result"
    self class ClaudeResponse: result.
    ^result withBlanksTrimmed!

serverLoop
	"Main server loop - accept and handle client connections (runs in background)"
	
	| clientSocket |
	
	[isRunning] whileTrue: [
		[
			Transcript show: 'Waiting for connection on port ', port printString; cr.
			
			"This blocks but it's OK because we're in a forked process"
			clientSocket := serverSocket accept.
			
			clientSocket ifNotNil: [
				"Handle each client in its own forked process"
				| server |
				server := self.
				[server handleClient: clientSocket] fork
			]
		] on: Error do: [:error |
			"Log errors but keep server running"
			Transcript show: 'Server error: ', error messageText; cr.
			"If socket is closed, stop the server"
			error messageText = 'Socket is closed' ifTrue: [isRunning := false]
		]
	].
	
	Transcript show: 'Server loop exited'; cr!

setupClaudeBridge
	"Set up connection to claude-headless-bridge.js"
	| process |
	process := ExternalProcess new.
	process 
		commandLine: 'node /mnt/c/smalltalk/claude/claude-headless-bridge.js';
		createProcessSuspended.
	inputPipe := process.
	^'Claude bridge connection established'!

start
    self class CurrentServer: self.
    isRunning ifTrue: [^self].
    serverSocket := ServerSocket2 port: self defaultPort.
    isRunning := true.
    Transcript show: 'Claude Code TCP Server started on port ', port printString; cr.
    serverProcess := [self serverLoop] fork.
    serverProcess name: 'Claude Code TCP Server'.
    process := ExternalProcess new.
    process commandLine: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js'.
    outputPipe := process stdoutPipe.
    inputPipe := process stdinPipe.
    [process executeSync] fork.
    
    [self class ClaudeResponse: ''.
    [outputPipe readStream atEnd] whileFalse: [
        | response jsonData sessionId |
        response := outputPipe readStream nextLine.
        response ifNotNil: [
            response notEmpty ifTrue: [
                [jsonData := STON fromString: response.
                 sessionId := jsonData at: #session_id ifAbsent: [nil].
                 sessionId ifNotNil: [
                     self class CurrentSessionId: sessionId.
                     Transcript show: 'Session ID: ', sessionId; cr
                 ]] on: Error do: [:ex | ].
                self class ClaudeResponse: self class ClaudeResponse, response, String cr.
                Transcript show: 'Response: ', response; cr; flush
            ]
        ]
    ]] fork!

stop
	"Stop the Claude Code TCP server"
	
	isRunning := false.
	
	serverSocket ifNotNil: [
		serverSocket close.
		serverSocket := nil
	].
	
	serverProcess ifNotNil: [
		serverProcess terminate.
		serverProcess := nil
	].
	
	Transcript show: 'Claude Code TCP Server stopped'; cr.

   inputPipe ifNotNil: [ inputPipe writeStream 
						nextPutAll: 'exit';
						cr;
						flush].
!

testBridgeProcessAlive
    "Test if we can start a simple process and get it alive"
    | bridgeProcess startTime timeout outputPipe inputPipe line |
    
    timeout := 10000.
    startTime := Time millisecondClockValue.
    
    "Start with simple cmd first"
    bridgeProcess := ExternalProcess new
        commandLine: 'cmd'.
    outputPipe := bridgeProcess stdoutPipe.
    inputPipe := bridgeProcess stdinPipe.
    [bridgeProcess executeSync] fork.
    
    "Wait for process to become alive"
    [bridgeProcess isAlive] whileFalse: [
        (Time millisecondClockValue - startTime) >= timeout ifTrue: [
            ^'ERROR: CMD process failed to start within timeout'
        ].
        Transcript show: 'Waiting for CMD to be alive...'; cr.
        Processor sleep: 100.
    ].
    
    Transcript show: 'CMD process is alive\!!'; cr.
    
    "Now send the actual node command"
    inputPipe writeStream
        nextPutAll: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js hello test';
        cr;
        flush.
    
    "Try to read some output"
    3 timesRepeat: [
        line := self safeReadLineFrom: outputPipe readStream withTimeout: 2000.
        line ifNotNil: [
            Transcript show: 'Got line: ', line; cr
        ] ifNil: [
            Transcript show: 'No line read'; cr
        ].
        Processor sleep: 500.
    ].
    
    bridgeProcess terminate.
    ^'Test completed'!

testSafety: msg | count | count := 0. [count < 10] whileTrue: [count := count + 1. Processor sleep: 100]. ^'Completed safely'! !

!ClaudeCodeTcpServer categoriesForMethods!
checkClaudeResponse!Claude Integration!public! !
defaultPort!accessing!public! !
evaluateCode:!evaluation!operations!public! !
extractResultFromBridgeResponse:!Claude Integration!public! !
getClaudeResponse!Claude Integration!public! !
handleClient:!operations!public! !
initialize!initialization!public! !
inputPipe!public! !
isRunning!accessing!public! !
outputPipe!public! !
port!accessing!public! !
port:!accessing!public! !
safeReadLineFrom:withTimeout:!public!utilities! !
sendToClaude:!Claude Integration!public! !
sendToClaudeBlocking:!Claude Integration!public! !
sendToClaudeWithBridge:!Claude Integration!public! !
sendToClaudeWithCallback:callback:!Claude Integration!public! !
sendToClaudeWithSessionManagement:!Claude Integration!public! !
sendToClaudeWithSessionManagementOneShot:!Claude Integration!public! !
sendToClaudeWithSessionManagementSafe:!Claude Integration!public! !
serverLoop!operations!public! !
setupClaudeBridge!Claude Integration!public! !
start!operations!public! !
stop!operations!public! !
testBridgeProcessAlive!public! !
testSafety:!public!testing! !
!

!ClaudeCodeTcpServer class methodsFor!

addShellView: aShellView "Add shell view to collection" | collection | collection := self currentShellView. collection add: aShellView. ^aShellView!

addToSessionHistory: sessionId
    "Add a session ID to our tracked history"
    | history |
    history := self SessionHistory.
    (history includes: sessionId) ifFalse: [
        history addFirst: sessionId.
        "Keep only last 20 sessions"
        history size > 20 ifTrue: [history removeLast]
    ]!

ClaudeResponse
	^ClaudeResponse!

ClaudeResponse: responseString
	ClaudeResponse := responseString!

CurrentServer
	^CurrentServer!

CurrentServer: anInstance
	CurrentServer := anInstance!

CurrentSessionId ^CurrentSessionId!

CurrentSessionId: aString CurrentSessionId := aString!

currentShellView "Answer collection of shell views" CurrentShellView ifNil: [CurrentShellView := OrderedCollection new]. ^CurrentShellView!

CurrentShellView ^CurrentShellView!

currentShellView: aView CurrentShellView := aView!

CurrentShellView: aShellView CurrentShellView := aShellView!

defaultPort
	"Answer the default port for Claude Code server"
	
	^8097!

defaultPort: anInteger
	"Set the default port for Claude Code server"
	
	DefaultPort := anInteger!

new
	"Create a new ClaudeCodeTcpServer instance"
	
	^super new initialize!

SendToClaude: aTextMessage | server | server := CurrentServer. server ifNil: [^'No server running']. server inputPipe ifNotNil: [server inputPipe writeStream nextPutAll: aTextMessage; cr; flush. ^'Message sent to Claude via headless bridge'] ifNil: [^'Bridge not connected']!

sendToClaudeSimpleForked: messageText
      "Simple forked reader exactly like example2"
      | outputPipe inputPipe bridgeProcess startTime timeout filtered lines |

      "Clear response and set timeout"
      self ClaudeResponse: ''.
      startTime := Time millisecondClockValue.
      timeout := 30000.

      "Create process exactly like example"
      bridgeProcess := ExternalProcess new.
      bridgeProcess commandLine: 'cmd'.
      outputPipe := bridgeProcess stdoutPipe.
      inputPipe := bridgeProcess stdinPipe.
      [bridgeProcess executeSync] fork.

      "Simple forked reader exactly like example2"
      [
          [outputPipe readStream atEnd] whileFalse: [
              self ClaudeResponse: self ClaudeResponse , outputPipe readStream nextLine , String cr
          ]
      ] fork.

      "Wait until process is alive"
      [bridgeProcess isAlive] whileFalse: [
          (Time millisecondClockValue - startTime) >= timeout ifTrue: [
              ^'ERROR: Process failed to start'
          ].
          Processor sleep: 100.
      ].

      "Send the node command"
      inputPipe writeStream
          nextPutAll: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js ', messageText printString;
          cr;
          flush.

      "Wait for ###END### with timeout"
      [(self ClaudeResponse findString: '###END###') = 0 and:
       [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
          Processor sleep: 200.
      ].

      "Cleanup"
      inputPipe writeStream nextPutAll: 'exit'; cr; flush.

      "Check timeout"
      (Time millisecondClockValue - startTime) >= timeout ifTrue: [
          ^'ERROR: Timeout - got: ', self ClaudeResponse
      ].

      "Filter and return result"
      lines := self ClaudeResponse subStrings: String cr.
      filtered := lines select: [:line |
          (line findString: 'Microsoft') = 0 and:
          [(line findString: '(c)') = 0 and:
           [(line beginsWith: 'C:\') not and:
            [line notEmpty]]]
      ].
      ^(filtered joinSeparatedBy: String cr) copyReplaceAll: '###END###' with: ''!

sendToClaudeWithBulletproofTimeout: messageText
      "BULLETPROOF method with non-blocking read"
      | bridgeProcess result line startTime endTime loopCount |

      self ClaudeResponse: ''.
      result := ''.
      startTime := Time millisecondClockValue.
      endTime := startTime + 30000.
      loopCount := 0.

      bridgeProcess := ExternalProcess new commandLine: 'cmd'.
      [bridgeProcess executeSync] fork.

      "Wait for cmd with hard loop limit"
      [((bridgeProcess isAlive not) and: [Time millisecondClockValue < endTime]) and: [loopCount < 100]] whileTrue: [
          loopCount := loopCount + 1.
          Processor sleep: 100.
      ].

      bridgeProcess isAlive ifFalse: [^'ERROR: CMD failed to start'].

      "Send node command"
      bridgeProcess stdinPipe writeStream
          nextPutAll: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js ', messageText printString;
          cr; flush.

      "Read with HARD loop limit and NON-BLOCKING check"
      loopCount := 0.
      [Time millisecondClockValue < endTime and: [loopCount < 30]] whileTrue: [
          loopCount := loopCount + 1.
          
          "NON-BLOCKING check for data"
          line := nil.
          bridgeProcess stdoutPipe readStream atEnd ifFalse: [
              line := bridgeProcess stdoutPipe readStream nextLine.
          ].
          
          line ifNotNil: [
              (line notEmpty and: [(line findString: 'Microsoft') = 0]) ifTrue: [
                  result := result , line , String cr.
                  (line findString: '###END###') > 0 ifTrue: [
                      bridgeProcess stdinPipe writeStream nextPutAll: 'exit'; cr; flush.
                      ^(result copyReplaceAll: '###END###' with: '') withBlanksTrimmed.
                  ].
              ].
          ].
          Processor sleep: 200.
      ].

      "GUARANTEED cleanup"
      bridgeProcess stdinPipe writeStream nextPutAll: 'exit'; cr; flush.
      ^'Timeout: ', loopCount printString, ' loops completed'!

sendToClaudeWithCompleteJSON: messageText
      "Wait for complete JSON responses with longer timeout"
      | outputPipe inputPipe bridgeProcess startTime timeout filtered lines jsonData sessionId content responseComplete |

      "Clear response and set LONG timeout (10 minutes)"
      self ClaudeResponse: ''.
      startTime := Time millisecondClockValue.
      timeout := 600000.

      "Create WSL process"
      bridgeProcess := ExternalProcess new.
      bridgeProcess commandLine: 'wsl'.
      outputPipe := bridgeProcess stdoutPipe.
      inputPipe := bridgeProcess stdinPipe.
      [bridgeProcess executeSync] fork.

      "Simple forked reader"
      [
          [outputPipe readStream atEnd] whileFalse: [
              self ClaudeResponse: self ClaudeResponse , outputPipe readStream nextLine , String cr
          ]
      ] fork.

      "Wait until process is alive"
      [bridgeProcess isAlive] whileFalse: [
          (Time millisecondClockValue - startTime) >= timeout ifTrue: [
              ^'ERROR: WSL failed to start'
          ].
          Processor sleep: 100.
      ].

      "Send the node command"
      inputPipe writeStream
          nextPutAll: 'node claude-bridge-resume.js ', messageText printString;
          cr;
          flush.

      "Wait for COMPLETE JSON response - check for proper closing"
      content := nil.
      sessionId := nil.
      responseComplete := false.
      [responseComplete not and: [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
          "Try to parse each line as complete JSON"
          lines := self ClaudeResponse subStrings: String cr.
          lines do: [:line |
              line notEmpty ifTrue: [
                  "Check if this line looks like a complete JSON object"
                ((line beginsWith: '{') and: [line endsWith: '}']) ifTrue: [
                      [ jsonData := STON fromString: line.
                        "Extract session ID if available"
                        (jsonData at: #session_id ifAbsent: [nil]) ifNotNil: [:id |
                            sessionId ifNil: [
                                sessionId := id.
                                self CurrentSessionId: sessionId.
                                self addToSessionHistory: sessionId.
                                self CurrentShellView ifNotNil: [:shell | shell updateSessionDisplay].
                                Transcript show: 'Session ID: ', sessionId; cr
                            ]
                        ].
                        "Look for assistant message with content"
                        (jsonData at: #type ifAbsent: [nil]) = 'assistant' ifTrue: [
                            | message |
                            message := jsonData at: #message ifAbsent: [Dictionary new].
                            (message at: #content ifAbsent: [#()]) do: [:item |
                                (item at: #type ifAbsent: [nil]) = 'text' ifTrue: [
                                    content := item at: #text ifAbsent: [''].
                                    content notEmpty ifTrue: [
                                        "Found complete assistant response"
                                        responseComplete := true.
                                        inputPipe writeStream nextPutAll: 'exit'; cr; flush.
                                        ^content withBlanksTrimmed
                                    ]
                                ]
                            ]
                        ]
                      ] on: Error do: [:ex | "Skip malformed JSON" ]
                  ]
              ]
          ].
          "Pump UI messages to prevent blocking"
          SessionManager inputState pumpMessages.
          Processor sleep: 500.
      ].

      "Cleanup"
      inputPipe writeStream nextPutAll: 'exit'; cr; flush.

      "Check timeout"
      (Time millisecondClockValue - startTime) >= timeout ifTrue: [
          ^'ERROR: 10-minute timeout - session: ', (sessionId ifNil: ['none']), ' lines: ', lines size printString
      ].

      ^content ifNil: ['No complete assistant response found'] ifNotNil: [content ]!

sendToClaudeWithForkedReader: messageText
      "Use forked reader pattern like example2 - CANNOT freeze"
      | outputPipe inputPipe bridgeProcess startTime timeout |

      "Clear response and set timeout"
      self ClaudeResponse: ''.
      startTime := Time millisecondClockValue.
      timeout := 30000.

      "Create process"
      bridgeProcess := ExternalProcess new.
      bridgeProcess commandLine: 'cmd'.
      outputPipe := bridgeProcess stdoutPipe.
      inputPipe := bridgeProcess stdinPipe.
      [bridgeProcess executeSync] fork.

      "Start forked reader that accumulates to ClaudeResponse"
      [
          [outputPipe readStream atEnd] whileFalse: [
              | line |
              line := outputPipe readStream nextLine.
              line ifNotNil: [
                  "Filter out Windows output AND command prompts"
                  ((line findString: 'Microsoft') = 0 and:
                   [(line findString: '(c)') = 0 and:
                    [(line beginsWith: 'C:\') not and:
                     [line notEmpty]]]) ifTrue: [
                      self ClaudeResponse: self ClaudeResponse , line , String cr
                  ]
              ]
          ]
      ] fork.

      "Wait until process is alive"
      [bridgeProcess isAlive] whileFalse: [
          (Time millisecondClockValue - startTime) >= timeout ifTrue: [
              ^'ERROR: Process failed to start'
          ].
          Processor sleep: 100.
      ].

      "Send the node command"
      inputPipe writeStream
          nextPutAll: 'node /mnt/c/smalltalk/claude/claude-bridge-resume.js ', messageText printString;
          cr;
          flush.

      "Wait for ###END### in ClaudeResponse with timeout"
      [(self ClaudeResponse findString: '###END###') = 0 and:
       [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
          Processor sleep: 200.
      ].

      "Cleanup"
      inputPipe writeStream nextPutAll: 'exit'; cr; flush.

      "Check for timeout"
      (Time millisecondClockValue - startTime) >= timeout ifTrue: [
          ^'ERROR: Timeout waiting for ###END###'
      ].

      "Return clean result"
      ^(self ClaudeResponse copyReplaceAll: '###END###' with: '') !

sendToClaudeWithJSONParsing: messageText
      "Parse JSON responses instead of waiting for ###END###"
      | outputPipe inputPipe bridgeProcess startTime timeout filtered lines jsonData sessionId content |

      "Clear response and set timeout"
      self ClaudeResponse: ''.
      startTime := Time millisecondClockValue.
      timeout := 30000.

      "Create WSL process"
      bridgeProcess := ExternalProcess new.
      bridgeProcess commandLine: 'wsl'.
      outputPipe := bridgeProcess stdoutPipe.
      inputPipe := bridgeProcess stdinPipe.
      [bridgeProcess executeSync] fork.

      "Simple forked reader"
      [
          [outputPipe readStream atEnd] whileFalse: [
              self ClaudeResponse: self ClaudeResponse , outputPipe readStream nextLine , String cr
          ]
      ] fork.

      "Wait until process is alive"
      [bridgeProcess isAlive] whileFalse: [
          (Time millisecondClockValue - startTime) >= timeout ifTrue: [
              ^'ERROR: WSL failed to start'
          ].
          Processor sleep: 100.
      ].

      "Send the node command"
      inputPipe writeStream
          nextPutAll: 'node claude-bridge-resume.js ', messageText printString;
          cr;
          flush.

      "Wait for complete JSON response using STON parsing"
      content := nil.
      sessionId := nil.
      [content isNil and: [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
          "Try to parse each line as JSON to find assistant response"
          lines := self ClaudeResponse subStrings: String cr.
          lines do: [:line |
              line notEmpty ifTrue: [
                  [ jsonData := STON fromString: line.
                    "Extract session ID if available"
                    (jsonData at: #session_id ifAbsent: [nil]) ifNotNil: [:id |
                        sessionId ifNil: [
                            sessionId := id.
                            self CurrentSessionId: sessionId.
                            Transcript show: 'Session ID: ', sessionId; cr
                        ]
                    ].
                    "Look for assistant message with content"
                    (jsonData at: #type ifAbsent: [nil]) = 'assistant' ifTrue: [
                        | message |
                        message := jsonData at: #message ifAbsent: [Dictionary new].
                        (message at: #content ifAbsent: [#()]) do: [:item |
                            (item at: #type ifAbsent: [nil]) = 'text' ifTrue: [
                                content := item at: #text ifAbsent: [''].
                                content notEmpty ifTrue: [
                                    "Found complete assistant response"
                                    inputPipe writeStream nextPutAll: 'exit'; cr; flush.
                                    ^content 
                                ]
                            ]
                        ]
                    ]
                  ] on: Error do: [:ex | "Skip non-JSON lines" ]
              ]
          ].
          Processor sleep: 200.
      ].

      "Cleanup"
      inputPipe writeStream nextPutAll: 'exit'; cr; flush.

     
	
      ^content ifNil: ['No assistant response found'] ifNotNil: [content ]!

sendToClaudeWSLForked: messageText
      "Simple forked reader using WSL instead of cmd"
      | outputPipe inputPipe bridgeProcess startTime timeout filtered lines |

      "Clear response and set timeout"
      self ClaudeResponse: ''.
      startTime := Time millisecondClockValue.
      timeout := 30000.

      "Create WSL process instead of cmd"
      bridgeProcess := ExternalProcess new.
      bridgeProcess commandLine: 'wsl'.
      outputPipe := bridgeProcess stdoutPipe.
      inputPipe := bridgeProcess stdinPipe.
      [bridgeProcess executeSync] fork.

      "Simple forked reader exactly like example2"
      [
          [outputPipe readStream atEnd] whileFalse: [
              self ClaudeResponse: self ClaudeResponse , outputPipe readStream nextLine , String cr
          ]
      ] fork.

      "Wait until process is alive"
      [bridgeProcess isAlive] whileFalse: [
          (Time millisecondClockValue - startTime) >= timeout ifTrue: [
              ^'ERROR: WSL failed to start'
          ].
          Processor sleep: 100.
      ].

      "Send the node command in WSL context"
      inputPipe writeStream
          nextPutAll: 'node claude-bridge-resume.js ', messageText printString;
          cr;
          flush.

      "Wait for ###END### with timeout"
      [(self ClaudeResponse findString: '###END###') = 0 and:
       [(Time millisecondClockValue - startTime) < timeout]] whileTrue: [
          Processor sleep: 200.
      ].

      "Cleanup"
      inputPipe writeStream nextPutAll: 'exit'; cr; flush.

      "Check timeout"
      (Time millisecondClockValue - startTime) >= timeout ifTrue: [
          ^'ERROR: Timeout - got: ', self ClaudeResponse
      ].

      "Filter and return result"
      lines := self ClaudeResponse subStrings: String cr.
      filtered := lines select: [:line |
          (line findString: 'Microsoft') = 0 and:
          [(line findString: 'Welcome') = 0 and:
           [(line beginsWith: '$') not and:
            [line notEmpty]]]
      ].
      ^(filtered joinSeparatedBy: String cr) copyReplaceAll: '###END###' with: '' withBlanksTrimmed!

SessionHistory ^SessionHistory ifNil: [SessionHistory := OrderedCollection new]!

startWithSkipPermissions "Start server with Claude Code configured to skip permissions" | server | server := self new. server start. ^server! !

!ClaudeCodeTcpServer class categoriesForMethods!
addShellView:!public! !
addToSessionHistory:!public!session management! !
ClaudeResponse!public! !
ClaudeResponse:!public! !
CurrentServer!public! !
CurrentServer:!public! !
CurrentSessionId!accessing!public! !
CurrentSessionId:!accessing!public! !
currentShellView!public! !
CurrentShellView!accessing!public! !
currentShellView:!public! !
CurrentShellView:!accessing!public! !
defaultPort!class initialization!public! !
defaultPort:!class initialization!public! !
new!class initialization!public! !
SendToClaude:!public! !
sendToClaudeSimpleForked:!class initialization!Claude Integration!public! !
sendToClaudeWithBulletproofTimeout:!Claude Integration!public! !
sendToClaudeWithCompleteJSON:!Claude Integration!claude integration!public! !
sendToClaudeWithForkedReader:!class initialization!Claude Integration!public! !
sendToClaudeWithJSONParsing:!Claude Integration!public! !
sendToClaudeWSLForked:!Claude Integration!public! !
SessionHistory!public!session management! !
startWithSkipPermissions!public! !
!

ComponentBuilder guid: (GUID fromString: '{fc00a84a-2c48-4c5f-9d9c-a9cf39e84f1a}')!

ComponentBuilder comment: ''!

!ComponentBuilder categoriesForClass!Kernel-Objects! !

!ComponentBuilder methodsFor!

addCommandHandlingTo: shell "Add command handling and force button enabling" "Add queryCommand to the shell class" shell class compile: 'queryCommand: aCommandQuery ""Enable Claude interface commands"" | command | command := aCommandQuery command. (#(sendMessageToClaude clearInterface) includes: command) ifTrue: [aCommandQuery enabled: true. ^true]. ^super queryCommand: aCommandQuery' classified: 'command handling'. "Force enable all command buttons" shell subViews do: [:view | (view respondsTo: #commandDescription) ifTrue: [view commandDescription ifNotNil: [view enable. view invalidate]]]. "Refresh the shell" shell invalidate!

advanceLayout: extent "Move to next position for component" currentY := currentY + extent y + 10. currentX := 10!

buildApp: spec "Build complete application using official Dolphin patterns" | className appClass sourceCode | className := spec at: #className ifAbsent: ['GeneratedApp', Time now millisecondClockValue printString]. sourceCode := self generateAppClass: spec className: className. Compiler evaluate: sourceCode. appClass := Smalltalk at: className asSymbol. ^appClass new show!

buildButton: spec "Build button component" | button name width height command | name := spec at: #name ifAbsent: ['button', components size printString]. width := spec at: #width ifAbsent: [100]. height := spec at: #height ifAbsent: [30]. command := spec at: #command ifAbsent: [#doNothing]. button := PushButton new. button parentView: shell. button createAt: currentX@currentY extent: width@height. button text: (spec at: #text ifAbsent: ['Button']). shell addSubView: button name: name. button commandDescription: (CommandDescription command: command description: (spec at: #tooltip ifAbsent: ['Button action'])). button enable. components at: name put: button. commands at: command put: spec. self advanceLayout: width@height. ^button!

buildCommandHandlingInto: shell "Build command handling directly into the shell object" | enabledCommands | enabledCommands := Set new. "Collect all commands from components" shell subViews do: [:view | (view respondsTo: #commandDescription) ifTrue: [view commandDescription ifNotNil: [enabledCommands add: view commandDescription command]]]. "Create singleton method on this specific shell instance to handle commands" shell class compile: ('queryCommand: aCommandQuery | command | command := aCommandQuery command. (', enabledCommands asArray printString, ' includes: command) ifTrue: [aCommandQuery beEnabled. ^true]. ^false') classified: 'autogenerated commands'. "Enable all command buttons" shell subViews do: [:view | (view respondsTo: #commandDescription) ifTrue: [view commandDescription ifNotNil: [view enable]]]!

buildComponent: spec "Enhanced component builder with proper text setting" | type component | type := spec at: #type. component := self createComponent: type spec: spec. self positionComponent: component spec: spec. self registerComponent: component spec: spec. self configureComponent: component spec: spec. ^component!

buildComponentReliably: spec forShell: shell "Build component with proper command setup" | type component name x y width height text command | type := spec at: #type. name := spec at: #name ifAbsent: ['component', components size printString]. x := spec at: #x ifAbsent: [10]. y := spec at: #y ifAbsent: [10]. width := spec at: #width ifAbsent: [100]. height := spec at: #height ifAbsent: [25]. text := spec at: #text ifAbsent: ['']. command := spec at: #command ifAbsent: [nil]. "Create component based on type" type = #Label ifTrue: [component := StaticText new]. type = #Button ifTrue: [component := PushButton new]. type = #Input ifTrue: [component := TextEdit new]. type = #TextArea ifTrue: [component := MultilineTextEdit new]. type = #ListBox ifTrue: [component := ListBox new]. type = #ComboBox ifTrue: [component := ComboBox new]. type = #CheckBox ifTrue: [component := CheckButton new]. type = #RadioButton ifTrue: [component := RadioButton new]. type = #GroupBox ifTrue: [component := GroupBox new]. component ifNil: [^nil]. "Set up component" component parentView: shell. component createAt: x@y extent: width@height. component text: text. "Set special properties" type = #TextArea ifTrue: [component wordWrap: true. (spec at: #readonly ifAbsent: [false]) ifTrue: [component isReadOnly: true]]. "Set command BEFORE registering" type = #Button ifTrue: [command ifNotNil: [component commandDescription: (CommandDescription command: command description: text)]]. "Register component" shell addSubView: component name: name. components at: name put: component. component enable. "Add special event handlers for Claude interface" (name = 'messageInput' and: [type = #TextArea]) ifTrue: [component when: #keyPressed send: #onMessageInputKeyPressed: to: shell]. ^component!

buildInput: spec "Build text input component" | input name width height | name := spec at: #name ifAbsent: ['input', components size printString]. width := spec at: #width ifAbsent: [200]. height := spec at: #height ifAbsent: [25]. input := TextEdit new. input parentView: shell. input createAt: currentX@currentY extent: width@height. input text: (spec at: #placeholder ifAbsent: ['']). shell addSubView: input name: name. components at: name put: input. self advanceLayout: width@height. ^input!

buildInterface: componentSpec "Build GUI from component specification with scaling fix" | spec | spec := componentSpec. shell := ShellView new. shell createAt: (spec at: #position ifAbsent: [100@100]) extent: (spec at: #size ifAbsent: [600@400]). shell text: (spec at: #title ifAbsent: ['Generated Interface']). shell scaleChildCreation: false. currentX := 10. currentY := 10. components := Dictionary new. commands := Dictionary new. (spec at: #components ifAbsent: [#()]) do: [:compSpec | self buildComponent: compSpec]. shell show. ClaudeCodeTcpServer addShellView: shell. ^shell!

buildInterfaceReliably: componentSpec "Build interface with automatic command handling built-in" | spec shell | spec := componentSpec. shell := ShellView new. shell createAt: (spec at: #position ifAbsent: [100@100]) extent: (spec at: #size ifAbsent: [600@400]). shell text: (spec at: #title ifAbsent: ['Generated Interface']). shell isResizable: true. components := Dictionary new. commands := Dictionary new. (spec at: #components ifAbsent: [#()]) do: [:compSpec | self buildComponentReliably: compSpec forShell: shell]. "Build command handling into the shell automatically" self buildCommandHandlingInto: shell. shell show. ClaudeCodeTcpServer addShellView: shell. ^shell!

buildLabel: spec "Build static text label" | label name width height | name := spec at: #name ifAbsent: ['label', components size printString]. width := spec at: #width ifAbsent: [200]. height := spec at: #height ifAbsent: [20]. label := StaticText new. label parentView: shell. label createAt: currentX@currentY extent: width@height. label text: (spec at: #text ifAbsent: ['Label']). label backcolor: nil. shell addSubView: label name: name. components at: name put: label. self advanceLayout: width@height. ^label!

buildSpacer: spec "Build spacer for layout" | height | height := spec at: #height ifAbsent: [10]. self advanceLayout: 0@height. ^nil!

buildTextArea: spec "Build multiline text area" | area name width height | name := spec at: #name ifAbsent: ['textarea', components size printString]. width := spec at: #width ifAbsent: [400]. height := spec at: #height ifAbsent: [200]. area := MultilineTextEdit new. area parentView: shell. area createAt: currentX@currentY extent: width@height. area text: (spec at: #text ifAbsent: ['']). (spec at: #readonly ifAbsent: [false]) ifTrue: [area disable]. shell addSubView: area name: name. components at: name put: area. self advanceLayout: width@height. ^area!

configureComponent: component spec: spec "Configure all component properties with debug" component ifNil: [^self]. component parentView: shell. Transcript show: 'Configuring component: ', component class name; cr. spec keysAndValuesDo: [:key :value | Transcript show: 'Setting ', key printString, ' to ', value printString; cr. self setProperty: key value: value on: component]!

createComponent: type spec: spec "Create the appropriate component type" type = #Input ifTrue: [^TextEdit new]. type = #Button ifTrue: [^PushButton new]. type = #TextArea ifTrue: [^MultilineTextEdit new]. type = #Label ifTrue: [^StaticText new]. type = #ListBox ifTrue: [^ListBox new]. type = #ComboBox ifTrue: [^ComboBox new]. type = #CheckBox ifTrue: [^CheckButton new]. type = #RadioButton ifTrue: [^RadioButton new]. type = #GroupBox ifTrue: [^GroupBox new]. type = #Spacer ifTrue: [^nil]. self error: 'Unknown component type: ', type printString!

deserializeInterfaceSpec: specString "Parse interface spec from string format" ^Compiler evaluate: specString!

extractComponentSpec: aView "Extract specification from individual component including commands" | spec type | spec := Dictionary new. type := self getComponentType: aView. spec at: #type put: type. spec at: #name put: (aView name ifNil: ['unnamed']). spec at: #x put: aView position x. spec at: #y put: aView position y. spec at: #width put: aView extent x. spec at: #height put: aView extent y. (aView respondsTo: #text) ifTrue: [spec at: #text put: aView text]. (aView respondsTo: #isReadOnly) ifTrue: [aView isReadOnly ifTrue: [spec at: #readonly put: true]]. (aView respondsTo: #isEnabled) ifTrue: [aView isEnabled ifFalse: [spec at: #enabled put: false]]. "Extract command description" (aView respondsTo: #commandDescription) ifTrue: [aView commandDescription ifNotNil: [spec at: #command put: aView commandDescription command]]. ^spec!

extractSpec: aShellView "Extract component specification from existing ShellView" | spec components | spec := Dictionary new. spec at: #title put: aShellView text. spec at: #size put: aShellView extent. spec at: #position put: aShellView position. components := OrderedCollection new. aShellView subViews do: [:view | | compSpec | compSpec := self extractComponentSpec: view. compSpec ifNotNil: [components add: compSpec]]. spec at: #components put: components. ^spec!

generateAppClass: spec className: className "Generate complete ShellView subclass source" | source components | components := spec at: #components ifAbsent: [#()]. source := String writeStream. source nextPutAll: 'ShellView subclass: #', className; cr. source nextPutAll: '    instanceVariableNames: ''; self generateInstanceVars: components on: source; nextPut: $'; cr. source nextPutAll: '    classVariableNames: '''; cr. source nextPutAll: '    poolDictionaries: '''; cr. source nextPutAll: '    classInstanceVariableNames: '''; cr; cr. self generateOnViewOpened: spec on: source. self generateEventHandlers: spec on: source. self generateDefaultExtent: spec on: source. ^source contents!

getComponentType: aView "Determine component type from view class" aView class = PushButton ifTrue: [^#Button]. aView class = TextEdit ifTrue: [^#Input]. aView class = MultilineTextEdit ifTrue: [^#TextArea]. aView class = StaticText ifTrue: [^#Label]. aView class = ListBox ifTrue: [^#ListBox]. aView class = ComboBox ifTrue: [^#ComboBox]. aView class = CheckButton ifTrue: [^#CheckBox]. aView class = RadioButton ifTrue: [^#RadioButton]. aView class = GroupBox ifTrue: [^#GroupBox]. ^#Unknown!

loadInterface: filename "Load interface specification from file and build it properly" | file specString interfaceSpec shell | file := FileStream read: filename text: true. specString := file contents. file close. interfaceSpec := self deserializeInterfaceSpec: specString. shell := self buildInterfaceReliably: interfaceSpec. ^shell!

positionAbsolute: component spec: spec "Position at exact coordinates" | x y width height | x := spec at: #x ifAbsent: [10]. y := spec at: #y ifAbsent: [10]. width := spec at: #width ifAbsent: [100]. height := spec at: #height ifAbsent: [25]. component createAt: x@y extent: width@height!

positionComponent: component spec: spec "Position component with flexible layout options" | x y width height layout | component ifNil: [^self]. layout := spec at: #layout ifAbsent: [#flow]. layout = #absolute ifTrue: [^self positionAbsolute: component spec: spec]. layout = #grid ifTrue: [^self positionGrid: component spec: spec]. layout = #flow ifTrue: [^self positionFlow: component spec: spec]. layout = #manual ifTrue: [^self positionManual: component spec: spec]!

positionFlow: component spec: spec "Position in flowing layout" | width height | width := spec at: #width ifAbsent: [200]. height := spec at: #height ifAbsent: [25]. component createAt: currentX@currentY extent: width@height. self advanceLayout: width@height!

positionGrid: component spec: spec "Grid-based positioning" | row col cellWidth cellHeight | row := spec at: #row ifAbsent: [0]. col := spec at: #col ifAbsent: [0]. cellWidth := spec at: #cellWidth ifAbsent: [100]. cellHeight := spec at: #cellHeight ifAbsent: [30]. component createAt: (col * (cellWidth + 10) + 10)@(row * (cellHeight + 10) + 10) extent: cellWidth@cellHeight!

positionManual: component spec: spec "Manual positioning with rectangle" | rect | rect := spec at: #rectangle ifAbsent: [10@10 corner: 110@35]. component rectangle: rect!

registerComponent: component spec: spec "Register component and set up events and properties" | name | component ifNil: [^self]. name := spec at: #name ifAbsent: ['component', components size printString]. shell addSubView: component name: name. components at: name put: component. component enable. "Apply text property after registration" (spec includesKey: #text) ifTrue: [component text: (spec at: #text)]. self setupEvents: component spec: spec!

saveInterface: interfaceSpec toFile: filename "Save interface specification to file for later loading" | file specString | file := FileStream write: filename text: true. specString := self serializeInterfaceSpec: interfaceSpec. file nextPutAll: specString; close. ^filename!

serializeInterfaceSpec: spec "Convert interface spec to saveable string format" ^spec storeString!

setProperty: key value: value on: component "Set any property on component" key = #text ifTrue: [component text: value. ^self]. key = #placeholder ifTrue: [component text: value. ^self]. key = #readonly ifTrue: [value ifTrue: [component isReadOnly: true. component isTabStop: false]. ^self]. key = #enabled ifTrue: [value ifTrue: [component enable] ifFalse: [component disable]. ^self]. key = #visible ifTrue: [value ifTrue: [component show] ifFalse: [component hide]. ^self]. key = #backcolor ifTrue: [component backcolor: (value = #transparent ifTrue: [nil] ifFalse: [value]). ^self]. key = #font ifTrue: [component font: value. ^self]. key = #tooltip ifTrue: [component helpText: value. ^self]. key = #command ifTrue: [component commandDescription: (CommandDescription command: value description: (component text ifNil: ['Button'])). ^self]. "Skip layout/meta properties" (#(type name width height x y layout row col cellWidth cellHeight rectangle) includes: key) ifTrue: [^self]!

setupEvents: component spec: spec "Safe event setup" | events | component ifNil: [^self]. shell ifNil: [^self]. events := spec at: #events ifAbsent: [^self]. events keysAndValuesDo: [:event :handler | [component when: event send: handler to: shell] on: Error do: [:ex | "Skip event setup errors"]]!

writeSpec: spec to: stream indent: level "Write spec to stream with proper indentation" | indent nextIndent | indent := String new: level * 2 withAll: $\s. nextIndent := String new: (level + 1) * 2 withAll: $\s. stream nextPutAll: indent, '#('. spec keysAndValuesDo: [:key :value | stream cr, nextIndent, '#', key printString, ' '. (value isKindOf: Collection) ifTrue: [value isString ifTrue: [stream nextPutAll: value printString] ifFalse: [stream nextPutAll: '#('. value do: [:item | stream cr, nextIndent, nextIndent. self writeSpec: item to: stream indent: level + 2] separatedBy: []. stream cr, nextIndent, ')']] ifFalse: [stream nextPutAll: value printString]]. stream cr, indent, ')'! !

!ComponentBuilder categoriesForMethods!
addCommandHandlingTo:!public! !
advanceLayout:!public! !
buildApp:!public! !
buildButton:!public! !
buildCommandHandlingInto:!public! !
buildComponent:!public! !
buildComponentReliably:forShell:!public! !
buildInput:!public! !
buildInterface:!public! !
buildInterfaceReliably:!public! !
buildLabel:!public! !
buildSpacer:!public! !
buildTextArea:!public! !
configureComponent:spec:!public! !
createComponent:spec:!public! !
deserializeInterfaceSpec:!public! !
extractComponentSpec:!public! !
extractSpec:!public! !
generateAppClass:className:!public! !
getComponentType:!public! !
loadInterface:!public! !
positionAbsolute:spec:!public! !
positionComponent:spec:!public! !
positionFlow:spec:!public! !
positionGrid:spec:!public! !
positionManual:spec:!public! !
registerComponent:spec:!public! !
saveInterface:toFile:!public! !
serializeInterfaceSpec:!public! !
setProperty:value:on:!public! !
setupEvents:spec:!public! !
writeSpec:to:indent:!public! !
!

!ComponentBuilder class methodsFor!

createClaudeMessageTemplate "Create template for Claude messaging interface" | spec components | spec := Dictionary new. spec at: #title put: 'Claude Message Interface'. spec at: #size put: 600@400. spec at: #position put: 100@100. components := OrderedCollection new. components add: (Dictionary new at: #type put: #Label; at: #name put: 'titleLabel'; at: #text put: 'Message to Claude:'; at: #width put: 400; at: #height put: 20; yourself). components add: (Dictionary new at: #type put: #TextArea; at: #name put: 'messageInput'; at: #width put: 580; at: #height put: 100; at: #text put: 'Type your message here...'; yourself). components add: (Dictionary new at: #type put: #Button; at: #name put: 'sendButton'; at: #text put: 'Send to Claude'; at: #command put: #sendMessageToClaude; at: #width put: 120; at: #height put: 30; yourself). components add: (Dictionary new at: #type put: #Label; at: #name put: 'responseLabel'; at: #text put: 'Claude Response:'; at: #width put: 400; at: #height put: 20; yourself). components add: (Dictionary new at: #type put: #TextArea; at: #name put: 'responseArea'; at: #width put: 580; at: #height put: 150; at: #text put: 'Claude responses will appear here...'; at: #readonly put: true; yourself). components add: (Dictionary new at: #type put: #Button; at: #name put: 'clearButton'; at: #text put: 'Clear'; at: #command put: #clearInterface; at: #width put: 80; at: #height put: 30; yourself). components add: (Dictionary new at: #type put: #Label; at: #name put: 'statusLabel'; at: #text put: 'Ready to send message'; at: #width put: 580; at: #height put: 20; yourself). spec at: #components put: components. self saveTemplate: 'claudeMessage' spec: spec. ^spec!

createDataEntryTemplate "Create template for data entry forms" | spec components | spec := Dictionary new. spec at: #title put: 'Data Entry Form'. spec at: #size put: 400@350. spec at: #position put: 150@150. components := OrderedCollection new. components add: (Dictionary new at: #type put: #Label; at: #text put: 'Name:'; at: #width put: 80; at: #height put: 20; yourself). components add: (Dictionary new at: #type put: #Input; at: #name put: 'nameField'; at: #width put: 200; at: #height put: 25; yourself). components add: (Dictionary new at: #type put: #Label; at: #text put: 'Email:'; at: #width put: 80; at: #height put: 20; yourself). components add: (Dictionary new at: #type put: #Input; at: #name put: 'emailField'; at: #width put: 200; at: #height put: 25; yourself). components add: (Dictionary new at: #type put: #Label; at: #text put: 'Description:'; at: #width put: 80; at: #height put: 20; yourself). components add: (Dictionary new at: #type put: #TextArea; at: #name put: 'descField'; at: #width put: 350; at: #height put: 80; yourself). components add: (Dictionary new at: #type put: #Button; at: #text put: 'Submit'; at: #command put: #submitForm; at: #width put: 80; at: #height put: 30; yourself). components add: (Dictionary new at: #type put: #Button; at: #text put: 'Cancel'; at: #command put: #cancelForm; at: #width put: 80; at: #height put: 30; yourself). spec at: #components put: components. self saveTemplate: 'dataEntry' spec: spec. ^spec!

listTemplates "List all available interface templates" | templateDir files | templateDir := 'C:\smalltalk\claude\templates\'. (File exists: templateDir) ifFalse: [^#()]. files := File search: templateDir, '*.gui'. ^files collect: [:path | (File splitPath: path) last copyFrom: 1 to: (File splitPath: path) last size - 4]!

loadAndEnableClaudeInterface "Load Claude interface with working buttons" | shell | shell := ComponentBuilder recreateClaudeInteractive. ^shell!

loadTemplate: name "Load interface template with automatic command handling" | filename | filename := 'C:\smalltalk\claude\', name, '.gui'. ^self new loadInterface: filename!

recreateClaudeInteractive "Recreate the Claude interactive interface manually" | shell titleLabel inputLabel messageInput sendButton clearButton responseLabel responseArea statusLabel | shell := ShellView new. shell createAt: 100@100 extent: 1024@768. shell text: 'Claude Code Interactive Interface'. shell isResizable: true. titleLabel := StaticText new. titleLabel parentView: shell. titleLabel createAt: 10@10 extent: 1000@25. titleLabel text: 'Claude Code Interactive Session'. inputLabel := StaticText new. inputLabel parentView: shell. inputLabel createAt: 10@45 extent: 120@20. inputLabel text: 'Your Message:'. messageInput := MultilineTextEdit new. messageInput parentView: shell. messageInput createAt: 10@70 extent: 1000@150. messageInput text: 'Type your message to Claude here... Press Enter to send.'. messageInput wordWrap: true. sendButton := PushButton new. sendButton parentView: shell. sendButton createAt: 10@230 extent: 120@35. sendButton text: 'Send Message'. sendButton commandDescription: (CommandDescription command: #sendMessageToClaude description: 'Send message to Claude'). clearButton := PushButton new. clearButton parentView: shell. clearButton createAt: 140@230 extent: 100@35. clearButton text: 'Clear All'. clearButton commandDescription: (CommandDescription command: #clearInterface description: 'Clear interface'). responseLabel := StaticText new. responseLabel parentView: shell. responseLabel createAt: 10@275 extent: 150@20. responseLabel text: 'Claude Response:'. responseArea := MultilineTextEdit new. responseArea parentView: shell. responseArea createAt: 10@300 extent: 1000@400. responseArea text: 'Claude responses will appear here...'. responseArea isReadOnly: true. responseArea wordWrap: true. statusLabel := StaticText new. statusLabel parentView: shell. statusLabel createAt: 10@710 extent: 800@20. statusLabel text: 'Ready - Type your message and press Send or Enter'. shell addSubView: titleLabel name: 'titleLabel'. shell addSubView: inputLabel name: 'inputLabel'. shell addSubView: messageInput name: 'messageInput'. shell addSubView: sendButton name: 'sendButton'. shell addSubView: clearButton name: 'clearButton'. shell addSubView: responseLabel name: 'responseLabel'. shell addSubView: responseArea name: 'responseArea'. shell addSubView: statusLabel name: 'statusLabel'. messageInput when: #keyPressed send: #onMessageInputKeyPressed: to: shell. shell show. ClaudeCodeTcpServer addShellView: shell. ^shell!

saveCurrentAsTemplate: templateName "Save current shell view as a template" | currentShells currentShell spec | currentShells := ClaudeCodeTcpServer currentShellView. currentShells size = 0 ifTrue: [^'No shell view to save']. currentShell := currentShells last. spec := self new extractSpec: currentShell. self saveTemplate: templateName spec: spec. ^'Template saved as: ', templateName!

saveTemplate: name spec: interfaceSpec "Save interface specification as reusable template" | filename | filename := 'C:\smalltalk\claude\', name, '.gui'. self new saveInterface: interfaceSpec toFile: filename. ^filename! !

!ComponentBuilder class categoriesForMethods!
createClaudeMessageTemplate!public! !
createDataEntryTemplate!public! !
listTemplates!public! !
loadAndEnableClaudeInterface!public! !
loadTemplate:!public! !
recreateClaudeInteractive!public! !
saveCurrentAsTemplate:!public! !
saveTemplate:spec:!public! !
!

ClaudeCodeError guid: (GUID fromString: '{d4e5f6a7-b8c9-0123-def0-456789012345}')!

ClaudeCodeError comment: 'ClaudeCodeError represents errors that occur during Claude Code CLI communication.

This error is raised when:
- Claude Code process fails to start
- Communication with Claude Code is interrupted
- Claude Code returns an error response
- Process timeout occurs

Example usage:
	[client sendPrompt: ''some prompt'']
		on: ClaudeCodeError
		do: [:error | 
			Transcript show: ''Claude Code error: '', error messageText
		].
'!

!ClaudeCodeError categoriesForClass!Claude Integration-Exceptions! !

AIWorkspace guid: (GUID fromString: '{1dfa10b6-ce36-4436-af13-1f4f9da5ffdb}')!

AIWorkspace comment: ''!

!AIWorkspace categoriesForClass!Unclassified! !

!AIWorkspace methodsFor!

accept "Send AI workspace text to Claude Code via TCP" | text response | text := self text. text isEmpty ifTrue: [^self]. response := ClaudeCodeClient new executeClaudeWithPrompt: text. self text: text, String cr, String cr, response!

queryCommand: aCommandQuery "Override to handle accept command for Claude Code integration" | selector | selector := aCommandQuery commandSymbol. selector == #accept ifTrue: [aCommandQuery isEnabled: self hasText. ^true]. ^super queryCommand: aCommandQuery! !

!AIWorkspace categoriesForMethods!
accept!commands!public! !
queryCommand:!commands!public! !
!

!AIWorkspace class methodsFor!

quotesDemo
  | s1 s2 |
  s1 := String with: (Character value: 34) with: Character space with: (Character value: 34).
  s2 := String with: (Character value: 39) with: Character space with: (Character value: 39).
  ^ Array with: s1 with: s2.! !

!AIWorkspace class categoriesForMethods!
quotesDemo!public!testing! !
!

"Binary Globals"!

