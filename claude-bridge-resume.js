#!/usr/bin/env node
// claude-bridge-resume.js
// Usage examples:
//   node claude-bridge-resume.js "what is 2+2?"
//   echo "next question" | node claude-bridge-resume.js

const { spawn } = require('child_process');
const readline = require('readline');

let SESSION_ID = process.env.CLAUDE_SESSION_ID || null; // optional seed
const BASE_ARGS = [
  '--output-format', 'stream-json',
  '--verbose',
  '--dangerously-skip-permissions' // YOLO; remove if you want prompts
];

function runClaudeTurn(prompt, { cwd = process.cwd() } = {}) {
  return new Promise((resolve, reject) => {
    const args = [...BASE_ARGS];
    if (SESSION_ID) args.push('--resume', SESSION_ID); else args.push('--continue');

    const child = spawn('claude', args, { stdio: ['pipe', 'pipe', 'pipe'], cwd });

    let finalResult = null;

    // NDJSON line decoder
    let buf = '';
    function handle(data) {
      buf += data.toString('utf8');
      let i;
      while ((i = buf.indexOf('\n')) >= 0) {
        const line = buf.slice(0, i).trim();
        buf = buf.slice(i + 1);
        if (!line) continue;
        try {
          const evt = JSON.parse(line);

          // Grab session id from init once
          if (evt.type === 'system' && evt.subtype === 'init' && evt.session_id && !SESSION_ID) {
            SESSION_ID = evt.session_id;
            // Store session ID silently - no output to stderr
          }

          // Look for result event first
          if (evt.type === 'result') {
            finalResult = evt.result ?? null;
            if (finalResult && !finalResult.includes('###END###')) {
              finalResult = finalResult + '\n###END###';
            }
          }
          
          // Also look for assistant message with text content as backup
          if (evt.type === 'assistant' && evt.message && evt.message.content) {
            for (const item of evt.message.content) {
              if (item.type === 'text' && item.text && !finalResult) {
                finalResult = item.text;
                if (finalResult && !finalResult.includes('###END###')) {
                  finalResult = finalResult + '\n###END###';
                }
                break;
              }
            }
          }

          // Output JSON so we can see what's happening
          process.stdout.write(line + '\n'); // forward to stdout for debugging
        } catch (e) {
          // If a line isn't JSON, forward it for debugging
          process.stdout.write(line + '\n');
        }
      }
    }

    child.stdout.on('data', handle);
    child.stderr.on('data', d => process.stderr.write(d));
    
    // Send prompt via stdin in interactive mode
    child.stdin.write(prompt + '\n');
    child.stdin.end();

    child.on('error', reject);
    child.on('close', (code) => {
      // Even if killed by timeout (143), still output any result we captured
      // Debug: log what we captured
      process.stderr.write(`[bridge] finalResult: ${finalResult}\n`);
      // Output the final result with ###END### to stdout for Dolphin
      if (finalResult) {
        process.stdout.write(finalResult);
      } else {
        process.stdout.write('ERROR: No result captured (exit code: ' + code + ')\n');
      }
      resolve({ sessionId: SESSION_ID, result: finalResult });
    });

    // Longer timeout for interactive mode with tool execution
    const timeoutMs = +(process.env.CLAUDE_TIMEOUT_MS || 300000); // 5 minutes
    setTimeout(() => { try { child.kill('SIGTERM'); } catch {} }, timeoutMs).unref();
  });
}

// CLI: prompt from argv or stdin
(async () => {
  const argPrompt = process.argv.slice(2).join(' ').trim();
  if (argPrompt) {
    await runClaudeTurn(argPrompt);
    return;
  }
  const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  let collected = '';
  for await (const line of rl) collected += line + '\n';
  if (collected.trim()) await runClaudeTurn(collected.trim());
})();