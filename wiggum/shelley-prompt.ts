#!/usr/bin/env bun
// shelley-prompt - CLI tool for running single Shelley conversations
// Usage: ./shelley-prompt.ts [flags] <prompt>
//        echo 'prompt' | ./shelley-prompt.ts [flags]

interface StreamResponse {
  messages: APIMessage[];
  conversation: { conversation_id: string; slug: string };
  conversation_state?: { conversation_id: string; working: boolean; model?: string };
}

interface APIMessage {
  message_id: string;
  conversation_id: string;
  sequence_id: number;
  type: string;
  llm_data?: string;
  created_at: string;
  end_of_turn?: boolean;
}

interface LLMMessage {
  Role: number;
  Content: LLMContent[];
  EndOfTurn: boolean;
}

interface LLMContent {
  Type: number; // 2 = text, 3 = tool_use, 6 = tool_result
  Text?: string;
  ToolName?: string;
  ToolInput?: unknown;
  ID?: string;
  ToolUseID?: string;
  ToolError?: boolean;
  ToolResult?: LLMContent[];
}

const ContentTypeText = 2;
const ContentTypeToolUse = 3;
const ContentTypeToolResult = 6;

// Parse command line arguments
function parseArgs(): {
  serverURL: string;
  model: string;
  cwd: string;
  userID: string;
  verbose: boolean;
  prompt: string;
} {
  const args = process.argv.slice(2);
  let serverURL = "http://localhost:9999";
  let model = "";
  let cwd = "";
  let userID = "cli";
  let verbose = false;
  const positional: string[] = [];

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "-server" || arg === "--server") {
      serverURL = args[++i] || serverURL;
    } else if (arg === "-model" || arg === "--model") {
      model = args[++i] || "";
    } else if (arg === "-cwd" || arg === "--cwd") {
      cwd = args[++i] || "";
    } else if (arg === "-user" || arg === "--user") {
      userID = args[++i] || "cli";
    } else if (arg === "-v" || arg === "--verbose") {
      verbose = true;
    } else if (arg === "-h" || arg === "--help") {
      console.log(`shelley-prompt - Run single-task Shelley conversations from the command line

USAGE
  shelley-prompt [flags] <prompt>
  echo 'prompt' | shelley-prompt [flags]
  shelley-prompt [flags] < prompt.txt

FLAGS
  -server <url>   Shelley server URL (default: http://localhost:9999)
  -model <model>  Model to use (default: server default)
  -cwd <path>     Working directory for the conversation
  -user <id>      User ID header value (default: cli)
  -v, --verbose   Verbose output (show tool calls and results)
  -h, --help      Show this help

OUTPUT
  stdout  Agent's text responses
  stderr  Tool calls, tool results (verbose mode), and errors

  Use -v to see what tools the agent is calling and their results.
  Redirect stderr to hide verbose output: ./shelley-prompt.ts -v "prompt" 2>/dev/null

MODELS
  List available models:
    curl -s -H "X-Exedev-Userid: cli" http://localhost:9999/api/models | jq '.[].id'

  Common models:
    claude-sonnet-4.5   Fast, capable, good for most tasks
    claude-opus-4.5     Most capable, slower, best for complex tasks
    claude-haiku-4.5    Fastest, cheapest, good for simple tasks

EXAMPLES
  Simple question:
    shelley-prompt "what is 2+2?"

  Specify model:
    shelley-prompt -model claude-sonnet-4.5 "explain quicksort briefly"

  File operations in specific directory:
    shelley-prompt -cwd /tmp "create hello.txt with 'hello world'"

  Pipe prompt from file:
    cat task.txt | shelley-prompt -model claude-opus-4.5

  Verbose mode to see tool execution:
    shelley-prompt -v "list files in current directory"

  Use in scripts (capture output):
    result=$(shelley-prompt "what is the capital of France? just the name")
    echo "The answer is: $result"

  Chain with other commands:
    shelley-prompt "generate 5 random words, one per line" | sort

BEHAVIOR
  - Creates a new conversation for each invocation
  - Streams output as it arrives (not buffered)
  - Exits with code 0 on success, 1 on error
  - The agent has access to tools: bash, file editing, browser, etc.
  - Conversations are saved in Shelley's database for later review

TIPS FOR EFFECTIVE PROMPTS
  - Be specific: "create file.txt containing 'hello'" not "make a file"
  - Specify output format: "answer with just the number" or "respond in JSON"
  - For complex tasks, break into steps or use -v to monitor progress
  - Use -cwd to set working directory for file operations`);
      process.exit(0);
    } else if (!arg.startsWith("-")) {
      positional.push(arg);
    }
  }

  let prompt = positional.join(" ");
  return { serverURL, model, cwd, userID, verbose, prompt };
}

async function readStdin(): Promise<string> {
  const chunks: Buffer[] = [];
  for await (const chunk of Bun.stdin.stream()) {
    chunks.push(Buffer.from(chunk));
  }
  return Buffer.concat(chunks).toString().trim();
}

async function createConversation(
  serverURL: string,
  userID: string,
  message: string,
  model: string,
  cwd: string
): Promise<string> {
  const body: Record<string, string> = { message };
  if (model) body.model = model;
  if (cwd) body.cwd = cwd;

  const resp = await fetch(`${serverURL}/api/conversations/new`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "X-Exedev-Userid": userID,
      "X-Shelley-Request": "1",
    },
    body: JSON.stringify(body),
  });

  if (resp.status !== 201) {
    const text = await resp.text();
    throw new Error(`Server returned ${resp.status}: ${text}`);
  }

  const result = (await resp.json()) as { conversation_id: string };
  return result.conversation_id;
}

function printMessage(msg: APIMessage, verbose: boolean): void {
  if (!msg.llm_data) return;

  let llmMsg: LLMMessage;
  try {
    llmMsg = JSON.parse(msg.llm_data);
  } catch {
    return;
  }

  switch (msg.type) {
    case "agent":
      for (const content of llmMsg.Content) {
        if (content.Type === ContentTypeText && content.Text) {
          process.stdout.write(content.Text);
        }
        if (content.ToolName && verbose) {
          process.stdout.write(`\n[TOOL: ${content.ToolName}]\n`);
          if (content.ToolInput) {
            const inputStr = typeof content.ToolInput === 'string' 
              ? content.ToolInput 
              : JSON.stringify(content.ToolInput, null, 2);
            process.stdout.write(`${inputStr}\n`);
          }
        }
      }
      break;

    case "error":
      for (const content of llmMsg.Content) {
        if (content.Type === ContentTypeText && content.Text) {
          process.stderr.write(`[ERROR: ${content.Text}]\n`);
        }
      }
      // Exit immediately on error messages
      process.exit(1);

    case "user":
      // Tool results come back as user messages with tool_result content
      if (verbose) {
        for (const content of llmMsg.Content) {
          if (content.Type === ContentTypeToolResult) {
            if (content.ToolError) {
              process.stdout.write("[TOOL ERROR]\n");
            }
            for (const result of content.ToolResult || []) {
              if (result.Text) {
                process.stdout.write(`[RESULT: ${result.Text}]\n`);
              }
            }
          }
        }
      }
      break;
  }
}

async function streamConversation(
  serverURL: string,
  userID: string,
  convID: string,
  verbose: boolean
): Promise<void> {
  const resp = await fetch(`${serverURL}/api/conversation/${convID}/stream`, {
    headers: {
      "X-Exedev-Userid": userID,
      Accept: "text/event-stream",
    },
  });

  if (!resp.ok) {
    const text = await resp.text();
    throw new Error(`Server returned ${resp.status}: ${text}`);
  }

  const reader = resp.body?.getReader();
  if (!reader) throw new Error("No response body");

  const decoder = new TextDecoder();
  const seenMessages = new Set<string>();
  let buffer = "";

  let sawEndOfTurn = false;
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });

    // Process complete lines
    const lines = buffer.split("\n");
    buffer = lines.pop() || ""; // Keep incomplete line in buffer

    for (const line of lines) {
      if (!line.startsWith("data: ")) continue;

      const data = line.slice(6);
      let streamResp: StreamResponse;
      try {
        streamResp = JSON.parse(data);
      } catch {
        continue;
      }

      if (!streamResp.messages) continue;

      for (const msg of streamResp.messages) {
        if (seenMessages.has(msg.message_id)) continue;
        seenMessages.add(msg.message_id);

        printMessage(msg, verbose);

        // Check if this message marks end of turn
        if (msg.end_of_turn === true) {
          sawEndOfTurn = true;
          console.log(); // Final newline
          await reader.cancel();
          process.exit(0);
        }
      }
    }
  }

  // Stream ended without end_of_turn - this is an error (EOF, connection lost, etc.)
  if (!sawEndOfTurn) {
    process.stderr.write("[ERROR: LLM request failed: EOF]\n");
    process.exit(1);
  }
}

async function main() {
  const opts = parseArgs();

  // Read from stdin if no prompt provided
  if (!opts.prompt) {
    opts.prompt = await readStdin();
  }

  if (!opts.prompt) {
    console.error(`Usage: shelley-prompt [flags] <prompt>
       echo 'prompt' | shelley-prompt [flags]

Flags:
  -server <url>   Shelley server URL (default: http://localhost:9999)
  -model <model>  Model to use (default: server default)
  -cwd <path>     Working directory for the conversation
  -user <id>      User ID header value (default: cli)
  -v, --verbose   Verbose output (show tool calls and results)
  -h, --help      Show this help

To list available models:
  curl -s -H "X-Exedev-Userid: cli" http://localhost:9999/api/models | jq '.[].id'`);
    process.exit(1);
  }

  try {
    const convID = await createConversation(
      opts.serverURL,
      opts.userID,
      opts.prompt,
      opts.model,
      opts.cwd
    );

    await streamConversation(opts.serverURL, opts.userID, convID, opts.verbose);
  } catch (err) {
    console.error(`Error: ${err instanceof Error ? err.message : err}`);
    process.exit(1);
  }
}

main();
