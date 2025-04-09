// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";
import { Wasm, ProcessOptions } from '@vscode/wasm-wasi/v1';
import { createStdioOptions, startServer, createUriConverters } from '@vscode/wasm-wasi-lsp';

import { Ctx } from './ctx';
import { syntaxTree } from './syntax_tree';
import { viewHIR } from './hir';

let client: LanguageClient | undefined;
let commands: vscode.Disposable[] = [];

async function initLanguageClient(context: vscode.ExtensionContext) {
	const wasm: Wasm = await Wasm.load();

	const channel = vscode.window.createOutputChannel('Camlot WASM Server');
	const serverOptions: ServerOptions = async () => {
		const options: ProcessOptions = {
			stdio: createStdioOptions(),
			args: ["lsp"],
			mountPoints: [
				{ kind: 'workspaceFolder' },
			]
		};
		const filename = vscode.Uri.joinPath(context.extensionUri, 'out', 'camlot-server.wasm');
		const module = await wasm.compile(filename);
		const memory = { initial: 160, maximum: 160, shared: true };
		const process = await wasm.createProcess('lsp-server', module, memory, options);

		const decoder = new TextDecoder('utf-8');
		process.stderr?.onData((data) => {
			channel.append(decoder.decode(data));
		});

		return startServer(process);
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ language: "camlot" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{cml,cmli}"),
		},
		outputChannel: channel,
		uriConverters: createUriConverters(),
	};

	return new LanguageClient("camlot-lsp", "Camlot Language Server", serverOptions, clientOptions);
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

	if (!client) {
		try {
			client = await initLanguageClient(context);
			await client.start();

			const ctx = {
				subscriptions: context.subscriptions,
				client,
				pushCleanup: (disposable: vscode.Disposable) => {
					context.subscriptions.push(disposable);
				},
			};
			// register syntax tree command
			commands.push(vscode.commands.registerCommand('camlot-analyzer.syntaxTree', syntaxTree(ctx)));
			commands.push(vscode.commands.registerCommand('camlot-analyzer.hirTree', viewHIR(ctx)));
			commands.push(vscode.commands.registerCommand('camlot-analyzer.restartServer', restart(ctx)));
		} catch (err: unknown) {
			vscode.window.showErrorMessage(err as string);
		}
	}
}

// This method is called when your extension is deactivated
export function deactivate() {
	if (client) {
		client.stop().catch((e: unknown) => {
			vscode.window.showErrorMessage(e as string);
		});
	}
	commands.map((c) => { c.dispose() });
	commands = [];
}
function restart(ctx: Ctx) {
	return async () => {
		await ctx.client.restart();
	};
}
