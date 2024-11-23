// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

import { Ctx } from './ctx';
import { syntaxTree } from './syntax_tree';
import { viewHIR } from './hir';

let client: LanguageClient | undefined;
let commands: vscode.Disposable[] = [];

function initLanguageClient() {
	const serverOptions: ServerOptions = {
		command: "camlot-server",
		args: ["lsp"],
	};
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ language: "camlot" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{cml,cmli}"),
		},
	};

	return new LanguageClient("camlot-lsp", "Camlot Language Server", serverOptions, clientOptions);
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

	if (!client) {
		try {
			client = initLanguageClient();
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
