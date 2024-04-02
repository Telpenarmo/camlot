// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as lc from "vscode-languageclient/node";

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";
import { syntaxTree } from './syntax_tree';

let client: LanguageClient | undefined;
let commands: vscode.Disposable[] = [];

async function initLanguageClient() {
	const serverOptions: ServerOptions = {
		command: "rideml-server",
		args: ["lsp"],
	};
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "rideml" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{rml,rmli}"),
		},
	};

	return new LanguageClient("rideml-lsp", "Rideml Language Server", serverOptions, clientOptions);
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

	if (!client) {
		try {
			client = await initLanguageClient();
			await client.start();

			let ctx = {
				subscriptions: context.subscriptions,
				client,
				pushCleanup: (disposable: vscode.Disposable) => {
					context.subscriptions.push(disposable);
				},
			};
			// register syntax tree command
			commands.push(vscode.commands.registerCommand('rideml-analyzer.syntaxTree', syntaxTree(ctx)));
		} catch (err: any) {
			vscode.window.showErrorMessage(err);
		}
	}
}

// This method is called when your extension is deactivated
export function deactivate() {
	if (client) {
		client.stop();
	}
	commands.map((c) => c.dispose());
	commands = [];
}
