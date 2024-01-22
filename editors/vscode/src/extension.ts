// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

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
}
