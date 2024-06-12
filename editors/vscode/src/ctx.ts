import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

export interface Ctx {
    pushCleanup(cleanup: vscode.Disposable): void;
    subscriptions: vscode.Disposable[];
    client: LanguageClient;
}
