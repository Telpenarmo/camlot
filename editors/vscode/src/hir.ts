import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient';
import { Ctx } from './ctx';

const scheme = 'camlot-analyzer-hir';

// Opens the virtual file that will show the HIR
//
// The contents of the file come from the `TextDocumentContentProvider`
export function viewHIR(ctx: Ctx) {
    const stcp = new HirContentProvider(ctx);

    ctx.pushCleanup(
        vscode.workspace.registerTextDocumentContentProvider(
            scheme,
            stcp,
        ),
    );

    vscode.workspace.onDidChangeTextDocument(
        (event: vscode.TextDocumentChangeEvent) => {
            const doc = event.document;
            if (doc.languageId !== 'camlot') {
                return;
            };
            afterLs(() => { stcp.eventEmitter.fire(stcp.uri) });
        },
        ctx.subscriptions,
    );

    vscode.window.onDidChangeActiveTextEditor(
        (editor: vscode.TextEditor | undefined) => {
            if (!editor || editor.document.languageId !== 'camlot') {
                return;
            }
            stcp.eventEmitter.fire(stcp.uri);
        },
        ctx.subscriptions,
    );

    return async () => {
        const document = await vscode.workspace.openTextDocument(stcp.uri);

        stcp.eventEmitter.fire(stcp.uri);

        return vscode.window.showTextDocument(
            document,
            vscode.ViewColumn.Two,
            true,
        );
    };
}

// We need to order this after LS updates, but there's no API for that.
// Hence, good old setTimeout.
function afterLs(f: () => unknown) {
    setTimeout(f, 10);
}

interface HirParams {
    text_document: lc.TextDocumentIdentifier;
}

export class HirContentProvider
    implements vscode.TextDocumentContentProvider {
    ctx: Ctx;
    uri = vscode.Uri.parse(scheme + '://hir/hir.cml');
    eventEmitter = new vscode.EventEmitter<vscode.Uri>();

    constructor(ctx: Ctx) {
        this.ctx = ctx;
    }

    provideTextDocumentContent(_uri: vscode.Uri): vscode.ProviderResult<string> {
        const editor = vscode.window.activeTextEditor;
        if (editor === undefined) {
            return '';
        }

        const request: HirParams = {
            text_document: { uri: editor.document.uri.toString() },
        };
        return this.ctx.client.sendRequest<string>(
            'camlot-analyzer/hir',
            request,
        );
    }

    get onDidChange(): vscode.Event<vscode.Uri> {
        return this.eventEmitter.event;
    }
}
