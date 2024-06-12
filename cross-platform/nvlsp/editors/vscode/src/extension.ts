// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path  from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

    const serverOptions: ServerOptions = 
    {
        command: context.asAbsolutePath(path.join('bin', 'nvlsp')),
        args: [],
        transport: TransportKind.stdio,
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'geckscript' }],
    };

    client = new LanguageClient(
        'geckscript-lsp',
        'geckscript language server',
        serverOptions,
        clientOptions,
    );

    client.start();
}

// This method is called when your extension is deactivated
export function deactivate() {
    if (client === undefined) {
        return undefined;
    }
    
    return client.stop();
}
