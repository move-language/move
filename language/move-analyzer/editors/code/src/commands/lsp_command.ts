
import * as lc from 'vscode-languageclient';
import type { Context } from '../context';

/**
 * An LSP command textDocument/Completion.
 */
export async function textDocumentCompletion(context: Readonly<Context>, params: lc.CompletionParams)
    : Promise<lc.CompletionList | Array<lc.CompletionItem> | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    return client.sendRequest(lc.CompletionRequest.type, params);
}

/**
 * An LSP command textDocument/documentSymbol
 */
export async function textDocumentDocumentSymbol(context: Readonly<Context>, params: lc.DocumentSymbolParams)
    : Promise<Array<lc.SymbolInformation> | Array<lc.DocumentSymbol> | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    return client.sendRequest(lc.DocumentSymbolRequest.type, params);
}
