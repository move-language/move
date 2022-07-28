import type {
    DocumentSymbolParams,
    SymbolInformation,
    DocumentSymbol,
} from 'vscode-languageclient';
import { DocumentSymbolRequest } from 'vscode-languageclient';
import type { Context } from '../context';

/**
 * An LSP command textDocument/documentSymbol
 */
export async function textDocumentDocumentSymbol(
    context: Readonly<Context>,
    params: DocumentSymbolParams,
    )
    : Promise<SymbolInformation[] | DocumentSymbol[] | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    // Send the request to the language client.
    return client.sendRequest(DocumentSymbolRequest.type, params);
}
