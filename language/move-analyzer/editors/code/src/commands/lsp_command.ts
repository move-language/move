import type {
    DocumentSymbolParams,
    SymbolInformation,
    DocumentSymbol,
    CompletionParams,
    CompletionList,
    CompletionItem,
    Location,
    LocationLink,
    DefinitionParams,
} from 'vscode-languageclient';

import { DocumentSymbolRequest,
    HoverRequest,
    CompletionRequest,
    DefinitionRequest,
} from 'vscode-languageclient';

import type { Context } from '../context';

/**
 * An LSP command textDocument/documentSymbol
 */
export async function textDocumentDocumentSymbol(
    context: Readonly<Context>,
    params: DocumentSymbolParams,
): Promise<SymbolInformation[] | DocumentSymbol[] | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    // Send the request to the language client.
    return client.sendRequest(DocumentSymbolRequest.type, params);
}

/**
 * An LSP command textDocument/completion
 */
export async function textDocumentCompletion(
    context: Readonly<Context>,
    params: CompletionParams,
): Promise<CompletionList | CompletionItem[] | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    // Send the request to the language client.
    return client.sendRequest(CompletionRequest.type, params);
}


/**
 * An LSP command textDocument/hover
 */
export async function textDocumentHover(
    context: Readonly<Context>,
    params: DocumentSymbolParams,
)
    : Promise<SymbolInformation[] | DocumentSymbol[] | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    // Send the request to the language client.
    return client.sendRequest(HoverRequest.method, params);
}


/**
 * An LSP command textDocument/definition
 */
export async function textDocumentDefinition(
    context: Readonly<Context>,
    params: DefinitionParams,
)
    : Promise<Location | Location[] | LocationLink[] | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    // Send the request to the language client.
    return client.sendRequest(DefinitionRequest.type, params);
}
