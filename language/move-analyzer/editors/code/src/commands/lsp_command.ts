
import * as lc from 'vscode-languageclient';
import type { Context } from '../context';

/**
 * An extension command that displays the version of the server that this extension
 * interfaces with.
 */
export async function textDocumentCompletion(context: Readonly<Context>, params: lc.CompletionParams)
    : Promise<lc.CompletionList | Array<lc.CompletionItem> | null> {
    const client = context.getClient();
    if (client === undefined) {
        return Promise.reject(new Error('No language client connected.'));
    }

    return client.sendRequest(lc.CompletionRequest.type, params);
}
