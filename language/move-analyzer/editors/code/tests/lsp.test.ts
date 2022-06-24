import * as assert from 'assert';
import * as Mocha from 'mocha';
import * as path from 'path';
import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient';

import { sleep } from './utils';

Mocha.suite('LSP', () => {
    Mocha.test('textDocument/documentSymbol', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate();
        await sleep(3000);

        // 1. get workdir
        const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

        // 2. open doc
        const docs = await vscode.workspace.openTextDocument(path.join(workDir, 'sources/M1.move'));
        await vscode.window.showTextDocument(docs);
        await sleep(1000);

        // 3. execute command
        const params: lc.DocumentSymbolParams = {
            textDocument: {
                uri: docs.uri.toString(),
            },
        };

        const syms: Array<lc.SymbolInformation> | Array<lc.DocumentSymbol> | null | undefined = await
            vscode.commands.executeCommand(
                'move-analyzer.textDocumentDocumentSymbol', params,
            );

        assert.ok(syms);
        assert.deepStrictEqual(syms[0]?.kind, lc.SymbolKind.Module);
        assert.deepStrictEqual(syms[0].name, 'M1');
    });
});
