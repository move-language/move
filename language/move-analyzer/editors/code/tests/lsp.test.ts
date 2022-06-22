import * as assert from 'assert';
import * as Mocha from 'mocha';
import * as path from 'path';
import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient';

import { sleep } from './utils';

Mocha.suite('LSP', () => {
    Mocha.test('textDocument/completion', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate();
        await sleep(1000);

        // 1. get workdir
        const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

        // 2. open doc
        const docs = await vscode.workspace.openTextDocument(path.join(workDir, 'sources/M1.move'));
        await vscode.window.showTextDocument(docs);
        await sleep(1000);

        // 3. execute command
        const params: lc.CompletionParams = {
            context: {
                triggerKind: lc.CompletionTriggerKind.TriggerCharacter,
            },
            textDocument: {
                uri: docs.uri.toString(),
            },
            position: lc.Position.create(36, 24),
        };

        const ret: Array<lc.CompletionItem> = await vscode.commands.executeCommand(
            'move-analyzer.textDocumentCompletion', params,
        );

        assert.deepStrictEqual(ret.length > 0, true);
    });

    Mocha.test('textDocument/documentSymbol', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate();
        await sleep(1000);

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

        const syms: Array<lc.SymbolInformation> = await vscode.commands.executeCommand(
            'move-analyzer.textDocumentDocumentSymbol', params,
        );

        console.log('syms: ', syms);

        assert.ok(syms);
        assert.deepStrictEqual(syms[0]?.kind, lc.SymbolKind.Function);
        assert.deepStrictEqual(syms[0]?.name, '0xCAFE::M1');
    });
});
