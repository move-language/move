import * as assert from 'assert';
import * as Mocha from 'mocha';
import * as path from 'path';
import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient';
import type { MarkupContent } from 'vscode-languageclient';

Mocha.suite('LSP', () => {
    Mocha.test('textDocument/documentSymbol', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate(); // Synchronous waiting for activation to complete

        // 1. get workdir
        const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

        // 2. open doc
        const docs = await vscode.workspace.openTextDocument(path.join(workDir, 'sources/M1.move'));
        await vscode.window.showTextDocument(docs);

        // 3. execute command
        const params: lc.DocumentSymbolParams = {
            textDocument: {
                uri: docs.uri.toString(),
            },
        };

        const syms: Array<lc.DocumentSymbol> | undefined = await
            vscode.commands.executeCommand(
                'move-analyzer.textDocumentDocumentSymbol', params,
            );

        assert.ok(syms);
        assert.deepStrictEqual(syms[0]?.kind, lc.SymbolKind.Module);
        assert.deepStrictEqual(syms[0].name, 'M1');

        assert.ok(syms[0].children);
        assert.deepStrictEqual(syms[0]?.children[0]?.kind, lc.SymbolKind.Constant);
        assert.deepStrictEqual(syms[0]?.children[0].name, 'SOME_CONST');
        assert.deepStrictEqual(syms[0]?.children[1]?.kind, lc.SymbolKind.Struct);
        assert.deepStrictEqual(syms[0]?.children[1].name, 'SomeOtherStruct');
        assert.ok(syms[0].children[1].children);
        assert.deepStrictEqual(syms[0]?.children[1]?.children[0]?.kind, lc.SymbolKind.Field);
        assert.deepStrictEqual(syms[0]?.children[1]?.children[0]?.name, 'some_field');
        assert.deepStrictEqual(syms[0]?.children[1].name, 'SomeOtherStruct');
        assert.deepStrictEqual(syms[0]?.children[2]?.kind, lc.SymbolKind.Function);
        assert.deepStrictEqual(syms[0]?.children[2].name, 'some_other_struct');
        assert.deepStrictEqual(syms[0]?.children[3]?.kind, lc.SymbolKind.Function);
        assert.deepStrictEqual(syms[0]?.children[3].name, 'this_is_a_test');
        assert.deepStrictEqual(syms[0]?.children[3]?.detail, '["test", "expected_failure"]');
    });

    Mocha.test('textDocument/hover for definition in the same module', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate(); // Synchronous waiting for activation to complete

        // 1. get workdir
        const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

        // 2. open doc
        const docs = await vscode.workspace.openTextDocument(
            path.join(workDir, 'sources/M6.move'),
        );
        await vscode.window.showTextDocument(docs);

        // 3. execute command
        const params: lc.HoverParams = {
            textDocument: {
                uri: docs.uri.toString(),
            },
            position: {
                line: 26,
                character: 8,
            },
        };

        const hoverResult: lc.Hover | undefined =
            await vscode.commands.executeCommand(
                'move-analyzer.textDocumentHover',
                params,
            );

        assert.ok(hoverResult);
        assert.deepStrictEqual((hoverResult.contents as MarkupContent).value,
            // eslint-disable-next-line max-len
            'fun Symbols::M6::other_doc_struct(): Symbols::M7::OtherDocStruct\n\n\nThis is a multiline docstring\n\nThis docstring has empty lines.\n\nIt uses the ** format instead of ///\n\n');

    });

    Mocha.test('textDocument/hover for definition in an external module', async () => {
        const ext = vscode.extensions.getExtension('move.move-analyzer');
        assert.ok(ext);

        await ext.activate(); // Synchronous waiting for activation to complete

        // 1. get workdir
        const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

        // 2. open doc
        const docs = await vscode.workspace.openTextDocument(
            path.join(workDir, 'sources/M6.move'),
        );
        await vscode.window.showTextDocument(docs);

        // 3. execute command
        const params: lc.HoverParams = {
            textDocument: {
                uri: docs.uri.toString(),
            },
            position: {
                line: 26,
                character: 41,
            },
        };

        const hoverResult: lc.Hover | undefined =
            await vscode.commands.executeCommand(
                'move-analyzer.textDocumentHover',
                params,
            );


        assert.ok(hoverResult);
        assert.deepStrictEqual((hoverResult.contents as MarkupContent).value,
            'Symbols::M7::OtherDocStruct\n\nDocumented struct in another module\n');

    });
});
