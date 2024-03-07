// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import * as assert from 'assert';
import * as Mocha from 'mocha';
import * as path from 'path';
import * as vscode from 'vscode';
import type * as lc from 'vscode-languageclient';

Mocha.suite('textDocument/signatureHelp', () => {
  Mocha.test('Cursor position is out of bounds', async () => {
    const ext = vscode.extensions.getExtension('move.move-analyzer');
    assert.ok(ext);

    await ext.activate(); // Synchronous waiting for activation to complete

    // 1. get workdir
    const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

    // 2. open doc
    const docs = await vscode.workspace.openTextDocument(
      path.join(workDir, 'sources/Completions.move'),
    );
    await vscode.window.showTextDocument(docs);

    // 3. execute command
    const leftOfFnBoundsParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 6,
      },
    };

    const signatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      leftOfFnBoundsParams,
    );
    assert.ok(signatureHelp);

    // We expect an empty signature help response because the cursor is
    // to the left of the open parenthesis of the expected function scope
    assert.strictEqual(signatureHelp.activeParameter, undefined);
    assert.strictEqual(signatureHelp.activeSignature, undefined);
    assert.strictEqual(signatureHelp.signatures.length, 0);

    // Similarly, we expect an empty signature help response if the cursor
    // is to the right of the closing parenthesis of the expected function scope
    const rightOfFnBoundsParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 45,
      },
    };

    const signatureHelp2 = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      rightOfFnBoundsParams,
    );
    assert.ok(signatureHelp2);

    assert.strictEqual(signatureHelp2.activeParameter, undefined);
    assert.strictEqual(signatureHelp2.activeSignature, undefined);
    assert.strictEqual(signatureHelp2.signatures.length, 0);
  });

  Mocha.test('Comma positions', async () => {
    const ext = vscode.extensions.getExtension('move.move-analyzer');
    assert.ok(ext);

    await ext.activate(); // Synchronous waiting for activation to complete

    // 1. get workdir
    const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

    // 2. open doc
    const docs = await vscode.workspace.openTextDocument(
      path.join(workDir, 'sources/Completions.move'),
    );
    await vscode.window.showTextDocument(docs);

    // 3. execute command
    const atCommaParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 10,
      },
    };

    const atCommaSignatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      atCommaParams,
    );
    assert.ok(atCommaSignatureHelp);

    assert.strictEqual(atCommaSignatureHelp.activeParameter, 1);
    assert.ok(atCommaSignatureHelp.signatures[0]);
    assert.strictEqual(atCommaSignatureHelp.signatures[0].label, 'fun Symbols::Completions::add(a: u64, b: u64): u64');
    assert.deepStrictEqual(atCommaSignatureHelp.signatures[0].parameters, [{ label: [30, 36] }, { label: [38, 44] }]);

    const preCommaParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 9,
      },
    };

    const preCommaSignatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      preCommaParams,
    );
    assert.ok(preCommaSignatureHelp);

    assert.strictEqual(preCommaSignatureHelp.activeParameter, 0);
    assert.ok(preCommaSignatureHelp.signatures[0]);
    assert.strictEqual(preCommaSignatureHelp.signatures[0].label, 'fun Symbols::Completions::add(a: u64, b: u64): u64');
    assert.deepStrictEqual(preCommaSignatureHelp.signatures[0].parameters, [{ label: [30, 36] }, { label: [38, 44] }]);
  });

  Mocha.test('Nested functions', async () => {
    const ext = vscode.extensions.getExtension('move.move-analyzer');
    assert.ok(ext);

    await ext.activate(); // Synchronous waiting for activation to complete

    // 1. get workdir
    const workDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '';

    // 2. open doc
    const docs = await vscode.workspace.openTextDocument(
      path.join(workDir, 'sources/Completions.move'),
    );
    await vscode.window.showTextDocument(docs);

    // 3. execute command
    const oneLvlFnParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 20,
      },
    };

    // For example, the subtract call in add(a, subtract(..., ...)) is 1 level deep
    const oneLvlFnSignatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      oneLvlFnParams,
    );
    assert.ok(oneLvlFnSignatureHelp);

    assert.strictEqual(oneLvlFnSignatureHelp.activeParameter, 0);
    assert.ok(oneLvlFnSignatureHelp.signatures[0]);
    assert.strictEqual(
      oneLvlFnSignatureHelp.signatures[0].label,
      'fun Symbols::Completions::subtract(a: u64, b: u64): u64',
    );
    assert.deepStrictEqual(oneLvlFnSignatureHelp.signatures[0].parameters, [{ label: [35, 41] }, { label: [43, 49] }]);

    const twoLvlFnParams: lc.SignatureHelpParams = {
      textDocument: {
        uri: docs.uri.toString(),
      },
      position: {
        line: 14,
        character: 31,
      },
    };

    // For example, the divide call in add(a, subtract(b, divide(..., ...))) is 2 levels deep
    const twoLvlFnSignatureHelp = await vscode.commands.executeCommand<vscode.SignatureHelp>(
      'move-analyzer.textDocumentSignatureHelp',
      twoLvlFnParams,
    );
    assert.ok(twoLvlFnSignatureHelp);

    assert.strictEqual(twoLvlFnSignatureHelp.activeParameter, 0);
    assert.ok(twoLvlFnSignatureHelp.signatures[0]);
    assert.strictEqual(
      twoLvlFnSignatureHelp.signatures[0].label,
      'fun Symbols::Completions::divide(a: u64, b: u64): u64',
    );
    assert.deepStrictEqual(twoLvlFnSignatureHelp.signatures[0].parameters, [{ label: [33, 39] }, { label: [41, 47] }]);
  });
});
