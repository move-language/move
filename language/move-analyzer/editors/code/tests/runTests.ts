// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

/**
 * This file contains what VS Code's documentation refers to as "the test script," which downloads,
 * unzips, launches a VS Code instance with our extension installed, and executes the "test runner."
 * For more information, see:
 * https://code.visualstudio.com/api/working-with-extensions/testing-extension#the-test-script
 */

import * as path from 'path';
import * as cp from 'child_process';
import {
    runTests,
    downloadAndUnzipVSCode,
    resolveCliArgsFromVSCodeExecutablePath,
} from '@vscode/test-electron';

/**
 * Launches a VS Code instance to run tests.
 *
 * This is essentially a TypeScript program that executes the "VS Code Tokenizer Tests" launch
 * target defined in this repository's `.vscode/launch.json`.
 */
async function main(): Promise<void> {
    try {
        // The `--extensionDevelopmentPath` argument passed to VS Code. This should point to the
        // directory that contains the extension manifest file, `package.json`.
        const extensionDevelopmentPath = path.resolve(__dirname, '..', '..');

        // The `--extensionTestsPath` argument passed to VS Code. This should point to a JavaScript
        // program that is considered to be the "test suite" for the extension.
        const extensionTestsPath = path.resolve(__dirname, 'index.js');

        // The workspace
        let testWorkspacePath = path.resolve(__dirname, './lsp-demo/lsp-demo.code-workspace');
        if (process.platform === 'win32') {
            testWorkspacePath = path.resolve(__dirname, './lsp-demo/lsp-demo-win.code-workspace');
        }

        // Install vscode and depends extension
        const vscodeVersion = '1.64.0';
        const vscodeExecutablePath = await downloadAndUnzipVSCode(vscodeVersion);
        const [cli, ...args] = resolveCliArgsFromVSCodeExecutablePath(vscodeExecutablePath);
        const newCli = cli ?? 'code';
        cp.spawnSync(newCli, [...args, '--install-extension', 'damirka.move-syntax', '--force'], {
            encoding: 'utf-8',
            stdio: 'inherit',
        });

        // Download VS Code, unzip it, and run the "test suite" program.
        await runTests({
            vscodeExecutablePath: vscodeExecutablePath,
            extensionDevelopmentPath,
            extensionTestsPath,
            launchArgs: [testWorkspacePath],
        });
    } catch (_err: unknown) {
        console.error('Failed to run tests');
        process.exit(1);
    }
}

void main();
