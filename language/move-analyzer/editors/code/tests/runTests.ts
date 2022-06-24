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
import { runTests, downloadAndUnzipVSCode, resolveCliArgsFromVSCodeExecutablePath } from '@vscode/test-electron';
import type { TestOptions } from '@vscode/test-electron/out/runTest';

/**
 * Launches a VS Code instance to run tests.
 *
 * This is essentially a TypeScript program that executes the "VS Code Tokenizer Tests" launch
 * target defined in this repository's `.vscode/launch.json`.
 */
async function main(): Promise<void> {
    // The `--extensionDevelopmentPath` argument passed to VS Code. This should point to the
    // directory that contains the extension manifest file, `package.json`.
    const extensionDevelopmentPath = path.resolve(__dirname, '..', '..');

    // The `--extensionTestsPath` argument passed to VS Code. This should point to a JavaScript
    // program that is considered to be the "test suite" for the extension.
    const extensionTestsPath = path.resolve(__dirname, 'index.js');

    // The workspace
    const testWorkspacePath = path.resolve(__dirname, './lsp-demo/lsp-demo.code-workspace');

    // Install vscode and depends extension
    const vscodeVersion = '1.64.0';
    const vscodeExecutablePath = await downloadAndUnzipVSCode(vscodeVersion);
    const [cli, ...args] = resolveCliArgsFromVSCodeExecutablePath(vscodeExecutablePath);
    const newCli = cli ?? 'code';
    cp.spawnSync(newCli, [...args, '--install-extension', 'damirka.move-syntax', '--force'], {
        encoding: 'utf-8',
        stdio: 'inherit',
    });

    // Run vscode tests
    const options: TestOptions = {
        vscodeExecutablePath: vscodeExecutablePath,
        extensionDevelopmentPath,
        extensionTestsPath,
        launchArgs: [testWorkspacePath],
    };

    console.log(`Test options: ${JSON.stringify(options)}`);

    // Download VS Code, unzip it and run the integration test
    try {
        await runTests(options);
    } catch (err: unknown) {
        console.error(err);

        // Note
        // no-process-exit is required to avoid the test runner to exit with error code 1
        // eslint-disable-next-line no-process-exit
        process.exit(1);
    }
}

void main();
