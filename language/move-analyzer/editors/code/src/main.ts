// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import { Configuration } from './configuration';
import { Context } from './context';
import { Extension } from './extension';
import { log } from './log';
import * as childProcess from 'child_process';
import * as vscode from 'vscode';

/**
 * An extension command that displays the version of the server that this extension
 * interfaces with.
 */
async function serverVersion(context: Readonly<Context>): Promise<void> {
    const version = childProcess.spawnSync(
        context.configuration.serverPath, ['--version'], { encoding: 'utf8' },
    );
    if (version.stdout) {
        await vscode.window.showInformationMessage(version.stdout);
    } else if (version.error) {
        await vscode.window.showErrorMessage(
            `Could not execute move-analyzer: ${version.error.message}.`,
        );
    } else {
        await vscode.window.showErrorMessage(
            `A problem occurred when executing '${context.configuration.serverPath}'.`,
        );
    }
}

/**
 * The entry point to this VS Code extension.
 *
 * As per [the VS Code documentation on activation
 * events](https://code.visualstudio.com/api/references/activation-events), "an extension must
 * export an `activate()` function from its main module and it will be invoked only once by
 * VS Code when any of the specified activation events [are] emitted."
 *
 * Activation events for this extension are listed in its `package.json` file, under the key
 * `"activationEvents"`.
 */
export function activate(extensionContext: Readonly<vscode.ExtensionContext>): void {
    const extension = new Extension();
    log.info(`${extension.identifier} version ${extension.version}`);

    const configuration = new Configuration();
    log.info(`configuration: ${configuration.toString()}`);

    const context = Context.create(extensionContext, configuration);
    // An error here -- for example, if the path to the `move-analyzer` binary that the user
    // specified in their settings is not valid -- prevents the extension from providing any
    // more utility, so return early.
    if (context instanceof Error) {
        void vscode.window.showErrorMessage(
            `Could not activate move-analyzer: ${context.message}.`,
        );
        return;
    }

    // Register handlers for VS Code commands that the user explicitly issues.
    context.registerCommand('serverVersion', serverVersion);

    // All other utilities provided by this extension occur via the language server.
    context.startClient();
}
