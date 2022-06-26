// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import type { Configuration } from './configuration';
import * as fs from 'fs';
import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient';
import { log } from './log';

/**
 * The observer for ready event.
 */
export class OnReady {
    private readonly _ready: Promise<void>;

    private _resolve: (() => void) | undefined;

    private _reject: ((reason: string) => void) | undefined;

    constructor() {
        this._ready = new Promise<void>((resolve, reject) => {
            this._resolve = resolve;
            this._reject = reject;
        });
    }

    public async wait(): Promise<void> {
        return this._ready;
    }

    public resolve(): void {
        if (this._resolve !== undefined) {
            this._resolve();
        }
    }

    public reject(reason: string): void {
        if (this._reject !== undefined) {
            this._reject(reason);
        }
    }

} // OnReady

/** Information passed along to each VS Code command defined by this extension. */
export class Context {
    private _client: lc.LanguageClient | undefined;

    private _symbolicatorReady: boolean;

    private readonly _onSymbolicatorReadyCallbacks: Array<OnReady>;

    private constructor(
        private readonly extensionContext: Readonly<vscode.ExtensionContext>,
        readonly configuration: Readonly<Configuration>,
        client: lc.LanguageClient | undefined = undefined,
    ) {
        this._client = client;
        this._symbolicatorReady = false;
        this._onSymbolicatorReadyCallbacks = [];
    }

    static create(
        extensionContext: Readonly<vscode.ExtensionContext>,
        configuration: Readonly<Configuration>,
    ): Context | Error {
        if (!fs.existsSync(configuration.serverPath)) {
            return new Error(
                `language server executable '${configuration.serverPath}' could not be found, so ` +
                'most extension features will be unavailable to you. Follow the instructions in ' +
                'the move-analyzer Visual Studio Code extension README to install the language ' +
                'server.',
            );
        }
        return new Context(extensionContext, configuration);
    }

    /**
     * Registers the given command with VS Code.
     *
     * "Registering" the function means that the VS Code machinery will execute it when the command
     * with the given name is requested by the user. The command names themselves are specified in
     * this extension's `package.json` file, under the key `"contributes.commands"`.
     */
    registerCommand(
        name: Readonly<string>,
        command: (context: Readonly<Context>, ...args: Array<any>) => any,
    ): void {
        const disposable = vscode.commands.registerCommand(`move-analyzer.${name}`, async (...args: Array<any>)
            : Promise<any> => {
            const ret = await command(this, ...args);
            return ret;
        });

        this.extensionContext.subscriptions.push(disposable);
    }

    /**
     * Configures and starts the client that interacts with the language server.
     *
     * The "client" is an object that sends messages to the language server, which in Move's case is
     * the `move-analyzer` executable. Unlike registered extension commands such as
     * `move-analyzer.serverVersion`, which are manually executed by a VS Code user via the command
     * palette or menu, this client sends many of its messages on its own (for example, when it
     * starts, it sends the "initialize" request).
     *
     * To read more about the messages sent and responses received by this client, such as
     * "initialize," read [the Language Server Protocol specification](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize).
     **/
    startClient(): void {
        const executable: lc.Executable = {
            command: this.configuration.serverPath,
        };
        const serverOptions: lc.ServerOptions = {
            run: executable,
            debug: executable,
        };

        // The vscode-languageclient module reads a configuration option named
        // "<extension-name>.trace.server" to determine whether to log messages. If a trace output
        // channel is specified, these messages are printed there, otherwise they appear in the
        // output channel that it automatically created by the `LanguageClient` (in this extension,
        // that is 'Move Language Server'). For more information, see:
        // https://code.visualstudio.com/api/language-extensions/language-server-extension-guide#logging-support-for-language-server
        const traceOutputChannel = vscode.window.createOutputChannel(
            'Move Analyzer Language Server Trace',
        );
        const clientOptions: lc.LanguageClientOptions = {
            documentSelector: [{ scheme: 'file', language: 'move' }],
            traceOutputChannel,
        };

        const client = new lc.LanguageClient(
            'move-analyzer',
            'Move Language Server',
            serverOptions,
            clientOptions,
        );
        log.info('Starting client...');
        const disposable = client.start();
        this.extensionContext.subscriptions.push(disposable);
        this._client = client;

        this._client.onReady().then(() => {
            log.info('Client ready.');

            client.onNotification('telemetry/event', (...params: Array<any>) => {
                log.info('Event:' + JSON.stringify(params));

                /* eslint @typescript-eslint/no-unsafe-member-access: off */
                if (params[0] !== undefined && params[0].event_type === 'SymbolicatorEvent') {
                    if (params[0].event_data.result === 'success') {
                        this._resoleSymbolicatorCallbacks();
                    } else {
                        this._rejectSymbolicatorCallbacks(params[0].event_data.result);
                    }
                }
            });
        }, (err) => {
            log.info(`Client failed to start: ${err}`);
        });
    }

    /**
     * Returns the client that this extension interacts with.
     *
     * @returns lc.LanguageClient
     */
    getClient(): lc.LanguageClient | undefined {
        return this._client;
    }

    /**
     * Returns whether the language server's symbolicator is ready to accept requests.
     *
     * @returns Promise<void>
     *
     */
    async onSymbolicatorReady(): Promise<void> {
        if (this._symbolicatorReady) {
            return Promise.resolve();
        }

        const onReady = new OnReady();
        this._onSymbolicatorReadyCallbacks.push(onReady);
        return onReady.wait();
    }

    _resoleSymbolicatorCallbacks(): void {
        this._symbolicatorReady = true;
        this._onSymbolicatorReadyCallbacks.forEach(callback => callback.resolve());
    }

    _rejectSymbolicatorCallbacks(reason: string): void {
        this._onSymbolicatorReadyCallbacks.forEach(callback => callback.reject(reason));
    }
} // Context
