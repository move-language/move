// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import { Configuration } from "./configuration";
import { Context } from "./context";
import { Extension } from "./extension";
import { log } from "./log";

import * as childProcess from "child_process";
import * as vscode from "vscode";
import * as commands from "./commands";
import * as fs from 'fs';
import * as path from "path";


/**
 * An extension command that displays the version of the server that this extension
 * interfaces with.
 */
async function serverVersion(context: Readonly<Context>): Promise<void> {
  const version = childProcess.spawnSync(
    context.configuration.serverPath,
    ["--version"],
    { encoding: "utf8" }
  );
  if (version.stdout) {
    await vscode.window.showInformationMessage(version.stdout);
  } else if (version.error) {
    await vscode.window.showErrorMessage(
      `Could not execute move-analyzer: ${version.error.message}.`
    );
  } else {
    await vscode.window.showErrorMessage(
      `A problem occurred when executing '${context.configuration.serverPath}'.`
    );
  }
}

/**
 * The entry point to this VS Code extension.
 *
 * As per [the VS Code documentation on activation
 * events](https://code.visualstudio.com/api/references/activation-events), 'an extension must
 * export an `activate()` function from its main module and it will be invoked only once by
 * VS Code when any of the specified activation events [are] emitted."
 *
 * Activation events for this extension are listed in its `package.json` file, under the key
 * `"activationEvents"`.
 *
 * In order to achieve synchronous activation, mark the function as an asynchronous function,
 * so that you can wait for the activation to complete by await
 */

export async function activate(
  extensionContext: Readonly<vscode.ExtensionContext>
): Promise<void> {
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
      `Could not activate move-analyzer: ${context.message}.`
    );
    return;
  }

  // Register handlers for VS Code commands that the user explicitly issues.
  context.registerCommand("serverVersion", serverVersion);

  // Configure other language features.
  context.configureLanguage();

  // All other utilities provided by this extension occur via the language server.
  await context.startClient();
  context.registerCommand(
    "textDocumentDocumentSymbol",
    commands.textDocumentDocumentSymbol
  );
  context.registerCommand("textDocumentHover", commands.textDocumentHover);
  context.registerCommand(
    "textDocumentCompletion",
    commands.textDocumentCompletion
  );
  /// a test ui button at move file.
  context.registerCommand("sui.test_ui", (_, ...args) => {
    const cwd = args[0];
    const name = args[1] as string;
    const sui_test = vscode.window.createTerminal({
      cwd: cwd,
      name: "sui test",
    });
    sui_test.show(true);
    sui_test.sendText("sui move test " + name, true);
    sui_test.sendText("Done");
  });
  context.registerCommand("sui.create_project", async function (_, ..._args) {
    const dir = await vscode.window.showSaveDialog();
    if (dir === undefined) {
      await vscode.window.showErrorMessage("Please input a directory");
      return
    }
    const dir2 = dir.fsPath;
    if (fs.existsSync(dir2)) {
      await vscode.window.showErrorMessage("File or directory already exists.");
      return
    }
    fs.mkdirSync(dir2);
    const project_name = path.parse(dir2).base;
    const replace_name = `my_first_package`;
    fs.writeFileSync(dir2 + "/Move.toml", sui_move_toml_template.replace(replace_name, project_name));
    fs.mkdirSync(dir2 + "/sources");
    fs.writeFileSync(dir2 + "/sources/my_module.move", sui_module_file_template.replace(replace_name, project_name));
  });
}

const sui_move_toml_template = `[package]
name = "my_first_package"
version = "0.0.1"

[dependencies]
Sui = { git = "https://github.com/MystenLabs/sui.git", subdir = "crates/sui-framework", rev = "devnet" }

[addresses]
my_first_package =  "0x0"
sui =  "0000000000000000000000000000000000000002"
`;


const sui_module_file_template = `
// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

module my_first_package::my_module {
    // Part 1: imports
    use sui::object::{Self, UID};
    use sui::transfer;
    use sui::tx_context::{Self, TxContext};

    // Part 2: struct definitions
    struct Sword has key, store {
        id: UID,
        magic: u64,
        strength: u64,
    }

    struct Forge has key {
        id: UID,
        swords_created: u64,
    }

    // Part 3: module initializer to be executed when this module is published
    fun init(ctx: &mut TxContext) {
        let admin = Forge {
            id: object::new(ctx),
            swords_created: 0,
        };
        // transfer the forge object to the module/package publisher
        transfer::transfer(admin, tx_context::sender(ctx));
    }

    // Part 4: accessors required to read the struct attributes
    public fun magic(self: &Sword): u64 {
        self.magic
    }

    public fun strength(self: &Sword): u64 {
        self.strength
    }

    public fun swords_created(self: &Forge): u64 {
        self.swords_created
    }

    // Part 5: entry functions to create and transfer swords
    public entry fun sword_create(forge: &mut Forge, magic: u64, strength: u64, recipient: address, ctx: &mut TxContext) {
        // create a sword
        let sword = Sword {
            id: object::new(ctx),
            magic: magic,
            strength: strength,
        };
        // transfer the sword
        transfer::transfer(sword, recipient);
        forge.swords_created = forge.swords_created + 1;
    }
}
`;
