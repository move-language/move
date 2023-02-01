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
    const cwd = args[0] as string;
    const name = args[1] as string;
    const sui_test = terminalManager.alloc(cwd + "/" + "sui.test_ui", () => {
      return vscode.window.createTerminal({
        cwd: cwd,
        name: "sui test",
      });
    });
    sui_test.show(true);
    sui_test.sendText("sui move test " + name, true);
    sui_test.show(false);
  });
  context.registerCommand("sui.create_project", async function (_, ..._args) {
    const dir = await vscode.window.showSaveDialog();
    if (dir === undefined) {
      await vscode.window.showErrorMessage("Please input a directory");
      return
    }
    const dir2 = dir.fsPath;
    // looks like vscode will check directory exists 
    // no need check for myself.
    // if (fs.existsSync(dir2)) {
    //   await vscode.window.showErrorMessage("File or directory already exists.");
    //   return
    // }
    fs.mkdirSync(dir2);
    const project_name = path.parse(dir2).base;
    const replace_name = `my_first_package`;
    fs.writeFileSync(dir2 + "/Move.toml", sui_move_toml_template.replace(replace_name, project_name));
    fs.mkdirSync(dir2 + "/sources");
    fs.writeFileSync(dir2 + "/sources/my_module.move", sui_module_file_template.replace(replace_name, project_name));
  });

  context.registerCommand("sui.move.new", async (_, ..._args) => {
    const name = await vscode.window.showInputBox({
      title: "New a project",
      placeHolder: "Type you project name."
    });
    if (name === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.move.new", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui move new",
      });
    });
    t.show(true);
    t.sendText("sui move new " + name, true);
  });
  context.registerCommand("sui.move.build", (_, ..._args) => {
    const t = terminalManager.alloc("sui.move.build", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui move build",
      });
    });
    t.show(true);
    t.sendText("sui move build", true);
  });
  context.registerCommand("sui.move.coverage", (_, ..._args) => {
    const t = terminalManager.alloc("sui.move.coverage", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui move coverage",
      });
    });
    t.show(true);
    t.sendText("sui move test --coverage", true);
    t.sendText("sui move coverage summary", true);
  });
  context.registerCommand("sui.move.test", (_, ..._args) => {
    const t = terminalManager.alloc("sui.move.test", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui move test",
      });
    });
    t.show(true);
    t.sendText("sui move test", true);
  });
  context.registerCommand("sui.move.prove", (_, ..._args) => {
    const t = terminalManager.alloc("sui.move.prove", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui move prove",
      });
    });
    t.show(true);
    t.sendText("sui move prove", true);
  });
  context.registerCommand("sui.client.active.address", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.active.address", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client active address",
      });
    });
    t.show(true);
    t.sendText("sui client active-address", true);
  });
  context.registerCommand("sui.client.active.env", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.active.env", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client active env",
      });
    });
    t.show(true);
    t.sendText("sui client active-env", true);
  });
  context.registerCommand("sui.client.addresses", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.addresses", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client addresses",
      });
    });
    t.show(true);
    t.sendText("sui client addresses", true);
  });
  context.registerCommand("sui.client.envs", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.envs", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client envs",
      });
    });
    t.show(true);
    t.sendText("sui client envs", true);
  });
  context.registerCommand("sui.client.gas", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.gas", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client gas",
      });
    });
    t.show(true);
    t.sendText("sui client gas", true);
  });
  context.registerCommand("sui.client.object", async (_, ..._args) => {
    const objectID = await vscode.window.showInputBox({
      placeHolder: "Type you object ID."
    });
    if (objectID === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.object", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client object",
      });
    });
    t.show(true);
    t.sendText("sui client object " + objectID, true);
  });
  context.registerCommand("sui.client.objects", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.objects", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client objects",
      });
    });
    t.show(true);
    t.sendText("sui client objects", true);
  });
  context.registerCommand("sui.client.publish", async (_, ..._args) => {
    const budget = await vscode.window.showInputBox({
      placeHolder: "Type you Gas Budget."
    });
    if (budget === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.publish", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client publish",
      });
    });
    t.show(true);
    t.sendText("sui client publish --gas-budget " + budget, true);
  });
  context.registerCommand("sui.client.new.address", async (_, ..._args) => {
    const schema = await vscode.window.showQuickPick(schemaTypes, { canPickMany: false, placeHolder: "Select you schema." });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.new.address", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui client new address",
      });
    });
    t.show(true);
    t.sendText("sui client new-address " + schema, true);
  });

  context.registerCommand("sui.keytool.generate", async (_, ..._args) => {
    const schema = await vscode.window.showQuickPick(schemaTypes, { canPickMany: false, placeHolder: "Select you schema." });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.keytool.generate", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui keytool generate",
      });
    });
    t.show(true);
    t.sendText("sui keytool generate " + schema, true);
  });

  context.registerCommand("sui.keytool.import", async (_, ..._args) => {
    const m = await vscode.window.showInputBox({
      placeHolder: "Type your mnemonic phrase."
    });
    if (m === undefined) {
      return;
    }
    const schema = await vscode.window.showQuickPick(schemaTypes, { canPickMany: false, placeHolder: "Select you schema." });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.keytool.import", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui keytool import",
      });
    });
    t.show(true);
    t.sendText("sui keytool import " + m + " " + schema, true);
  });
  context.registerCommand("sui.keytool.list", (_, ..._args) => {
    const t = terminalManager.alloc("sui.client.keytool.list", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui keytool list",
      });
    });
    t.show(true);
    t.sendText("sui keytool list ", true);
  });
  context.registerCommand("sui.keytool.load.keypair", async (_, ..._args) => {
    const file = await vscode.window.showOpenDialog({
      canSelectFiles: false,
    });
    if (file === undefined) {
      return;
    }
    if (file.length == 0) {
      return;
    }
    if (file[0] !== undefined) {
      const t = terminalManager.alloc("sui.client.keytool.load.keypair", (): vscode.Terminal => {
        return vscode.window.createTerminal({
          name: "sui keytool load keypair",
        });
      });
      t.show(true);
      t.sendText("sui keytool load-keypair " + file[0].fsPath, true);
    }
  });
  context.registerCommand("sui.keytool.show", async (_, ..._args) => {
    const file = await vscode.window.showOpenDialog({
      canSelectFiles: false,
    });
    if (file === undefined) {
      return;
    }
    if (file.length == 0) {
      return;
    }
    if (file[0] !== undefined) {
      const t = terminalManager.alloc("sui.client.keytool.show", (): vscode.Terminal => {
        return vscode.window.createTerminal({
          name: "sui keytool show",
        });
      });
      t.show(true);
      t.sendText("sui keytool show " + file[0].fsPath, true);
    }
  });
  context.registerCommand("sui.keytool.unpack", async (_, ..._args) => {
    const str = await vscode.window.showInputBox({
      placeHolder: "Type your ????"
    });
    if (str === undefined) {
      return;
    }
    const t = terminalManager.alloc("sui.client.keytool.unpack", (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: "sui keytool unpack",
      });
    });
    t.show(true);
    t.sendText("sui keytool unpack '" + str + "'", true);
  });

}


const schemaTypes = ["ed25519", "secp256k1", "secp256r1"];



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





class TerminalManager {
  all: Map<string, vscode.Terminal | undefined>;
  constructor() {
    this.all = new Map();
  }

  alloc(typ: string, new_fun: () => vscode.Terminal): vscode.Terminal {
    const x = this.all.get(typ);
    if (x === undefined || x.exitStatus !== undefined) {
      const x = new_fun();
      this.all.set(typ, x);
      return x;
    } else {
      return x;
    }


  }
}

const terminalManager = new TerminalManager();  