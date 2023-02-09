// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

import { Configuration } from './configuration';
import { Context } from './context';
import { Extension } from './extension';
import { log } from './log';

import * as childProcess from 'child_process';
import * as vscode from 'vscode';
import * as commands from './commands';
import * as fs from 'fs';
import * as path from 'path';


/**
 * An extension command that displays the version of the server that this extension
 * interfaces with.
 */
async function serverVersion(context: Readonly<Context>): Promise<void> {
  const version = childProcess.spawnSync(
    context.configuration.serverPath,
    ['--version'],
    { encoding: 'utf8' },
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
    }
    return x;


  }
}

const terminalManager = new TerminalManager();
const schemaTypes = ['ed25519', 'secp256k1', 'secp256r1'];
const sui_move_toml_template
  = `[package]
name = "my_first_package"
version = "0.0.1"

[dependencies]
Sui = { git = "https://github.com/MystenLabs/sui.git", subdir = "crates/sui-framework", rev = "devnet" }

[addresses]
my_first_package =  "0x0"
sui =  "0x2"
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
    public entry fun sword_create(forge: &mut Forge, magic: u64, strength: u64, recipient: address, 
                                  ctx: &mut TxContext) {
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

class TraverseDirItem {
  path: string;

  is_file: boolean;

  constructor(path: string,
    is_file: boolean) {
    this.path = path;
    this.is_file = is_file;
  }
}

function traverseDir(dir: any, call_back: (path: TraverseDirItem) => void): void {
  fs.readdirSync(dir).forEach(file => {
    const fullPath = path.join(dir, file);
    if (fs.lstatSync(fullPath).isDirectory()) {
      call_back(new TraverseDirItem(fullPath, false));
      traverseDir(fullPath, call_back);
    } else {
      call_back(new TraverseDirItem(fullPath, true));
    }
  });
}

function workSpaceDir(): string | undefined {
  if (vscode.workspace.workspaceFolders !== undefined) {
    if (vscode.workspace.workspaceFolders[0] !== undefined) {
      const f = vscode.workspace.workspaceFolders[0].uri.fsPath;
      return f;
    }
  }
  return undefined;
}

function get_all_move_toml_dirs(): string[] {
  const working_dir = workSpaceDir();
  if (working_dir === undefined) {
    return [];
  }
  const ret: string[] = [];
  traverseDir(working_dir, (item) => {
    if (item.is_file && item.path.endsWith('Move.toml')) {
      ret.push(item.path);
    }
  });
  return ret;
}

function discovery_sui_working_dir(): string | undefined {
  const working_dir = workSpaceDir();
  if (working_dir === undefined) {
    return undefined;
  }
  const x = get_all_move_toml_dirs();
  if (x.length === 1) {
    return working_dir;
  }
  return undefined;
}

let sui_working_dir: string | undefined = discovery_sui_working_dir();


async function get_use_input_sui_working_dir(): Promise<string | undefined> {
  return vscode.window.showQuickPick(get_all_move_toml_dirs(),
    {
    }).then((x): string | undefined => {
      if (x === undefined) {
        return undefined;
      }
      sui_working_dir = path.parse(x).dir;
      return sui_working_dir;
    });
}

async function get_sui_working_dir(): Promise<string | undefined> {
  if (sui_working_dir !== undefined) {
    return sui_working_dir;
  }
  return get_use_input_sui_working_dir();
}


export async function activate(
  extensionContext: Readonly<vscode.ExtensionContext>,
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
      `Could not activate move-analyzer: ${context.message}.`,
    );
    return;
  }
  if (sui_working_dir !== undefined) {
    void vscode.window.showInformationMessage('sui working directory set to ' + sui_working_dir);
  }

  // Register handlers for VS Code commands that the user explicitly issues.
  context.registerCommand('serverVersion', serverVersion);

  // Configure other language features.
  context.configureLanguage();

  // All other utilities provided by this extension occur via the language server.
  await context.startClient();
  context.registerCommand(
    'textDocumentDocumentSymbol',
    commands.textDocumentDocumentSymbol,
  );
  context.registerCommand('textDocumentHover', commands.textDocumentHover);
  context.registerCommand(
    'textDocumentCompletion',
    commands.textDocumentCompletion,
  );
  // / a test ui button at move file.
  context.registerCommand('sui.test_ui', (_, ...args) => {
    const cwd = args[0] as string;
    const name = args[1] as string;
    const sui_test = terminalManager.alloc(cwd + 'sui.test_ui', () => {
      return vscode.window.createTerminal({
        cwd: cwd,
        name: 'sui test',
      });
    });
    sui_test.show(true);
    sui_test.sendText('sui move test ' + name, true);
    sui_test.show(false);
  });
  context.registerCommand('sui.create_project', async () => {
    let w = workSpaceDir();
    if (w === undefined) {
      w = '.';
    }
    const dir = await vscode.window.showSaveDialog({
      defaultUri: vscode.Uri.parse(w),
    });
    if (dir === undefined) {
      void vscode.window.showErrorMessage('Please input a directory');
      return;
    }
    const dir2 = dir.fsPath;
    fs.mkdirSync(dir2);
    const project_name = path.parse(dir2).base;
    const replace_name = 'my_first_package';
    fs.writeFileSync(dir2 + '/Move.toml', sui_move_toml_template.toString().replaceAll(replace_name, project_name));
    fs.mkdirSync(dir2 + '/sources');
    fs.writeFileSync(dir2 + '/sources/my_module.move', sui_module_file_template.replaceAll(replace_name, project_name));
    const client = context.getClient();
    if (undefined !== client && dir2.startsWith(w)) {
      void client.sendRequest('move/load_project',
        {
          filepath: dir,
        });
    }

  });

  context.registerCommand('sui.move.new', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const name = await vscode.window.showInputBox({
      title: 'New a project',
      placeHolder: 'Type you project name.',
    });
    if (name === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.move.new', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui move new',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui move new ' + name, true);
  });
  context.registerCommand('sui.move.build', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.move.build', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui move build',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui move build', true);
  });
  context.registerCommand('sui.move.coverage', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.move.coverage', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui move coverage',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui move test --coverage', true);
    t.sendText('sui move coverage summary', true);
  });
  context.registerCommand('sui.move.test', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.move.test', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui move test',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true); t.sendText('cd ' + working_dir, true);
    t.sendText('sui move test', true);
  });
  context.registerCommand('sui.move.prove', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.move.prove', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui move prove',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui move prove', true);
  });
  context.registerCommand('sui.client.active.address', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.active.address', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client active address',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client active-address', true);
  });
  context.registerCommand('sui.client.active.env', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.active.env', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client active env',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client active-env', true);
  });
  context.registerCommand('sui.client.addresses', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.addresses', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client addresses',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client addresses', true);
  });
  context.registerCommand('sui.client.envs', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.envs', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client envs',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client envs', true);
  });
  context.registerCommand('sui.client.gas', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.gas', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client gas',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client gas', true);
  });
  context.registerCommand('sui.client.object', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const objectID = await vscode.window.showInputBox({
      placeHolder: 'Type you object ID.',
    });
    if (objectID === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.object', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client object',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client object ' + objectID, true);
  });
  context.registerCommand('sui.client.objects', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.objects', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client objects',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client objects', true);
  });
  context.registerCommand('sui.client.publish', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const budget = await vscode.window.showInputBox({
      placeHolder: 'Type you Gas Budget.',
    });
    if (budget === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.publish', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client publish',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client publish --gas-budget ' + budget, true);
  });
  context.registerCommand('sui.client.new.address', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const schema = await vscode.window.showQuickPick(schemaTypes, {
      canPickMany: false, placeHolder: 'Select you schema.',
    });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.new.address', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui client new address',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui client new-address ' + schema, true);
  });

  context.registerCommand('sui.keytool.generate', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const schema = await vscode.window.showQuickPick(schemaTypes, {
      canPickMany: false, placeHolder: 'Select you schema.',
    });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.keytool.generate', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui keytool generate',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui keytool generate ' + schema, true);
  });

  context.registerCommand('sui.keytool.import', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const m = await vscode.window.showInputBox({
      placeHolder: 'Type your mnemonic phrase.',
    });
    if (m === undefined) {
      return;
    }
    const schema = await vscode.window.showQuickPick(schemaTypes, {
      canPickMany: false, placeHolder: 'Select you schema.',
    });
    if (schema === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.keytool.import', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui keytool import',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui keytool import ' + m + ' ' + schema, true);
  });
  context.registerCommand('sui.keytool.list', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.keytool.list', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui keytool list',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui keytool list ', true);
  });
  context.registerCommand('sui.keytool.load.keypair', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const file = await vscode.window.showOpenDialog({
      canSelectFiles: false,
    });
    if (file === undefined) {
      return;
    }
    if (file.length === 0) {
      return;
    }
    if (file[0] !== undefined) {
      const t = terminalManager.alloc('sui.client.keytool.load.keypair', (): vscode.Terminal => {
        return vscode.window.createTerminal({
          name: 'sui keytool load keypair',
        });
      });
      t.show(true);
      t.sendText('cd ' + working_dir, true);
      t.sendText('sui keytool load-keypair ' + file[0].fsPath, true);
    }
  });
  context.registerCommand('sui.keytool.show', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const file = await vscode.window.showOpenDialog({
      canSelectFiles: false,
    });
    if (file === undefined) {
      return;
    }
    if (file.length === 0) {
      return;
    }
    if (file[0] !== undefined) {
      const t = terminalManager.alloc('sui.client.keytool.show', (): vscode.Terminal => {
        return vscode.window.createTerminal({
          name: 'sui keytool show',
        });
      });
      t.show(true);
      t.sendText('cd ' + working_dir, true);
      t.sendText('sui keytool show ' + file[0].fsPath, true);
    }
  });
  context.registerCommand('sui.keytool.unpack', async () => {
    const working_dir = await get_sui_working_dir();
    if (working_dir === undefined) {
      return;
    }
    const str = await vscode.window.showInputBox({
      placeHolder: 'Type your ????',
    });
    if (str === undefined) {
      return;
    }
    const t = terminalManager.alloc('sui.client.keytool.unpack', (): vscode.Terminal => {
      return vscode.window.createTerminal({
        name: 'sui keytool unpack',
      });
    });
    t.show(true);
    t.sendText('cd ' + working_dir, true);
    t.sendText('sui keytool unpack \'' + str + '\'', true);
  });

  context.registerCommand('sui.reset.working.space', async () => {
    const new_ = await get_use_input_sui_working_dir();
    if (new_ === undefined) {
      return;
    }
    sui_working_dir = new_;
    void vscode.window.showInformationMessage('sui working directory set to ' + new_);
  });
}
