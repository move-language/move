#!/usr/local/bin/python3

# This is a script to compile Move source code into artifacts that can be used for testing.
# Copy this to the root of your hardhat project to use it.
#
# Note: this is a temporary solution that will be phased out once we implement the Move plugin.
#
# Move code should be stored within the `contracts` directory, along with an ABI file.
#   - contracts
#     - MyContract.move
#     - MyContract.abi.json
#
# The ABI file should look something like this:
# [
#     {
#       "inputs": [],
#       "name": "foo",
#       "outputs": [
#         {
#           "internalType": "uint256",
#           "name": "",
#           "type": "uint256"
#         }
#       ],
#       "stateMutability": "view",
#       "type": "function"
#     }
# ]

dependencies = [
    "../stdlib/sources",
    "../../move-stdlib/sources"
]
named_address_mapping = {
    "Std": "0x1",
    "Evm": "0x2"
}

import os
import shutil
import tempfile
import subprocess
import json
import sys
from os import path


path_root = path.dirname(__file__)
path_contracts = path.join(path_root, "contracts")
path_artifacts = path.join(path_root, "artifacts", "contracts")
path_home = path.expanduser("~")


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


def locate_solc():
    p = path.join(path_home, "bin", "solc")
    if path.isfile(p):
        return p
    p = shutil.which("solc")
    if p is not None:
        return p
    eprint("Failed to locate solc.")
    exit(1)


def locate_move_to_yul():
    p = shutil.which("move-to-yul")
    if p is not None:
        return p
    eprint("Failed to locate move-to-yul -- you can install it by running `cargo install --path <path to the move-to-yul crate>.`")
    exit(1)


path_solc = locate_solc()
path_move_to_yul = locate_move_to_yul()

def list_move_sources():
    paths = []
    for name in os.listdir(path_contracts):
        path_ = path.join(path_contracts, name)
        if path.isfile(path_) and path.splitext(path_)[1] == ".move":
            paths.append(path_)
    return paths


def load_abi(path_source):
    path_abi = path.splitext(path_source)[0] + ".abi.json"
    if not path.isfile(path_abi):
        eprint()
        eprint("Missing ABI definition: {}.".format(path_abi))
        exit(1)
    with open(path_abi, "r") as f:
        text = f.read()
    return json.loads(text)


def move_to_yul(path_source):
    with tempfile.NamedTemporaryFile() as output_file:
        args = [path_move_to_yul, "--output", output_file.name]

        if len(dependencies) > 0:
            args.append("-d")
            args.extend(dependencies)

        if len(named_address_mapping) > 0:
            args.append("-n")
            for (name, addr) in named_address_mapping.items():
                args.append("{}={}".format(name, addr))

        path_abi = path.splitext(path_source)[0] + ".abi.json"
        args.append("--abi-output")
        args.append(path_abi)

        args.extend(["--", path_source])

        move_to_yul_res = subprocess.run(args, capture_output = True)
        if move_to_yul_res.returncode != 0:
            eprint()
            eprint(move_to_yul_res.stderr.decode("utf-8"))
            exit(1)
        return output_file.read()


def solc(path_source, yul_code):
    solc_res = subprocess.run([path_solc, "--optimize", "--strict-assembly", "--bin", "-"], input = yul_code, capture_output = True)
    if solc_res.returncode != 0:
        eprint()
        eprint(solc_res.stderr.decode("utf-8"))
        exit(1)
    output = solc_res.stdout.decode("utf-8")
    return "0x{}".format(output.split("Binary representation:")[1].replace("\n", ""))


def gen_artifact(path_source, abi, bytecode):
    basename = path.basename(path_source)
    contract_name = path.splitext(basename)[0]

    path_artifact = path.join(path_artifacts, basename)
    if not path.isdir(path_artifact):
        if path.exists(path_artifact):
            eprint("Failed to generate artifact. Path {} already exists, but it's not a directory.".format(path_artifact))
            exit(1)
        os.makedirs(path_artifact)

    artifact = {
        "_format": "hh-sol-artifact-1",
        "contractName": contract_name,
        "sourceName": path_source,
        "abi": abi,
        "bytecode": bytecode,
        "deployedBytecode": bytecode,
        "linkReferences": {},
        "deployedLinkReferences": {}
    }

    with open(path.join(path_artifact, contract_name + ".json"), "w") as f:
        json.dump(artifact, f, indent = 4)


def run(path_source):
    print("Compiling {}...".format(path_source))
    yul_code = move_to_yul(path_source)
    abi = load_abi(path_source)
    bytecode = solc(path_source, yul_code)
    gen_artifact(path_source, abi, bytecode)


for path_source in list_move_sources():
    run(path_source)
print("Success.")
