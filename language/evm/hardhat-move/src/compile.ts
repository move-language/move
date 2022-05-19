import * as Fs from "fs";
import * as Path from "path";
import { Result, ok, err } from "neverthrow";
import { MoveBuildError, ChainedError } from "./types"
import { executeChildProcess } from "./executable"
import { readTextFile, resultify, readDir } from "./util"
import { Artifacts as ArtifactsImpl } from "hardhat/internal/artifacts";
import { Artifact, Artifacts, HardhatConfig } from "hardhat/types";

// Utilities to List Move packages in the Contracts Directory
async function isMovePackage(path: Fs.PathLike): Promise<boolean> {
    // TODO: Result-based error handling
    let stats: Fs.Stats = await Fs.promises.stat(path);

    if (stats.isDirectory()) {
        let manifestPath = Path.join(path.toString(), "Move.toml");
        let manifestStats: Fs.Stats = await Fs.promises.stat(manifestPath);

        return manifestStats.isFile();
    }

    return false;
}

export async function listMovePackages(contractsPath: Fs.PathLike): Promise<Array<String>> {
    // TODO: Result-based error handling
    let dirs: String[] = await Fs.promises.readdir(contractsPath);

    let promises: Promise<String | null>[] = dirs.map((name, idx_, _arr) => {
        let path = Path.join(contractsPath.toString(), name.toString());
        return isMovePackage(path).then(isMove => isMove ? path : null)
    });

    return (await Promise.all(promises)).filter((path): path is String => path !== null)
}

async function movePackageBuild(arch: string, movePath: string, packagePath: string): Promise<Result<void, MoveBuildError>> {
    let cmd = `${movePath} package build --path ${packagePath} --arch ${arch}`;

    let [e, stdout, stderr] = await executeChildProcess(cmd);
    if (e !== null) {
        return err(new MoveBuildError(e, stdout, stderr));
    }

    return ok(undefined);
}

// Artifact Generation
async function loadAbi(packagePath: string, contractName: string): Promise<Result<any, ChainedError>> {
    let abiPath = Path.join(packagePath, "build", "evm", `${contractName}.abi.json`);

    let readFileRes = await readTextFile(abiPath);
    if (readFileRes.isErr()) {
        return err(new ChainedError(`Failed to load ABI from ${abiPath}`, readFileRes.error));
    }
    let content = readFileRes.value;

    let jsonParseRes = resultify(() => JSON.parse(content));
    if (jsonParseRes.isErr()) {
        return err(new ChainedError(`Failed to decode ABI -- invalid JSON: ${content}`, jsonParseRes.error));
    }
    return ok(jsonParseRes.value);
}

async function loadBytecode(packagePath: string, contrantName: string): Promise<Result<string, ChainedError>> {
    let bytecodePath = Path.join(packagePath, "build", "evm", `${contrantName}.bin`);

    let readFileRes = await readTextFile(bytecodePath);
    if (readFileRes.isErr()) {
        return err(new ChainedError(`Failed to load bytecode from ${bytecodePath}`, readFileRes.error));
    }

    return ok(readFileRes.value);
}

async function listCompiledContracts(packagePath: string): Promise<Result<string[], ChainedError>> {
    let path = Path.join(packagePath, "build", "evm");

    let readDirRes = await readDir(path);
    if (readDirRes.isErr()) {
        return err(new ChainedError(`Failed to list compiled contracts in ${path}`, readDirRes.error));
    }
    let entries = readDirRes.value;

    let info = [];
    for (let entry of entries) {
        if (entry.isFile()) {
            // REVIEW: can this throw?
            let parsed = Path.parse(entry.name);

            if (parsed.ext == ".bin") {
                info.push(parsed.name);
            }
        }
    }
    return ok(info);
}

async function generateArtifact(hardhatRootPath: string, packagePath: string, contractName: string): Promise<Result<Artifact, ChainedError>> {
    let [loadbytecodeRes, loadAbiRes] = await Promise.all(
        [loadBytecode(packagePath, contractName), loadAbi(packagePath, contractName)]);

    if (loadbytecodeRes.isErr()) {
        return err(loadbytecodeRes.error);
    }

    if (loadAbiRes.isErr()) {
        return err(loadAbiRes.error);
    }

    let bytecode = loadbytecodeRes.value;
    if (!bytecode.startsWith("0x")) {
        bytecode = "0x" + bytecode;
    }
    let abi = loadAbiRes.value;

    let sourcePath = Path.relative(hardhatRootPath, packagePath);

    let artifact: Artifact = {
        "_format": "hh-move-artifact-1",
        "contractName": contractName,
        "sourceName": sourcePath,
        "abi": abi,
        "bytecode": bytecode,
        "deployedBytecode": bytecode,
        "linkReferences": {},
        "deployedLinkReferences": {}
    };

    return ok(artifact);
}

async function generateArtifactsForPackage(hardhatRootPath: string, packagePath: string): Promise<Result<Artifact[], ChainedError>> {
    let listRes = await listCompiledContracts(packagePath);
    if (listRes.isErr()) {
        return err(new ChainedError(`Failed to list compiled contracts in ${packagePath}`, listRes.error));
    }
    let contractNames = listRes.value;

    let genResults = await Promise.all(
        contractNames.map(contractName => generateArtifact(hardhatRootPath, packagePath, contractName)));

    let errors = [];
    let artifacts = [];
    for (let res of genResults) {
        if (res.isErr()) {
            errors.push(res.error);
        }
        else {
            artifacts.push(res.value);
        }
    }

    if (errors.length > 0) {
        return err(new ChainedError(`Failed to generate artifacts for ${packagePath}`, errors));
    }

    return ok(artifacts);
}

async function buildPackage(arch: string, movePath: string, packagePath: string): Promise<Result<number, MoveBuildError | ChainedError>> {
    let buildRes = await movePackageBuild(arch, movePath, packagePath);
    if (buildRes.isErr()) {
        let e = buildRes.error;
        console.log(`\nFailed to build ${packagePath}\n${e.stdout}${e.stderr}`);
        return err(e);
    }

    console.log(`Successfully built ${packagePath}`);
    return ok(0);
}

async function generateArtifacts(hardhatRootPath: string, packagePath: string): Promise<Result<Artifact[], MoveBuildError | ChainedError>> {
    let genArtifactsRes = await generateArtifactsForPackage(hardhatRootPath, packagePath);
    if (genArtifactsRes.isErr()) {
        let e = genArtifactsRes.error;
        console.log(`Failed to build ${packagePath}\n${e}`);
        return err(genArtifactsRes.error);
    }

    return ok(genArtifactsRes.value);
}

export async function compile(
    arch: string,
    movePath: string,
    artifacts: Artifacts,
    config: HardhatConfig
) {
    let packagePaths: String[] = await listMovePackages(Path.join(config.paths.root, "sources"));
    packagePaths = [...packagePaths, config.paths.root];

    if (packagePaths.length == 0) {
        console.log("No Move contracts to compile");
        return;
    }

    let plural = packagePaths.length == 1 ? "" : "s";
    console.log("Building %d Move package%s...", packagePaths.length, plural);

    let buildResults = await Promise.all(
        packagePaths.map(path => buildPackage(arch, movePath, path.toString())));

    let genArtifactResults = await Promise.all(
        packagePaths.map(path => generateArtifacts(config.paths.root, path.toString()))); 

    let failedToBuildAll = false;
    console.assert(packagePaths.length == buildResults.length);
    for (let idx in packagePaths) {

        let packagePathRel = Path.relative(config.paths.root, packagePaths[idx].toString());
        let res = genArtifactResults[idx];

        if (res.isOk()) {
            let contractNames = [];
            for (let artifact of res.value) {
                contractNames.push(artifact.contractName);
                // TODO: error handling
                await artifacts.saveArtifactAndDebugFile(artifact);
            }

            // TODO: write this in a better way
            const artifactsImpl = artifacts as ArtifactsImpl;
            artifactsImpl.addValidArtifacts([{ sourceName: packagePathRel, artifacts: contractNames }]);
        }
        else {
            failedToBuildAll = true;
        }
    }

    if (failedToBuildAll) {
        // TODO: terminate gracefully
        throw new Error("Failed to build one or more Move packages");
    }
}
