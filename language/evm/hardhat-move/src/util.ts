import * as Fs from "fs";
import { Result, ok, err } from "neverthrow";

// Functions in the js standard lib uses execeptions for error handling, of which the
// the correctness is hard to reason about. Here are a few wrappers that transform
// them into Result-based APIs for easy error handling and chaining.
export async function resultifyAsync<T>(f: () => Promise<T>): Promise<Result<T, Error>> {
    try {
        return ok(await f());
    }
    catch (e) {
        if (e instanceof Error) {
            return err(e);
        }
        else {
            throw new Error(`${e} is not an instance of Error -- this should not happen`);
        }
    }
}

export function resultify<T>(f: () => T): Result<T, Error> {
    try {
        return ok(f());
    }
    catch (e) {
        if (e instanceof Error) {
            return err(e);
        }
        else {
            throw new Error(`${e} is not an instance of Error -- this should not happen`);
        }
    }
}

export async function readTextFile(path: Fs.PathLike): Promise<Result<string, Error>> {
    return resultifyAsync(() => {
        return Fs.promises.readFile(path, { encoding: "utf-8" });
    });
}


export async function writeTextFile(path: Fs.PathLike, data: string): Promise<Result<void, Error>> {
    return resultifyAsync(() => {
        return Fs.promises.writeFile(path, data);
    });
}

export async function readDir(path: Fs.PathLike): Promise<Result<Fs.Dirent[], Error>> {
    return resultifyAsync(() => {
        return Fs.promises.readdir(path, { withFileTypes: true });
    });
}

export async function createDirIfNotExists(path: Fs.PathLike): Promise<Result<string | undefined, Error>> {
    return resultifyAsync(() => {
        return Fs.promises.mkdir(path, { recursive: true });
    });
}
