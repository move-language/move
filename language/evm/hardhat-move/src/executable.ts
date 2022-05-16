import { Result, ok, err } from "neverthrow";
import * as ChildProcess from "child_process";

export async function executeChildProcess(cmd: string): Promise<[ChildProcess.ExecException | null, string, string]> {
    return new Promise((resolve, _reject) => {
        // TODO: preserve coloring
        let proc = ChildProcess.exec(cmd, (err, stdout, stderr) => {
            resolve([err, stdout, stderr]);
        });

        proc.stdin!.end();
    });
}

// find local `move` executable
export async function locateMoveExecutablePath(): Promise<Result<string, Error>> {
    let [e, stdout, _stderr] = await executeChildProcess("which move");

    if (e !== null) {
        return err(e);
    }

    console.assert(stdout !== "");
    let lines: string[] = stdout.split(/\r?\n/);
    return ok(lines[0]);
}
