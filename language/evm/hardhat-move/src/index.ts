import { Artifacts, HardhatConfig } from "hardhat/types";
import { extendConfig, subtask, types } from "hardhat/config";
import { TASK_COMPILE_GET_COMPILATION_TASKS } from "hardhat/builtin-tasks/task-names";
import {
    TASK_COMPILE_MOVE,
    TASK_COMPILE_MOVE_GET_BUILD,
    TASK_COMPILE_MOVE_RUN_BINARY,
} from "./task-names";
import { compile } from "./compile"
import { MoveBuild } from "./types";
import { DEFAULT_MOVE_ARCH } from "./constants"
import { locateMoveExecutablePath } from "./executable"

extendConfig((config, userConfig) => {
    const defaultConfig = userConfig.move ?? { arch: DEFAULT_MOVE_ARCH };
    config.move = { ...defaultConfig, ...config.move };
});

// This adds a new subtask "compile:move" which is added to the queue when one runs
// `npx hardhat compile`. This task will build all the move contracts using the `move`
// executable and generate the artifacts hardhat requires for testing and deployment.
subtask(TASK_COMPILE_GET_COMPILATION_TASKS,
    async (_, __, runSuper): Promise<string[]> => {
        const otherTasks = await runSuper();
        return [...otherTasks, TASK_COMPILE_MOVE];
    });

subtask(TASK_COMPILE_MOVE)
    .addParam("quiet", undefined, undefined, types.boolean)
    .setAction(async ({ quiet }: { quiet: boolean }, { artifacts, config, run }) => {
        const arch = config.move.arch;
        const compilerPath = config.move.compilerPath;

        let movePath: string;
        if (compilerPath) {
            movePath = compilerPath;
            console.log("using local move executable:", movePath);
        } else {
            const moveBuild: MoveBuild = await run(TASK_COMPILE_MOVE_GET_BUILD);
            movePath = moveBuild.compilerPath;
            console.log("using auto find move executable:", movePath);
        }

        await run(
            TASK_COMPILE_MOVE_RUN_BINARY,
            {
                arch: arch,
                movePath: movePath,
                artifacts: artifacts,
                config: config
            }
        );
    });

subtask(TASK_COMPILE_MOVE_RUN_BINARY)
    .addParam("arch", undefined, undefined, types.string)
    .addParam("movePath", undefined, undefined, types.string)
    .setAction(async ({
        arch,
        movePath,
        artifacts,
        config
    }: {
        arch: string,
        movePath: string,
        artifacts: Artifacts,
        config: HardhatConfig
    }) => {
        await compile(arch, movePath, artifacts, config);
    });

subtask(TASK_COMPILE_MOVE_GET_BUILD)
    .setAction(async (): Promise<MoveBuild> => {
        let arch = DEFAULT_MOVE_ARCH;

        let locateRes = await locateMoveExecutablePath();
        if (locateRes.isErr()) {
            console.log("Failed to locate the `move` executable.");
            console.log(locateRes.error);
            let compilerPath = ""
            return { compilerPath, arch };
        }
        let compilerPath = locateRes.value;

        return { compilerPath, arch };
    });

module.exports = {};
