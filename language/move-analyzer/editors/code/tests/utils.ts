import * as vscode from 'vscode';

export async function sleep(ms: number): Promise<void> {
    return new Promise((resolve) => {
        setTimeout(resolve, ms);
    });
}

export async function taskResult(taskExecution: vscode.TaskExecution): Promise<number | undefined> {
    const disposables = new Array<vscode.Disposable>();
    const disposeAll = function(): void {
        disposables.forEach((d) => d.dispose());
    };

    return new Promise<number | undefined>((resolve) => {
        disposables.push(
            vscode.tasks.onDidEndTask((e) => {
                if (e.execution === taskExecution) {
                    disposeAll();
                    resolve(0);
                }
            }),
        );

        disposables.push(
            vscode.tasks.onDidEndTaskProcess((e) => {
                if (e.execution === taskExecution) {
                    disposeAll();
                    resolve(e.exitCode);
                }
            }),
        );
    });
}
