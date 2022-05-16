import * as ChildProcess from "child_process";

export interface MoveBuild {
  arch: string;
  compilerPath: string;
}

export class MoveBuildError {
  exec_err: ChildProcess.ExecException;
  // TODO: right now, `move package build` outputs its build errors to stdout instead of stderr.
  // This may not be ideal and we may want to fix it and then revisit the error definition here.
  stdout: string;
  stderr: string;

  constructor(exec_err: ChildProcess.ExecException, stdout: string, stderr: string) {
      this.exec_err = exec_err;
      this.stdout = stdout;
      this.stderr = stderr;
  }
}

export class ChainedError extends Error {
  causes: Error[];

  constructor(message: string, cause?: Error | Error[]) {
      super(message);

      if (cause === undefined) {
          this.causes = [];
      }
      else if (Array.isArray(cause)) {
          this.causes = cause;
      }
      else {
          this.causes = [cause];
      }
  }
}
