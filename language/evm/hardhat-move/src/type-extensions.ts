import "hardhat/types/config";

import { MoveBuild } from "./types";

declare module "hardhat/types/config" {
  interface HardhatUserConfig {
    move?: Partial<MoveBuild>;
  }

  interface HardhatConfig {
    move: MoveBuild;
  }
}
