const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Revert", function () {
  before(async function () {
    this.Revert = await ethers.getContractFactory("Revert");
    this.revert = await this.Revert.deploy();
    await this.revert.deployed();
  });
  it("revertIf0(0) should revert", async function () {
    const tx = this.revert.revertIf0(0);
    await expect(tx).to.be.reverted;
  });
  it("revertWithMessage() should revert with a message", async function () {
    const tx = this.revert.revertWithMessage();
    await expect(tx).to.be.revertedWith('error message');
  });
});
