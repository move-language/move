const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Native", function () {
  before(async function () {
    this.Native = await ethers.getContractFactory("Native");
    this.native = await this.Native.deploy();
    await this.native.deployed();
  });
  it("getContractAddr() should return the contract address", async function () {
    const tx = this.native.getContractAddr();
    expect(await tx).to.equal(this.native.address);
  });
  it("getSenderAddr() should return the sender address", async function () {
    const tx = this.native.getSenderAddr();
    expect(await tx).to.equal(this.native.signer.address);
  });
});
