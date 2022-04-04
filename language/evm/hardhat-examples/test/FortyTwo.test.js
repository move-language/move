const { expect } = require("chai");

describe("FortyTwo (the Move contract)", function () {
  before(async function () {
    this.FortyTwo = await ethers.getContractFactory("FortyTwo");
    this.fortyTwo = await this.FortyTwo.deploy();
    await this.fortyTwo.deployed();
  });

  it("forty_two() should return 42", async function () {
    expect(await this.fortyTwo.forty_two()).to.be.equal(42);
  });

  it("forty_two_as_u256() should return 42", async function () {
    expect(await this.fortyTwo.forty_two_as_u256()).to.be.equal(42);
  });

  // TODO: fix string/vector returning
  // it("forty_two_as_string() should return \"forty two\"", async function () {
  //   expect(await this.fortyTwo.forty_two_as_string()).to.be.equal("forty two");
  // });

  it("forty_two_plus_alpha(7) should return 49", async function () {
    expect(await this.fortyTwo.forty_two_plus_alpha(7)).to.be.equal(42 + 7);
  });
});

describe("FortyTwo_Sol (the Solidity contract)", function () {
  before(async function () {
    this.FortyTwo = await ethers.getContractFactory("FortyTwo_Sol");
    this.fortyTwo = await this.FortyTwo.deploy();
    await this.fortyTwo.deployed();
  });

  it("forty_two() should return 42", async function () {
    expect(await this.fortyTwo.forty_two()).to.be.equal(42);
  });

  it("forty_two_as_u256() should return 42", async function () {
    expect(await this.fortyTwo.forty_two_as_u256()).to.be.equal(42);
  });

  it("forty_two_as_string() should return \"forty two\"", async function () {
    expect(await this.fortyTwo.forty_two_as_string()).to.be.equal("forty two");
  });

  it("forty_two_plus_alpha(7) should return 49", async function () {
    expect(await this.fortyTwo.forty_two_plus_alpha(7)).to.be.equal(42 + 7);
  });
});
