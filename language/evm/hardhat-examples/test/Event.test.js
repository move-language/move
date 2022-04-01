const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Event", function () {
  before(async function () {
    this.Event = await ethers.getContractFactory("Event");
    this.event = await this.Event.deploy();
    await this.event.deployed();
  });
  it("emitSimpleEvent(42) should return an event", async function () {
    const tx = this.event.emitSimpleEvent(42);
    await expect(tx).to.emit(this.event, 'SimpleEvent').withArgs(42);
  });
  it("emitSimpleEventTwice(42) should return two events", async function () {
      const tx = this.event.emitSimpleEventTwice(42);
      await expect(tx).to.emit(this.event, 'SimpleEvent').withArgs(42);
      await expect(tx).to.emit(this.event, 'SimpleEvent').withArgs(84);
  });
  it("emitMyEvent(42) should return an event", async function () {
    const tx = this.event.emitMyEvent(42);
    await expect(tx).to.emit(this.event, 'MyEvent').withArgs(42, 'hello_event');
  });
  it("emitMyEventTwice(42) should return two events", async function () {
      const tx = this.event.emitMyEventTwice(42);
      await expect(tx).to.emit(this.event, 'MyEvent').withArgs(42, 'hello_event_#1');
      await expect(tx).to.emit(this.event, 'MyEvent').withArgs(84, 'hello_event_#2');
  });
});
