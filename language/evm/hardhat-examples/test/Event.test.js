const { expect } = require("chai");
const { ethers } = require("hardhat");

const make_test = function(contract_name) {
  return function() {
    before(async function () {
      this.Event = await ethers.getContractFactory(contract_name);
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
    it("emitMyEventWith(42, 'hello_event') should return an event", async function () {
      const tx = this.event.emitMyEventWith(42, "hello_event");
      await expect(tx).to.emit(this.event, 'MyEvent').withArgs(42, 'hello_event');
    });
    it("emitMyEventWithTwice(42, 'hello_event') should return two events", async function () {
        const tx = this.event.emitMyEventWithTwice(42, "hello_event");
        await expect(tx).to.emit(this.event, 'MyEvent').withArgs(42, 'hello_event');
        await expect(tx).to.emit(this.event, 'MyEvent').withArgs(84, 'hello_event');
    });
  }
};

describe("Event (the Move contract)", make_test('Event'));
describe("Event_Sol (the Solidity contract)", make_test('Event_Sol'));
