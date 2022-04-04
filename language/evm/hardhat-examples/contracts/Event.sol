//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

contract Event_Sol {
    event SimpleEvent(uint64 x);
    event MyEvent(uint64 x, string message);

    function emitNothing(uint64 x) public {
    }

    function emitSimpleEvent(uint64 x) public {
        emit SimpleEvent(x);
    }

    function emitSimpleEventTwice(uint64 x) public {
        emit SimpleEvent(x);
        emit SimpleEvent(x+x);
    }

    function emitMyEvent(uint64 x) public {
        emit MyEvent(x, "hello_event");
    }

    function emitMyEventTwice(uint64 x) public {
        emit MyEvent(x, "hello_event_#1");
        emit MyEvent(x+x, "hello_event_#2");
    }

    function emitMyEventWith(uint64 x, string memory message) public {
        emit MyEvent(x, message);
    }

    function emitMyEventWithTwice(uint64 x, string memory message) public {
        emit MyEvent(x, message);
        emit MyEvent(x+x, message);
    }
}
