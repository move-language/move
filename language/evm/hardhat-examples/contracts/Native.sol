//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

contract Native_Sol {
    function getContractAddr() public view returns (address) {
        return address(this);
    }

    function getSenderAddr() public view returns (address) {
        return msg.sender;
    }
}
