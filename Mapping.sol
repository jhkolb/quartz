pragma solidity >=0.5.7;

contract Mapping {
    enum State {
        open
    }
    mapping(uint => mapping(address => address payable)) public Map1;
    mapping(address => mapping(uint => address payable)) public Map2;
    State public __currentState;

    constructor(uint id, address payable id2) {
        __currentState = State.open;
        Map1[id][id2].transfer(uint(0));
        Map2[id2][id].transfer(uint(0));
    }


}
