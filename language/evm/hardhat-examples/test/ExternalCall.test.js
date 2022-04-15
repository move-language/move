const { BN, constants, expectEvent, expectRevert } = require('@openzeppelin/test-helpers');
const { ZERO_ADDRESS } = constants;

const { expect } = require('chai');

const ExternalCall = artifacts.require('ExternalCall');
const FortyTwo = artifacts.require('FortyTwo');
const Revert = artifacts.require('Revert');
const Receiver = artifacts.require('ERC721ReceiverMock');

const Error = [ 'None', 'RevertWithMessage', 'RevertWithoutMessage', 'Panic' ]
  .reduce((acc, entry, idx) => Object.assign({ [entry]: idx }, acc), {});
const RECEIVER_MAGIC_VALUE = '0x150b7a02';

contract('ExternalCall', function (accounts) {
    const [operator, tokenHolder, tokenBatchHolder, ...otherAccounts] = accounts;

    beforeEach(async function () {
        this.externalCall = await ExternalCall.new();
        this.fortyTwo = await FortyTwo.new();
        this.revert = await Revert.new();
    });

    describe('call_forty_two', function () {
        it('returns 42', async function () {
            expect(await this.externalCall.call_forty_two(this.fortyTwo.address)).to.be.bignumber.equal('42');
        });
    });

    describe('call_revertWithMessage', function () {
        it('reverts with error message', async function () {
            await expectRevert(
                this.externalCall.call_revertWithMessage(this.revert.address),
                'error message',
            );
        });
    });

    describe('try_call_forty_two', function () {
        it('returns 42', async function () {
            expect(await this.externalCall.try_call_forty_two(this.fortyTwo.address)).to.be.bignumber.equal('42');
        });

        it('revert with not implemented', async function () {
             await expectRevert(
                 this.externalCall.try_call_forty_two(this.revert.address),
                 'not implemented',
             );
        });
    });

    describe('try_call_revertWithMessage', function () {
        it('callee reverts with error message', async function () {
            await expectRevert(
                this.externalCall.try_call_revertWithMessage(this.revert.address),
                'error reason',
            );
        });

        it('callee did not implement the function', async function () {
            await expectRevert(
                this.externalCall.try_call_revertWithMessage(this.fortyTwo.address),
                'error data',
            );
        });
    });

    describe('Receiver', function () {
        it('receiver test', async function () {
            const receiver = await Receiver.new(RECEIVER_MAGIC_VALUE, Error.None);
            // TODO: uncomment the following line to see the error "Transaction ran out of gas".
            //       For more details, https://github.com/move-language/move/issues/31
            const receipt = await this.externalCall.doSafeTransferAcceptanceCheck(this.externalCall.address, receiver.address, 5042, '0x42');
        });
    });
});
