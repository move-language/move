const { BN, constants, expectEvent, expectRevert } = require('@openzeppelin/test-helpers');
const { ZERO_ADDRESS } = constants;

const { expect } = require('chai');

const ExternalCall = artifacts.require('ExternalCall');
const FortyTwo = artifacts.require('FortyTwo');
const Revert = artifacts.require('Revert');

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
        it('revert with error message', async function () {
            await expectRevert(
                this.externalCall.try_call_revertWithMessage(this.revert.address),
                'error reason',
            );
        });

        // TODO: Fix this.
        // it('revert with not implemented', async function () {
        //     await expectRevert(
        //         this.externalCall.try_call_revertWithMessage(ZERO_ADDRESS),
        //         'non-contract',
        //     );
        // });
    });
});
