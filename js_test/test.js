var assert = require("assert");
//var Remote = require('skywell-lib').Remote;
var test1 = require('./test_payment').check_balance;
var test_address = require('./sys-config').accounts;

describe('Test Accounts', function(){
 
// Setup the test network, this may be read from an outside file
// 07/19/2015
// 123.57.215.108 5020 ws 
// Extract the address from input configuration
  var addr = test_address.alice;// = "jDPHkkAqtB6jEihCYfDaFzJixEzBvqmvds";
  //Create the remote 
//  console.log('acct '+addr.secret);
  describe('Check account', function(){
    it('should return a valid address if the process is successful', function(){
      assert.equal(true,test1('alice', addr.secret));
    })
  })
  describe('Check balance', function(){
    it('should return a valid address if the process is successful', function(){
      assert.equal(2, [1,2,3].indexOf(3));
    })
  })

})
