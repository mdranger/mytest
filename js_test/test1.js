//var assert = require("assert");
//var Remote = require('skywell-lib').Remote;
//var test1 = require('./test_payment').check_balance;
var test_address = require('./sys-config').accounts;
var fs = require('fs');

function run(input1, input2) {
 
// Setup the test network, this may be read from an outside file
// 07/19/2015
// 123.57.215.108 5020 ws 
// Extract the address from input configuration
var cur_payment = {
    source_account: "",
    source_amount: "",
    source_slippage: "",
    destination_account: "",
    destination_amount: "",
    paths: [],
  };//=

  var addr = input2.alice;// = "jDPHkkAqtB6jEihCYfDaFzJixEzBvqmvds";
  var src_wallet = input2.a1.account;
  var des_wallet = input2.a2.account;
  var gateway    = input2.b1.account;
  var amount = 100;
  var currency = 'CNY';

  try {
  var buf =  require('crypto').randomBytes(16);
  console.log('Have %d bytes of random data: %s', buf.length, buf.toString('hex'));
    //buf.toString('hex');
    console.log("Conver to string ", buf.toString('hex'));
  } catch (ex) {
  // handle error
  // most likely, entropy sources are drained
    console.log("Error of gen the ID!");
  }
  var command = '/v1/accounts/'+src_wallet+'/payments/paths/'  
      + des_wallet + '/' + amount + '+' + currency + '+' 
      + gateway;
  cur_payment.source_account = src_wallet;
  cur_payment.destination_amount = input2.a1;//lice;
  //Create the remote 
  console.log('accts ', src_wallet);
  console.log('accts ', cur_payment.source_account);
  console.log('accts ', JSON.stringify(cur_payment.destination_amount));
  
/*  
  console.log('acct 1:', addr);
  console.log('command:', command);
  console.log('Result: ', test1('alice', addr.secret));
  fs.writeFile('wallet.txt', 
        '\n' + input2.a1.account +
        ' ' + input2.a1.secret, function (err) {
          if (err) throw err;
          console.log('It\'s saved!');
        });
  fs.appendFile('wallet.txt',
        '\n' + input2.a2.account +
        ' ' + input2.a2.secret, function (err) {
          if (err) throw err;
          console.log('It\'s saved!');
        }); 
*/
}

run('alice', test_address);
