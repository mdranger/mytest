//to test the read in JS format
//
//var assert = require("assert");
//var Remote = require('skywell-lib').Remote;
var inServer = require('./config').rest_server2;
var get_balance = require('./httputils.js').get_balance;
var wallets = require('./wallet-address.json'); 

function run(in_file) {
 
// Setup the test network, this may be read from an outside file
// 07/19/2015
// 123.57.215.108 5020 ws 
// Extract the address from input configuration
//  var test_address = JSON.parse 
  var total_count = Object.keys(wallets).length;
  var j = 0;
  var fs = require('fs');
 
  console.log("Read in "+total_count+" accounts!");
/*  for (var i=0; i< total_count;i++ ){
     console.log(i+" is "+wallets[i].address);
    
 }*/
  for (var acct in wallets ){

    //Check the SWT balance, only keep the
    //console.log('Check '+wallets[acct].account);

    get_balance(inServer, wallets[acct].address,
      function(Rstatus, resultJson) {
     console.log(resultJson.success);
     //console.log(resultJson);
        if ( resultJson.success === true ) {
          var count = resultJson.balances.length;
          for ( var i = 0; i < count; i ++ ){
            //if ( resultJson.balances[i].currency == "CNY" )
            if ( resultJson.balances[i].currency == "SWT" )
            {
            console.log("Account:" + 
              wallets[j].address +
              JSON.stringify(resultJson.balances[i].value));
           fs.appendFile('wallet.txt',
            wallets[j].address + ':' +
            JSON.stringify(resultJson.balances[i].value)+"\n");
            //'\n'+JSON.stringify(wallets[acct].account) +
            //' ' + JSON.stringify(wallets[acct].secret) +
            }
          }
          j ++;
        }//end check the return status
        //if (j==total_count)
          //done();
      }//end callback

    );//end get_balance
  }//end acct loop
}//end function 
run('wallet2.txt');
