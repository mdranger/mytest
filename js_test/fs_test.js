//to test the read in JS format
//
//var assert = require("assert");
//var Remote = require('skywell-lib').Remote;
var test1 = require('./test_payment').check_balance;
var test_address = require('./sys-config').accounts;
var wallets = require('./wallet-address.json'); 

function run(in_file) {
 
// Setup the test network, this may be read from an outside file
// 07/19/2015
// 123.57.215.108 5020 ws 
// Extract the address from input configuration
//  var test_address = JSON.parse 
  var total_count = Object.keys(wallets).length; 
  console.log("Read in "+total_count+" accounts!");
  for (var i=0; i< total_count;i++ ){
     console.log(i+" is "+wallets[i].address);
 } 

/*
  var fs = require('fs');
  fs.readFile(in_file, function (err, data) {
    if (err) throw err; 
    //Convert the input content into string 
    //then 
    //break into JSON msg
    var acct_lines=String(data).split("\n");
    console.log('Size '+acct_lines.length);
    var acct_array=[];//init the array 
    var in_acct;

    var j = 0;

    for (var i=0; i< acct_lines.length-1;i++ ){ 
      in_acct = JSON.parse(acct_lines[i], function(name, value){
      //if ( typeof name === "string" ) 
        console.log("name:"+name);
        return value;
    });
//      in_acct = JSON.parse(acct_lines[i]);  
      console.log(typeof in_acct); 
      if ( typeof in_acct != "undefined" )
      {
        console.log(in_acct.wallet.address);
        acct_array[j] = in_acct;
        j ++;
      }
    }//end of parsing all the lines
    console.log("Total "+j+" accounts!" );

   // if (     console.log(in_acct.success);
  })  */
  
  /*fs.writeFile('wallet.txt', 
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
        }); */

}

run('wallet2.txt');
