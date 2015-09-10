/*To test the Payment process in JS format
1. Read in the test wallet addresses
2. Save the current balance in a separate file.
3. Make the payment from the trusted acount
   Yonghu Tong.
4. Check the new balance and see if the 
   result is good.   

*/
var accounts = require('./config.js').accounts;
var inServer = require('./config.js').rest_server_https;
var get_balance = require('./httputils.js').get_balance;
var submit_payment = require('./httputils.js').submit_payment;
var wallets = require('./wallet-address.json'); 

/*
Main function to check the balance of the input 
*/
function run(in_file, in_base, in_currency) {
 

// Setup the test network, this may be read from an outside file
// 07/19/2015
// 123.57.215.108 5020 ws 
// Extract the address from input configuration
//  var test_address = JSON.parse 
  var total_count = Object.keys(wallets).length;
  var j = 0;
  var fs = require('fs');
//make a new object array holding the 
//input address and a certain balance
  //var wallets_with_balance(total_count);

  console.log("Read in "+total_count+" accounts!");
  console.log("Pay from "+in_base.account+":"+in_base.secret);
   console.log(" Server" + inServer.hostname);
  //Create the payment object using necessary info
  var send_amount = {
  "value": 10.0,
  "currency": "SWT",
  "issuer" : "",
  };//=require('./httputils.js').amount;
  //Payment object
  var cur_payment = {
    source_account: "",
    source_amount: "",
    source_slippage: "",
    destination_account: "",
    destination_amount: "",
    paths: [],
  };//=require('./httputils.js').payment;
  var in_data = {
    secret:"",
    client_resource_id:109,
    //payment: cur_payment,
  }; 
 
  //Create a fixed amount of SWT to send
  //for (var i=0; i< total_count;i++ ){
  for (var i=0; i< 1;i++ ){
    console.log(i+" is "+wallets[i].address);
    //create_payment_obj();
    //From base account to destination account
    cur_payment.source_account = in_base.address;
    cur_payment.source_amount = send_amount;
     //Note the source amount need to be higher than send amount
    cur_payment.source_amount.value = send_amount.value + 0.1;
    cur_payment.source_slippage = 0;
    cur_payment.destination_account = wallets[i].address;
    cur_payment.destination_amount = send_amount; 
    cur_payment.paths = [];

    in_data.secret = in_base.secret;
    in_data.client_resource_id = "109";
    in_data.payment = cur_payment;
     
    //Submit the payment for SWT
    submit_payment(inServer, wallets[i].address, 
      in_data,
      function(Rstatus, resultJson){
        //Check to see if the return is success
        if ( resultJson.success === true ) {
          console.log( "result: "+resultJson.result);
          console.log( "state: "+resultJson.state);
          console.log( "fee: "+resultJson.fee);
        }else
        {
          console.log("Error!");
        }
      }//end callback function
    );//end submit_payment
       
  }
   
  j ++;

  //Add a if statement to debug the program
  //Check the balance after the payment.
  //The new balance should be higher than the 
  //input balance
  if ( j < 1 ) {
  for (var acct in wallets ){

    //Check the SWT balance, only keep the
    //console.log('Check '+wallets[acct].account);
    
    get_balance(inServer, wallets[acct].address,
      function(Rstatus, resultJson) {
    //     console.log(resultJson.success);
     //console.log(resultJson);
        if ( resultJson.success === true ) {
          var count = resultJson.balances.length;
          for ( var i = 0; i < count; i ++ ){
            //if ( resultJson.balances[i].currency == "CNY" )
            if ( resultJson.balances[i].currency == in_currency )
            {
            console.log("Account:" + 
              wallets[j].address +
              JSON.stringify(resultJson.balances[i].value));
           //Add the balance in the original wallet object
             wallets[j].currency = in_currency;
             wallets[j].value = resultJson.balances[i].value;
           fs.appendFile(in_file,
             JSON.stringify(wallets[j])+"\n");
            }
          }
          j ++;
        }//end check the return status
        //Only used in the mocha test
        //if (j==total_count)
          //done();
      }//end callback

    );//end get_balance
  }//end acct loop
  }//end j < 1 if

}//end function 
run('wallets_with_balance.JSON', accounts.base, "SWT");
//run('wallet2.txt', accounts.base);
