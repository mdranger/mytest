/*--------------------------------------------------
  This is the module used for testing
the Jingtum-rest API.
It contains asynchronout HTTP GET requests for different
REST-API Routes.

1. ACCOUNTS
1.1 Generate Wallet 
    GET /v1/wallet/new
1.2 Get_balance
    GET /v1/accounts/{:address}/balances

2. Payments
2.1 Check_payment_path 
    GET /v1/accounts/{:source_address}/payments/paths/
      {:destination_address}/{:amount}
2.2 Submit Payment 
    POST /v1/accounts/{:source_address}/payments
2.3 Confirm Payment
    GET /v1/accounts/{:address}/payments/{:id}
2.4 Get Payment History
    GET /v1/accounts/{:address}/payments

3. ORDERS
3.1 Place Order
    POST /v1/accounts/{:address}/orders?validated=true
3.2 Cancel Order
    DELETE /v1/accounts/{:address}/orders/{:order}?validated=true
3.3 Get Account Orders
    
3.4 Get Order Transaction
3.5 Get Order Book

4. TRUSTLINES
4.1 Get Trustlines
4.2 Grant Trustline
---------------------------------------------------*/
//http_request = require("https");
http_request = require("http");

/*
1.1 Generate new wallet
/v1/wallet/new
status
*/
exports.gen_wallet = function(options, callback){
    var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/wallet/new';
  cmd.method = 'GET';

  // Make a simple GET request
  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = "";
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();

};

/* 
1.2 Get_balance
/v1/accounts/{:address}/balances，GET
Inputs
options - REST API server site and port number
in_acct - the account number to be checked
Note:
 
Test cases should include invalid account number
*/
exports.get_balance = function(options, in_acct, callback){
  // Parse the URL and get the pieces we need from it
  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/balances';
  cmd.method = 'GET';

  // Make a simple GET request
  // A function to handle the response when it starts to arrive
  //request.on(cmd, function(response) {
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = "";
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body); 
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

///////////////////////////////////////////////////
//2 PAYMENTS
/* 
2.1 Check payment path
/v1/accounts/{:source_address}/payments/paths/{:destination_address}/{:amount}，GET
Get quotes for possible ways to make a particular payment.
Input:
  options - REST API server site and port number
 src_acct - The one make the payment
 des_acct - The one receive the payment
 amount   - Amount
 currency - Currency type
 issuer   - Gateway issue the Currency.

Output:
  callback returns a JSON message, format is
  success  - boolean 
  payments - Array 
Note:
  Not for SWT since it doesn't need a path to pay.
*/
exports.check_payment_path = function(options, 
  src_acct, des_acct, 
  amount, currency, issuer, 
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };
 
  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + src_acct + '/payments/paths/'
    + des_acct + '/' + amount + '+' + currency + '+'
    + issuer;
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

/*
2.2 Submit Payment
Descriptions:

Input:
  options    - REST API server site and port number
  in_payment - A payment object, can be created through
               check_payment_path call.

Output:
  callback returns a JSON message, format is
  success  - boolean
  payments - Array
*/
exports.submit_payment = function(options,
  dest_acct, in_payment, callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + des_acct + '/payments/'
    + 'payments?validated=true';
  cmd.method = 'POST';

  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

/*
2.3 Confirm Payment
Input:
  options    - REST API server site and port number
  in_acct    - The account address who make the payment
  payment_id - A unique identifier for the transaction: either a client 
               resource ID or a transaction hash.

Output:
  callback returns a JSON message, format is
  success  - boolean 
  payments - Array 

*/
exports.confirm_payment = function(options, 
  in_acct, payment_id,
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };
 
  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/payments/'
    + payment_id;
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
     
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

/*
Get Payment History
Input:
  options    - REST API server site and port number
  in_acct    - The account address whose orders to look up

Output:
  callback returns a JSON message, format is
  success  - boolean 
  payments - Array 

*/

exports.get_payment_history = function(options, 
  in_acct, callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };
 
  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/payments';
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
  // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

  // When response is complete, call the callback
    response.on("end", function() {
  //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};


/*
Get Account Orders

Retrieves all open currency-exchange orders associated with the
Jingtum address.

GET /v1/accounts/{:address}/orders

Input:
  options    - REST API server site and port number
  in_acct    - The account address whose orders to look up

Output:
  callback returns a JSON message, format is
  success  - boolean
  orders   - Order Array

*/

exports.get_account_orders = function(options,
  in_acct, callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/orders';
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = require("https").request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};


/*
Get Order Transaction

Get the details of an order transaction. An order transaction 
either places an order or cancels an order.

GET /v1/accounts/{:address}/orders/{:hash}

Input:
  options    - REST API server site and port number
  in_acct    - The account address whose orders to look up
  in_hash    - The transaction hash for the order 

Output:
  callback returns a JSON message, format is
  success  - boolean
  orders   - Order Array

*/

exports.get_order_transaction= function(options,
  in_acct, in_hash,
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/orders/'
    + in_hash;
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

/*
3.5 Order Book
Retrieves the top of the order book for a currency pair.
GET /v1/accounts/{:address}/order_book/{:base}/{:counter}

Input:
  options    - REST API server site and port number
  in_acct    - The account address whose orders to look up
  in_base    - The base currency as currency+counterparty (e.g., USD+)
  in_counter - The counter currency as currency+counterparty (e.g., BTC+) 
  in_limit   - (Defaults to 200) Max results per response. 
               Cannot be less than 10. 
               Cannot be greater than 400.

Output:
  callback returns a JSON message, format is
  success  - boolean
  orders   - Order Array

Note:
There is a problem about the in_limit
to see if it has the limit as a parameter 

*/


exports.get_order_book = function(options,
  in_acct, in_base, in_counter, in_limit,
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/order_book/'
    + in_base + '/' + in_counter;
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};


/*
4.1 Get Trustlines
Retrieves all trustlines associated with the Ripple address.
GET /v1/accounts/{:address}/trustlines

Input:
  options    - REST API server site and port number
  in_acct    - The account address whose trustline to look up
 in_currency - Filter results to include only trustlines for 
               the given currency.
 in_counterparty - Filter results to include only trustlines 
                   to the given account.
 
Output:
  callback returns a JSON message, format is
  success  - boolean
  orders   - Lines Array

*/


exports.get_trustlines = function(options,
  in_acct, in_currency, in_counterparty,
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  //Build up the commond to send out
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  cmd.path = '/v1/accounts/' + in_acct + '/trustlines/'
    + in_currency + '/' + in_counterparty + '/';
  cmd.method = 'GET';


  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd, function(response) {
    // Save the response body as it arrives
    var body = ""
    response.on("data", function(chunk) { body += chunk; });

    // When response is complete, call the callback
    response.on("end", function() {
    //Break the return message into a JSON message
      var resultJson = JSON.parse(body);
      //console.log("Receive: ", body);
      if (callback) callback(response.statusCode, resultJson);
    });
  });
  request.end();
};

/*
4.2 Grant Trustline
Creates or modifies a trustline.
/v1/accounts/{:address}/trustlines?validated=true
cmd:
 
*/
exports.grant_trustlines = function(options,
  des_acct, in_currency, in_counterparty,
  callback){


  var cmd = {
    hostname : '',
    port : 0,
    path : '',
    method : ''
  };

  //Build up the link to send
  cmd.hostname = options.hostname; //: 'tapi.jingtum.com',
  cmd.port = options.port; // 443,
  //Create the link
  cmd.path = '/v1/accounts/' + des_acct 
    + '/trustlines?validated=true';
  cmd.method = 'POST';

  // A function to handle the response when it starts to arrive
  var  request = http_request.request(cmd);

  request.write(data);
  request.end();

  // Save the response body as it arrives
  request.on("response", function(response) {
  //Create the command object
    var body = ""
    response.on("data", function(chunk) { body += chunk; });
  });

};

