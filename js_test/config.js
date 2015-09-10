/*config.js
  This file contains the information to setup the auto tests.
   
*/
//The REST server for testing
exports.rest_server_https = {
  hostname: 'tapi.jingtum.com',
  port: 443,
  path: '',
  method: '',
};

exports.rest_server_http= {
  hostname: '101.200.176.249',
  port: 80,
  path: '',
  method: '',
};

//Test accounts
exports.accounts = {
  // Users
  //Base
  "base"  : {
    'account' : "jHb9CJAWyB4jr91VRWn96DkukG4bwdtyTh",
    'secret':"snoPBjXtMeMyMHUVTgbuqAfg1SUTb",
  },

  "gateway" : {
    'account' : "jBciDE8Q3uJjf111VeiUNM775AMKHEbBLS",
    'secret': "missing",
  },
  "alice" : {
    'account' : "jG1QQv2nh2gj7RCZ1P8YYcBUKCCN633rCn",
    'secret' : "alice",
  },
  "a1": {
    'account' : "jsqRs9BDCjyTuRWEPZk3yHa4MFmRi9D834",
    'secret' : "a1",
  },
  "a2": {
    'account' : "jHjRE34XZa2CE3cf4tEdLCk1kTGGk4F7pk",
    'secret' : "a2",
  },

   "p1":{
    'account': "jQNdYXxgNHY49oxDL8mrr7J6k7tdNy1kM",
    'secret' : "unknown",
  },
//Gateway issuer
  "CNYissuer1":{
    'account': "jBciDE8Q3uJjf111VeiUNM775AMKHEbBLS",
    'secret' : "unknown",
  },

// Used for TX fees
  "fee": {
    'account' : "jQNdYXxgNHY49oxDL8mrjr7J6k7tdNy1kM",
    'secret' : "snxVJXMkURjrscL7gfwfWcywYzPkL",
  },
  "oldfee": {
    'account' : "jGmrQ6aiiHBSG1F7ESP4Qw9g4uMY2BfNnX",
    'secret' : "unknown",
  },

// Manager Account of ts10.jingtum.comi(123.57.215.108)
  "management": {
    'account' : "j9Bn1W8RhpoNjMYFd8TZyV7gmkQt5pmkzx",
    'secret' : "snHq4b4KHGrh4S2T73mAXVEtii9jr",
  },

// Wrong address for testing, should return false
  "w1": {
    'account' : "Rs9BDCjyTuRWEPZk3yHa4MFmRi9D834",
    'secret' : "w1",
  },

// 
  "counterparty": {
    'account' : "jBciDE8Q3uJjf111VeiUNM775AMKHEbBLS",
    'secret' : "unknown",
	}

};

//Settings of the REST server ws
exports.apiservers = {
 "api1": {
  'address' : "123.57.215.108",
  'port'    : 5020 ,
  },
};

//ts10.jingtum.comi(123.57.215.108:5050) rpc port
exports.api_server_ts10 = {
  hostname: '123.57.215.108',
  port: 5050,
  path: '',
  method: ''
};

