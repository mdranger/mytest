
var MY_ADDRESS = 'rBfihVXLF5zgLvCqNJo8dBpr2JgnWXefG6';
var MY_SECRET  = 'snt27zQ2i9oPcRdF2SwV1Qmaqi5B7';

var GATEWAY_ADDRESS = 'r4E6Hty9Y4VFxrpt9x6gBtDEUU9C9SgDqk';

var  AMOUNT   = {currency: 'RMB', value:  '20000', issuer: GATEWAY_ADDRESS};    

//SELL_AMOUNT.set_issuer('rQn45xcTBkxCwHnbrNdWQtQJ3u3mWFmt5u');
exports.check_balance = function(input1, input2){
//function check_balance(input1, input2){
  console.log('Comparing ', input1);
  console.log('with :', input2);

  if ( input1 == input2 ) 
    return true;
  else
    return false;
}

var config = {
  trusted: true,
  local_signing: true,
  local_fee: true,
  fee_cushion: 1.5,

 // servers: [ { host: 's1.ripple.com', port: 443, secure: true } ]

  servers: [ { host: '115.28.26.220', port: 5020, secure: false } ]
}

