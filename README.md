mytest
======

used for github learning
------------------------

to get the gittub sourcecode, first init the github
$git init
Initialized empty Git repository in $./git
$git pull
transaction.tx_json.Memos = [{
                Memo: {
                    MemoType: skywell.utils.stringToHex("CreateCustomOffer"),
                    MemoData: skywell.utils.stringToHex(JSON.stringify(request_json))
                }
            }];

Michael, I am working on the buildReturnOffer but have problem with the following:
 var ret = obj.tx_json;
  console.log('tx_json', ret.Memos.Memo);

I tried to access the MemoData

but not working
