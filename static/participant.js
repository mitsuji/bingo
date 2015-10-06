//
// reporter.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    setCaption('');
    setTotal('0');
    setAha('0');


    var boardPublicKey = function() {
	var url = location.href;
	var posQuery = url.indexOf('?');
	if( posQuery >= 0 ) {
	    return url.substr( posQuery +1 );
	} else {
	    return null;
	}
    }();

    
    //
    // boarderPublicKey が指定されていないときは何もしない
    //
    if( boardPublicKey != null ) {

	console.log('gamePublicKey: ' + boardPublicKey );
	console.log('localStorage: pk: ' + localStorage.getItem('pk:' + boardPublicKey));

	connect(boardPublicKey);
	
    }
    

    function connect( boardPublicKey ) {

	//
	// WebSocketクライアントの実装
	//

	var wsurl = '/participant?game_public_key=' + boardPublicKey;
	if( localStorage.getItem('pk:' + boardPublicKey) != null)
	{
	    wsurl += '&participant_key=' + localStorage.getItem('pk:' + boardPublicKey);
	}
	
	var ws = webSocketUtil.webSocket( wsurl );
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
    
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data );
	    var json = JSON.parse(event.data);
	    if(json.type == 'participant') {
		localStorage.setItem('pk:' + json.content.game_public_key, json.content.participant_key)
		setCaption(json.content.game_caption)
		setCard(json.content.card);
		setEval(json.content.eval);
		setState(json.content.state);
	    } else if(json.type == 'draw') {
		setEval(json.content.eval);
		setState(json.content.state);
	    } else if(json.type == 'reset') {
		setCard(json.content.card);
		setEval([]);
		$('#message').text('');
	    }
	};


	//
	// AHAボタン押下
	//
	if ( 'ontouchend' in window ) {
	    $('#aha')[0].addEventListener('touchend',function(e){
		ws.send('aha');
	    },false);
	} else {
	    $('#aha').click(function(e){
		ws.send('aha');
	    });
	}
    }

    
    //
    // 画面表示
    //
    function setConnect( flag ) {
	if( flag ) {
	    $('#connectionStatus .ng').hide();
	    $('#connectionStatus .ok').show();
	} else {
	    $('#connectionStatus .ok').hide();
	    $('#connectionStatus .ng').show();
	}
    }

    function setCaption( caption ) {
	$('#caption').text( caption );
    }
    
    function setTotal( total ) {
	$('#totalCount').text(total);
    }
    
    function setAha( aha ) {
	$('#ahaCount').text(aha);
    }

    function setCard( card ) {
	console.log('setCard: ' + card );
	var card0 = card[0];
	var card1 = card[1];
	
	var i;
	for( i=0; i < card0.length; i++ )
	{
	    $('#matrix .m' + i).text(card0[i]);
	}
	
	for( i=0; i < card1.length; i++ )
	{
	    $('#matrix .m' + (i+13)).text(card1[i]);
	}
    }

    function setEval( ev ) {
	console.log('setEval: ' + ev );
	if( ev.length == 0) {
	    var i;
	    for( i=0; i < 25; i++ )
	    {
		$('#matrix .m' + i).css("color","black");
		$('#matrix .m' + i).css("background-color","white");
	    }
	}else{
	    var i;
	    for( i=0; i < ev.length; i++ )
	    {
		if(ev[i]){
		    $('#matrix .m' + i).css("color","white");
		    $('#matrix .m' + i).css("background-color","red");
		}else{
		    $('#matrix .m' + i).css("color","black");
		    $('#matrix .m' + i).css("background-color","white");
		}
	    }
	}
	

	
    }

    function setState( st ) {
	console.log('setState: ' + st );
	if( st == null ) {
		$('#message').text('');
	}else{
	    if( st.type == 'lizhi' ){
		$('#message').text('リーチ!' + st.content.num);
	    } else if( st.type == 'bingo') {
		$('#message').text('ビンゴ!!');
	    }
	}
	
    }
    
});


