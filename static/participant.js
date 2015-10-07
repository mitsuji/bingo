//
// participant.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    setCaption('');

    
    var gamePublicKey = function() {
	var url = location.href;
	var posQuery = url.indexOf('?');
	if( posQuery >= 0 ) {
	    return url.substr( posQuery +1 );
	} else {
	    return null;
	}
    }();

    
    //
    // gamePublicKey が指定されていないときは何もしない
    //
    if( gamePublicKey != null ) {

	console.log('gamePublicKey: ' + gamePublicKey );
	console.log('localStorage: pk: ' + localStorage.getItem('pk:' + gamePublicKey));

	connect(gamePublicKey);
	
    }
    

    function connect( gamePublicKey ) {

	//
	// WebSocketクライアントの実装
	//

	var wsurl = '/participant?game_public_key=' + gamePublicKey;
	if( localStorage.getItem('pk:' + gamePublicKey) != null)
	{
	    wsurl += '&participant_key=' + localStorage.getItem('pk:' + gamePublicKey);
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
		setEvalEmpty();
		setStateEmpty();
	    }
	};

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
    
    function setCard( card ) {
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
	var i;
	for( i=0; i < ev.length; i++ )
	{
	    if(ev[i]){
		$('#matrix .m' + i).css("color","white");
		$('#matrix .m' + i).css("background-color","#f035cf");
	    }else{
		$('#matrix .m' + i).css("color","#f035cf");
		$('#matrix .m' + i).css("background-color","white");
	    }
	}
    }

    function setEvalEmpty() {
	var i;
	for( i=0; i < 25; i++ )
	{
	    if(i == 12){
		$('#matrix .m' + i).css("color","white");
		$('#matrix .m' + i).css("background-color","#f035cf");
	    }else{
		$('#matrix .m' + i).css("color","#f035cf");
		$('#matrix .m' + i).css("background-color","white");
	    }
	}
    }

    function setState( st ) {
	if( st.type == 'lizhi' ){
	    if(st.content.num == 1) {
		$('#message').text('リーチ!');
	    } else if(st.content.num == 2) {
		$('#message').text('ダブルリーチ!!');
	    } else if(st.content.num == 3) {
		$('#message').text('トリプルリーチ!!!');
	    } else {
		$('#message').text(st.content.num + '重リーチ!!!!');
	    }
	} else if( st.type == 'bingo') {
	    $('#message').text('ビンゴ!!!!!');
	}
    }

    function setStateEmpty( ) {
	$('#message').text('');
    }


    
});


