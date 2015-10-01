//
// viewer.html 用 UIコントローラ
//
$(document).ready(function () {

    //
    // 表示リセット
    //
    setConnect(false);
    setCaption('');
    setTotal('0');

    
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

	connect(boardPublicKey);
	
    }

    
    function connect( boardPublicKey ) {

	//
	// WebSocketクライアントの実装
	//
	var ws = webSocketUtil.webSocket('/viewer?public_key=' + boardPublicKey);
	
	ws.onopen = function() {
	    setConnect(true);
	};
	
	ws.onclose = function(event) {
	    setConnect(false);
	};
	
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data);
	    var json = JSON.parse(event.data);
	    if(json.type == 'game') {
		setCaption(json.content.caption);
		setReporterAddress(json.content.public_key);
		setQr(json.content.public_key);
		setDraw('',json.content.selected);
	    } else if(json.type == 'draw') {
		setDraw(json.content.item,json.content.selected);
	    } else if(json.type == 'reset') {
		setTotal('0');
		setDraw('',[]);
	    }
	};
	
	
	//
	// リセットボタン押下
	//
	$('#reset').click(function () {

	    var boardKeys;
	    if(localStorage.getItem('gameKeys') != null){
		boardKeys = JSON.parse(localStorage.getItem('gameKeys'));
	    } else {
		boardKeys = {};
	    }

	    if(boardKeys.hasOwnProperty(boardPublicKey)) {
		$.post(
		    'http://' + location.host + '/reset_game',
		    {secret_key: boardKeys[boardPublicKey]},
		    function (data){
			if(data.success) {
			    // ok
			} else {
			    alert('error: ' + data.type);
			}
		    },
		    'json'
		);
	    }else{
		alert('reset not permitted...')
	    }
	    


	});
	
	//
	// Drawボタン押下
	//
	$('#draw').click(function () {

	    var boardKeys;
	    if(localStorage.getItem('gameKeys') != null){
		boardKeys = JSON.parse(localStorage.getItem('gameKeys'));
	    } else {
		boardKeys = {};
	    }

	    if(boardKeys.hasOwnProperty(boardPublicKey)) {
		$.post(
		    'http://' + location.host + '/draw_game',
		    {secret_key: boardKeys[boardPublicKey]},
		    function (data){
			if(data.success) {
			    // ok
			} else {
			    alert('error: ' + data.type);
			}
		    },
		    'json'
		);
	    }else{
		alert('draw not permitted...')
	    }
	    


	});
	
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
	$('#total').text( total );
    }

    
    function getReporterAddress( publicKey ) {
	return 'http://' + location.host + '/participant.html?' + publicKey;
    }
    
    function setReporterAddress( publicKey ) {
	var reporter_url = getReporterAddress(publicKey);
	$('#reporterAddress').text(reporter_url);
    }

    function setQr( publicKey ) {
	var reporter_url = getReporterAddress(publicKey);
	var qr_url = 'https://chart.googleapis.com/chart';
	qr_url += '?chs=300x300&cht=qr&chl=' + reporter_url;
	$('#qr').attr('src',qr_url);
    }

    function setDraw( item, selected ) {
	$('#item').text(item);
	$('#selected').text(selected);
    }

    
});



/* .: @Samantha :. */

$(window).load(function(){
    //
    // モーダルウィンドウ
    //
    $("#modal-open").click( function(){
    	$( this ).blur() ;	//ボタンからフォーカスを外す
		if( $( "#modal-overlay" )[0] ) return false ;		//新しくモーダルウィンドウを起動しない
		$( "body" ).append( '<div id="modal-overlay"></div>' ) ;
		
		var overlay$ = $("#modal-overlay");
		var qr$ = $("#modal-qr");
		
		overlay$.fadeIn( "slow" );
		centeringModalSyncer() ;
		qr$.fadeIn( "slow" ) ;
		
		
		$("#modal-overlay,#modal-close").off().click(function(){	
			$("#modal-qr,#modal-overlay").fadeOut("slow" , function(){
				overlay$.remove();
			} ) ;
		} ) ;
    	
    });

    return;

    //
    // グラフ描画
    //
    var smoothie = new SmoothieChart({	//グラフの形の指定
      grid: { strokeStyle:'rgb(125, 0, 0)', fillStyle:'rgb(60, 0, 0)',
	      lineWidth: 1, millisPerLine: 250, verticalSections: 6, },
      labels: { fillStyle:'rgb(60, 0, 0)' }
    });
    smoothie.streamTo(document.getElementById("ahagraph"),1000);
    
    var line1 = new TimeSeries();
    setInterval(function() {	// １秒毎に描画
	var total = g_total - g_total_old;
	if(total<0) total=0 ;
	g_total_old =  g_total;
	line1.append(new Date().getTime(),total);
    }, 1000);
    
    
    // Add to SmoothieChart
    smoothie.addTimeSeries(line1,{ strokeStyle:'rgb(255, 0, 255)', fillStyle:'rgba(255, 0, 255, 0.3)', lineWidth:3 });
    
});

//
//　グラフ用に覚えておく変数
//
var g_total=0; /* グローバル変数 */
var g_total_old=0; /* グローバル変数 */


//
//リサイズされた時のモーダルウィンドウの位置
//
var timer = false; /* グローバル変数 */
$(window).resize(function() {
    if(timer !== false){
    	clearTimeout(timer);
    }
    timer = setTimeout(function() {
    	centeringModalSyncer();
   }, 200);
});

//
// モーダルウィンドウのセンタリング関数
//
function centeringModalSyncer() {
  	var qr$ = $("#modal-qr");
	var w = $( window ).width();
	var h = $( window ).height();
	console.log(w+"..,"+h);
	var cw = qr$.outerWidth( true );
	var ch = qr$.outerHeight( true );
	console.log(cw+".,,"+ch);
	qr$.css( {"left": ((w - cw)/2) + "px","top": ((h - ch)/2)-(ch/2) + "px"} ) ;
}
